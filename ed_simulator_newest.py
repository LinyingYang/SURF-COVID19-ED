import pandas as pd
import numpy as np
import datetime

#Column Name
COL_DAY = 'Day'
COL_DATE = 'Date'

#Parameters
MAX_SIMULATIOn_total_days = 100
# MAX_REMAINING_LOS = 20
STARTING_DAY = datetime.date(year = 2020, month = 4, day = 1)

class PUI_High_Simulator():
    def __init__(self, n_total_days = 10,
                    doubling_time = 25,
                    admission_fraction = [0.2,0.8],
                    los_pui_high=2,
                    pui_high_arrival_hour_distribution=[1/24]*24,
                    dt_change_days = [], # e.g. [1, 10, 15]
                    dt_change_dts = [] # e.g. [9, 12, 14]
                    ):

        # number of total days in the simulation, all starting from STARTING_DAY
        self.n_historical_days = 1 #the number of historical data
        self.n_total_days = n_total_days + self.n_historical_days #the number of days need to be present in total.
        # Arrival related
        self.pui_high_arrival_hour_distribution=pui_high_arrival_hour_distribution
        self.pui_high_arrival_hour=None #2-dimension array
        self.pui_high_arrival_day=None#list
        self.pui_high_cumulative_day=None#list
        # Census related
        self.pui_high_census = None #dataframe
        # Admission related
        self.pui_high_admission = None #2-dimension array


        self.doubling_time_init = doubling_time
        self.dt_change_days = dt_change_days
        self.dt_change_dts = dt_change_dts
        self.doubling_time_seq = np.ones(self.n_total_days) * self.doubling_time_init


        # parameters for admission paths. Should be of dimension row=type, col=unit (icu and floor)
        self.admission_fraction = admission_fraction
        # parameters for los
        self.los = los_pui_high

    def state_init(self, pui_high_census_0=0, df_input_pui_high_arrival = None,pui_high_arrival_day_mean=10,pui_high_cumulative_0=0):
        '''
        initialze the states using input census
        major inputs:
            - df_input_pui_arrival: Dataframe for the arrival records of pui. The row indicates date, while column indicates hour.
        '''
      
        #Always assume we have hourly data. If users only have the daily total arrivals, tell them to breakdown.
        
        if df_input_pui_high_arrival is not None:

            # only select the non empty rows
            df_input_pui_high_arrival[COL_DAY]=np.arange(len(df_input_pui_high_arrival))
            df_input_pui_high_arrival = df_input_pui_high_arrival.loc[~df_input_pui_high_arrival['1'].isna()]
            
            self.n_historical_days = len(df_input_pui_high_arrival)
            self.n_total_days += (self.n_historical_days-1) # current days + projected days

            #initialize
            self.pui_high_arrival_hour = np.zeros(self.n_total_days*24).reshape([self.n_total_days,24])
            self.pui_high_census = np.zeros(self.n_total_days*24).reshape([self.n_total_days,24])
            self.pui_high_arrival_day=np.array([pui_high_arrival_day_mean]*self.n_total_days)
            self.pui_high_cumulative_day=np.array([0]*self.n_total_days)
            for _, row in df_input_pui_high_arrival.iterrows():
                day=row[COL_DAY]
                self.pui_high_arrival_hour[day]=np.array(row)[1:25]
                #The total arrival number in a day
                self.pui_high_arrival_day[day]=row['Total']
                #The cumulative pui high
                if day==0:
                    self.pui_high_cumulative_day[day]=pui_high_cumulative_0+self.pui_high_arrival_day[day]
                else:
                    self.pui_high_cumulative_day[day]=self.pui_high_cumulative_day[day-1]+self.pui_high_arrival_day[day]
            #initialize the census numbers for starting day:
            self.pui_high_census[0,0]=pui_high_census_0+self.pui_high_arrival_hour[0,0]
            for i in range(1,24):
                if i<self.los:
                    self.pui_high_census[0,i]=self.pui_high_census[0,i-1]+self.pui_high_arrival_hour[0,i]
                if i>=self.los:
                    self.pui_high_census[0,i]=sum(self.pui_high_arrival_hour[0,i-self.los+1:i+1])
            for day in range(1,self.n_historical_days):
                arrival_2day=np.concatenate((self.pui_high_arrival_hour[day-1],self.pui_high_arrival_hour[day]))
                for hour in range(0,24):
                    self.pui_high_census[day,hour]=sum(arrival_2day[hour+24-self.los+1:hour+24+1])
            self.doubling_time_seq = np.ones(self.n_total_days) * self.doubling_time_init

        else:
            self.pui_high_arrival_hour = np.zeros(self.n_total_days*24).reshape([self.n_total_days,24])
            self.pui_high_census = np.zeros(self.n_total_days*24).reshape([self.n_total_days,24])
            self.pui_high_arrival_day=np.array([pui_high_arrival_day_mean]*self.n_total_days)
            self.pui_high_cumulative_day=np.array([0]*self.n_total_days)
            self.pui_high_cumulative_day[0]=pui_high_cumulative_0*2**(1/self.doubling_time_seq[0])
            self.pui_high_arrival_day[0]=self.pui_high_cumulative_day[0]-pui_high_cumulative_0
            self.pui_high_arrival_hour[0]=np.multiply(self.pui_high_arrival_day[0],self.pui_high_arrival_hour_distribution)
            #initialize the census numbers for starting day:
            self.pui_high_census[0,0]=pui_high_census_0+self.pui_high_arrival_hour[0,0]
            for i in range(1,24):
                if i<self.los:
                    self.pui_high_census[0,i]=self.pui_high_census[0,i-1]+self.pui_high_arrival_hour[0,i]
                if i>=self.los:
                    self.pui_high_census[0,i]=sum(self.pui_high_arrival_hour[0,i-self.los+1:i+1])
            
        #################################
        #   init the doubling time seq  #
        #################################
        if self.dt_change_dts: # if there is doubling time change
            assert len(self.dt_change_days) == len(self.dt_change_dts), "DT change days and valuse should have equal length!!"
            today_ind = int((datetime.date.today() - STARTING_DAY) / datetime.timedelta(days = 1))
            dt_change_days = self.dt_change_days.copy()
            for i in range(len(dt_change_days)):
                dt_change_days[i] += today_ind
            dt_change_days += [self.n_total_days]
            for i, start, end in zip(range(len(self.dt_change_dts)), dt_change_days[:-1], dt_change_days[1:]):
                self.doubling_time_seq[start:end] = self.dt_change_dts[i]
        self.pui_high_admission=np.zeros(self.n_total_days*2).reshape([self.n_total_days,2])
        

    def run(self):
        '''
        run the simulation of self.n_total_days length
        '''

        for day in range(self.n_historical_days, self.n_total_days): # start from the next input day
            assert(day>0)
            # generate new arrival
            self.generate_new_arrival(day)
            self.update_census(day)
            self.update_admission(day)


    def generate_new_arrival(self, day):
        assert(day>=1)
        #get the doubling time for today
        doubling_time=self.doubling_time_seq[day]
        
        #update the cumulative puis
        self.pui_high_cumulative_day[day]=self.pui_high_cumulative_day[day-1]*2**(1/doubling_time)
        #get new arrival
        self.pui_high_arrival_day[day]=self.pui_high_cumulative_day[day]-self.pui_high_cumulative_day[day-1]
        self.pui_high_arrival_hour[day]=np.multiply(self.pui_high_arrival_day[day],self.pui_high_arrival_hour_distribution)

    def update_census(self, day):
        arrival_2day=np.concatenate((self.pui_high_arrival_hour[day-1],self.pui_high_arrival_hour[day]))
        for hour in range(0,24):
            self.pui_high_census[day,hour]=sum(arrival_2day[hour+24-self.los+1:hour+24+1])
    def update_admission(self,day):
        self.pui_high_admission[day]=np.multiply(self.pui_high_arrival_day[day],self.admission_fraction)

    def return_data(self):

        return [self.pui_high_arrival_day, self.pui_high_arrival_hour,
                self.pui_high_census, self.pui_high_admission]

class PUI_Low_Simulator():
    def __init__(self, n_total_days = 10,
                    starting_total = 10,
                    doubling_time = 25,
                    admission_fraction = [0.2,0.8],
                    los_pui_low=2,
                    pui_low_arrival_hour_distribution=[1/24]*24,
                    dt_change_days = [], # e.g. [1, 10, 15]
                    dt_change_dts = [] # e.g. [9, 12, 14]
                    ):

        # number of total days in the simulation, all starting from STARTING_DAY
        self.n_historical_days = 1 #the number of historical data
        self.n_total_days = n_total_days + self.n_historical_days #the number of days need to be present in total.
        # Arrival related
        self.pui_low_arrival_hour_distribution=pui_low_arrival_hour_distribution
        self.pui_low_arrival_hour=None #2-dimension array
        self.pui_low_arrival_day=None#list
        self.pui_low_cumulative_day=None#list
        # Census related
        self.pui_low_census = None #dataframe
        # Admission related
        self.pui_low_admission = None #2-dimension array


        # self.starting_total = starting_total
        self.doubling_time_init = doubling_time
        self.dt_change_days = dt_change_days
        self.dt_change_dts = dt_change_dts
        self.doubling_time_seq = np.ones(self.n_total_days) * self.doubling_time_init
        # self.daily_covid_admissions = None # number of daily new covid admissions

        # parameters for admission paths. Should be of dimension row=type, col=unit(2)
        self.admission_fraction = admission_fraction
        # parameters for los
        self.los = los_pui_low

    def state_init(self, pui_low_census_0=0, df_input_pui_low_arrival = None,pui_low_arrival_day_mean=1,pui_low_cumulative_0=0):
        '''
        initialze the states using input census
        major inputs:
            - df_input_pui_arrival: Dataframe for the arrival records of pui. The row indicates date, while column indicates hour.
        '''
        #Always assume we have hourly data. If users only have the daily total arrivals, tell them to breakdown.
        self.pui_low_arrival_day_mean=float(pui_low_arrival_day_mean)
        if df_input_pui_low_arrival is not None:

            # only select the non empty rows
            df_input_pui_low_arrival[COL_DAY]=np.arange(len(df_input_pui_low_arrival))
            df_input_pui_low_arrival = df_input_pui_low_arrival.loc[~df_input_pui_low_arrival['1'].isna()]
            
            self.n_historical_days = len(df_input_pui_low_arrival)
            self.n_total_days += (self.n_historical_days-1) # current days + projected days

            #initialize
            self.pui_low_arrival_hour = np.zeros(self.n_total_days*24).reshape([self.n_total_days,24])
            self.pui_low_census = np.zeros(self.n_total_days*24).reshape([self.n_total_days,24])
            self.pui_low_arrival_day=np.array([pui_low_arrival_day_mean]*self.n_total_days)
            self.pui_low_cumulative_day=np.array([0]*self.n_total_days)
            for _, row in df_input_pui_low_arrival.iterrows():
                day=row[COL_DAY]
                self.pui_low_arrival_hour[day]=np.array(row)[1:25]
                #The total number
                self.pui_low_arrival_day[day]=row['Total']
                if day==0:
                    self.pui_low_cumulative_day[day]=pui_low_cumulative_0+self.pui_low_arrival_day[day]
                else:
                    self.pui_low_cumulative_day[day]=self.pui_low_cumulative_day[day-1]+self.pui_low_arrival_day[day]
            #initialize the census numbers for starting day:
            self.pui_low_census[0,0]=pui_low_census_0+self.pui_low_arrival_hour[0,0]
            for i in range(1,24):
                if i<self.los:
                    self.pui_low_census[0,i]=self.pui_low_census[0,i-1]+self.pui_low_arrival_hour[0,i]
                if i>=self.los:
                    self.pui_low_census[0,i]=sum(self.pui_low_arrival_hour[0,i-self.los+1:i+1])
            for day in range(1,self.n_historical_days):
                arrival_2day=np.concatenate((self.pui_low_arrival_hour[day-1],self.pui_low_arrival_hour[day]))
                for hour in range(0,24):
                    self.pui_low_census[day,hour]=sum(arrival_2day[hour+24-self.los+1:hour+24+1])
            self.doubling_time_seq = np.ones(self.n_total_days) * self.doubling_time_init

        else:
            self.pui_low_arrival_hour = np.zeros(self.n_total_days*24).reshape([self.n_total_days,24])
            self.pui_low_census = np.zeros(self.n_total_days*24).reshape([self.n_total_days,24])
            self.pui_low_arrival_day=np.array([pui_low_arrival_day_mean]*self.n_total_days)
            self.pui_low_cumulative_day=np.array([0]*self.n_total_days)
            self.pui_low_cumulative_day[0]=pui_low_cumulative_0*2**(1/self.doubling_time_seq[0])
            self.pui_low_arrival_day[0]=self.pui_low_cumulative_day[0]-pui_low_cumulative_0
            self.pui_low_arrival_hour[0]=np.multiply(self.pui_low_arrival_day[0],self.pui_low_arrival_hour_distribution)
            #initialize the census numbers for starting day:
            self.pui_low_census[0,0]=pui_low_census_0+self.pui_low_arrival_hour[0,0]
            for i in range(1,24):
                if i<self.los:
                    self.pui_low_census[0,i]=self.pui_low_census[0,i-1]+self.pui_low_arrival_hour[0,i]
                if i>=self.los:
                    self.pui_low_census[0,i]=sum(self.pui_low_arrival_hour[0,i-self.los+1:i+1])
        
        #################################
        #   init the doubling time seq  #
        #################################
        if self.dt_change_dts: # if there is doubling time change
            assert len(self.dt_change_days) == len(self.dt_change_dts), "DT change days and valuse should have equal length!!"
            today_ind = int((datetime.date.today() - STARTING_DAY) / datetime.timedelta(days = 1))
            dt_change_days = self.dt_change_days.copy()
            for i in range(len(dt_change_days)):
                dt_change_days[i] += today_ind
            dt_change_days += [self.n_total_days]
            for i, start, end in zip(range(len(self.dt_change_dts)), dt_change_days[:-1], dt_change_days[1:]):
                self.doubling_time_seq[start:end] = self.dt_change_dts[i]
        self.pui_low_admission=np.zeros(self.n_total_days*2).reshape([self.n_total_days,2])
        

    def run(self):
        '''
        run the simulation of self.n_total_days length
        '''

        for day in range(self.n_historical_days, self.n_total_days): # start from the next input day
            assert(day>0)
            # generate new arrival
            self.generate_new_arrival(day)
            self.update_census(day)
            self.update_admission(day)


    def generate_new_arrival(self, day):
        assert(day>=1)
        #get the doubling time for today
        doubling_time=self.doubling_time_seq[day]
        
        #update the cumulative puis
        self.pui_low_cumulative_day[day]=self.pui_low_cumulative_day[day-1]*2**(1/doubling_time)
        #get new arrival
        self.pui_low_arrival_day[day]=self.pui_low_cumulative_day[day]-self.pui_low_cumulative_day[day-1]

        self.pui_low_arrival_day[day]=self.pui_low_arrival_day[day-1]*2**(1/doubling_time)
        self.pui_low_arrival_hour[day]=np.multiply(self.pui_low_arrival_day[day],self.pui_low_arrival_hour_distribution)

    def update_census(self, day):
        arrival_2day=np.concatenate((self.pui_low_arrival_hour[day-1],self.pui_low_arrival_hour[day]))
        for hour in range(0,24):
            self.pui_low_census[day,hour]=sum(arrival_2day[hour+24-self.los+1:hour+24+1])
    def update_admission(self,day):
        self.pui_low_admission[day]=np.multiply(self.pui_low_arrival_day[day],self.admission_fraction)

    def return_data(self):

        return [self.pui_low_arrival_day,self.pui_low_arrival_hour,
                self.pui_low_census,self.pui_low_admission]

class Non_PUI_High_Simulator():
    def __init__(self, n_total_days = 10,
                    admission_fraction = [0.2,0.8],
                    los_non_pui_high=4,
                    non_pui_high_arrival_hour_distribution=[1/24]*24,
                    ):

        # number of total days in the simulation, all starting from STARTING_DAY
        self.n_historical_days = 1 #the number of historical data
        self.n_total_days = n_total_days + self.n_historical_days #the number of days need to be present in total.
        # Arrival related
        self.non_pui_high_arrival_hour_distribution=non_pui_high_arrival_hour_distribution
        self.non_pui_high_arrival_hour=None #2-dimension array
        self.non_pui_high_arrival_day=None#list
        # Census related
        self.non_pui_high_census = None #dataframe
        # Admission related
        self.non_pui_high_admission = None #2-dimension array

        # parameters for admission paths. Should be of dimension row=type, col=unit(2)
        self.admission_fraction = admission_fraction
        # parameters for los
        self.los = los_non_pui_high

    def state_init(self, non_pui_high_census_0=0, df_input_non_pui_high_arrival = None,non_pui_high_arrival_day_mean=10):
        '''
        initialze the states using input census
        major inputs:
            - df_input_pui_arrival: Dataframe for the arrival records of pui. The row indicates date, while column indicates hour.
        '''
        #Always assume we have hourly data. If users only have the daily total arrivals, tell them to breakdown.
        self.non_pui_high_arrival_day_mean=float(non_pui_high_arrival_day_mean)
        if df_input_non_pui_high_arrival is not None:

            # only select the non empty rows
            df_input_non_pui_high_arrival[COL_DAY]=np.arange(len(df_input_non_pui_high_arrival))
            df_input_non_pui_high_arrival = df_input_non_pui_high_arrival.loc[~df_input_non_pui_high_arrival['1'].isna()]
            
            self.n_historical_days = len(df_input_non_pui_high_arrival)
            self.n_total_days += (self.n_historical_days-1) # current days + projected days

            #initialize
            self.non_pui_high_arrival_hour = np.zeros(self.n_total_days*24).reshape([self.n_total_days,24])
            self.non_pui_high_census = np.zeros(self.n_total_days*24).reshape([self.n_total_days,24])
            self.non_pui_high_arrival_day=np.array([non_pui_high_arrival_day_mean]*self.n_total_days)

            for _, row in df_input_non_pui_high_arrival.iterrows():
                day=row[COL_DAY]
                self.non_pui_high_arrival_hour[day]=np.array(row)[1:25]
                #The total number
                self.non_pui_high_arrival_day[day]=row['Total']

            #initialize the census numbers for starting day:
            self.non_pui_high_census[0,0]=non_pui_high_census_0+self.non_pui_high_arrival_hour[0,0]
            for i in range(1,24):
                if i<self.los:
                    self.non_pui_high_census[0,i]=self.non_pui_high_census[0,i-1]+self.non_pui_high_arrival_hour[0,i]
                if i>=self.los:
                    self.non_pui_high_census[0,i]=sum(self.non_pui_high_arrival_hour[0,i-self.los+1:i+1])
            for day in range(1,self.n_historical_days):
                arrival_2day=np.concatenate((self.non_pui_high_arrival_hour[day-1],self.non_pui_high_arrival_hour[day]))
                for hour in range(0,24):
                    self.non_pui_high_census[day,hour]=sum(arrival_2day[hour+24-self.los+1:hour+24+1])

        else:
            self.non_pui_high_arrival_hour = np.zeros(self.n_total_days*24).reshape([self.n_total_days,24])
            self.non_pui_high_census = np.zeros(self.n_total_days*24).reshape([self.n_total_days,24])
            self.non_pui_high_arrival_day=np.array([non_pui_high_arrival_day_mean]*self.n_total_days)
            self.non_pui_high_arrival_hour[0]=np.multiply(self.non_pui_high_arrival_day[0],self.non_pui_high_arrival_hour_distribution)
            
            #initialize the census numbers for starting day:
            self.non_pui_high_census[0,0]=non_pui_high_census_0+self.non_pui_high_arrival_hour[0,0]
            for i in range(1,24):
                if i<self.los:
                    self.non_pui_high_census[0,i]=self.non_pui_high_census[0,i-1]+self.non_pui_high_arrival_hour[0,i]
                if i>=self.los:
                    self.non_pui_high_census[0,i]=sum(self.non_pui_high_arrival_hour[0,i-self.los+1:i+1])

        self.non_pui_high_admission=np.zeros(self.n_total_days*2).reshape([self.n_total_days,2])
        

    def run(self):
        '''
        run the simulation of self.n_total_days length
        '''

        for day in range(self.n_historical_days, self.n_total_days): # start from the next input day
            assert(day>0)
            # generate new arrival
            self.generate_new_arrival(day)
            self.update_census(day)
            self.update_admission(day)


    def generate_new_arrival(self, day):
        assert(day>=1)
        #get the simulated daily arrival
        #self.non_pui_high_arrival_day[day]=self.non_pui_high_arrival_day_mean
        self.non_pui_high_arrival_hour[day]=np.multiply(self.non_pui_high_arrival_day[day],self.non_pui_high_arrival_hour_distribution)

    def update_census(self, day):
        arrival_2day=np.concatenate((self.non_pui_high_arrival_hour[day-1],self.non_pui_high_arrival_hour[day]))
        for hour in range(0,24):
            self.non_pui_high_census[day,hour]=sum(arrival_2day[hour+24-self.los+1:hour+24+1])
    def update_admission(self,day):
        self.non_pui_high_admission[day]=np.multiply(self.non_pui_high_arrival_day[day],self.admission_fraction)

    def return_data(self):
        return [self.non_pui_high_arrival_day,self.non_pui_high_arrival_hour,
                self.non_pui_high_census,self.non_pui_high_admission]

class Non_PUI_Low_Simulator():
    def __init__(self, n_total_days = 10,
                    admission_fraction = [0,0.2],
                    los_non_pui_low=4,
                    non_pui_low_arrival_hour_distribution=[1/24]*24,
                    ):

        # number of total days in the simulation, all starting from STARTING_DAY
        self.n_historical_days = 1 #the number of historical data
        self.n_total_days = n_total_days + self.n_historical_days #the number of days need to be present in total.
        # Arrival related
        self.non_pui_low_arrival_hour_distribution=non_pui_low_arrival_hour_distribution
        self.non_pui_low_arrival_hour=None #2-dimension array
        self.non_pui_low_arrival_day=None#list
        # Census related
        self.non_pui_low_census = None #dataframe
        # Admission related
        self.non_pui_low_admission = None #2-dimension array

        # parameters for admission paths. Should be of dimension row=type, col=unit(2)
        self.admission_fraction = admission_fraction
        # parameters for los
        self.los = los_non_pui_low

    def state_init(self, non_pui_low_census_0=0, df_input_non_pui_low_arrival = None,non_pui_low_arrival_day_mean=10):
        '''
        initialze the states using input census
        major inputs:
            - df_input_pui_arrival: Dataframe for the arrival records of pui. The row indicates date, while column indicates hour.
        '''
        #Always assume we have hourly data. If users only have the daily total arrivals, tell them to breakdown.
        self.non_pui_low_arrival_day_mean=float(non_pui_low_arrival_day_mean)
        if df_input_non_pui_low_arrival is not None:

            # only select the non empty rows
            df_input_non_pui_low_arrival[COL_DAY]=np.arange(len(df_input_non_pui_low_arrival))
            df_input_non_pui_low_arrival = df_input_non_pui_low_arrival.loc[~df_input_non_pui_low_arrival['1'].isna()]
            
            self.n_historical_days = len(df_input_non_pui_low_arrival)
            self.n_total_days += (self.n_historical_days-1) # current days + projected days

            #initialize
            self.non_pui_low_arrival_hour = np.zeros(self.n_total_days*24).reshape([self.n_total_days,24])
            self.non_pui_low_census = np.zeros(self.n_total_days*24).reshape([self.n_total_days,24])
            self.non_pui_low_arrival_day=np.array([non_pui_low_arrival_day_mean]*self.n_total_days)

            for _, row in df_input_non_pui_low_arrival.iterrows():
                day=row[COL_DAY]
                self.non_pui_low_arrival_hour[day]=np.array(row)[1:25]
                #The total number
                self.non_pui_low_arrival_day[day]=row['Total']

            #initialize the census numbers for starting day:
            self.non_pui_low_census[0,0]=non_pui_low_census_0+self.non_pui_low_arrival_hour[0,0]
            for i in range(1,24):
                if i<self.los:
                    self.non_pui_low_census[0,i]=self.non_pui_low_census[0,i-1]+self.non_pui_low_arrival_hour[0,i]
                if i>=self.los:
                    self.non_pui_low_census[0,i]=sum(self.non_pui_low_arrival_hour[0,i-self.los+1:i+1])
            for day in range(1,self.n_historical_days):
                arrival_2day=np.concatenate((self.non_pui_low_arrival_hour[day-1],self.non_pui_low_arrival_hour[day]))
                for hour in range(0,24):
                    self.non_pui_low_census[day,hour]=sum(arrival_2day[hour+24-self.los+1:hour+24+1])
        else:
            self.non_pui_low_arrival_hour = np.zeros(self.n_total_days*24).reshape([self.n_total_days,24])
            self.non_pui_low_census = np.zeros(self.n_total_days*24).reshape([self.n_total_days,24])
            self.non_pui_low_arrival_day=np.array([non_pui_low_arrival_day_mean]*self.n_total_days)
            self.non_pui_low_arrival_hour[0]=np.multiply(self.non_pui_low_arrival_day[0],self.non_pui_low_arrival_hour_distribution)
            
            
            #initialize the census numbers for starting day:
            self.non_pui_low_census[0,0]=non_pui_low_census_0+self.non_pui_low_arrival_hour[0,0]
            for i in range(1,24):
                if i<self.los:
                    self.non_pui_low_census[0,i]=self.non_pui_low_census[0,i-1]+self.non_pui_low_arrival_hour[0,i]
                if i>=self.los:
                    self.non_pui_low_census[0,i]=sum(self.non_pui_low_arrival_hour[0,i-self.los+1:i+1])

        self.non_pui_low_admission=np.zeros(self.n_total_days*2).reshape([self.n_total_days,2])
        

    def run(self):
        '''
        run the simulation of self.n_total_days length
        '''

        for day in range(self.n_historical_days, self.n_total_days): # start from the next input day
            assert(day>0)
            # generate new arrival
            self.generate_new_arrival(day)
            self.update_census(day)
            self.update_admission(day)


    def generate_new_arrival(self, day):
        assert(day>=1)
        #get the simulated daily arrival
        #self.non_pui_low_arrival_day[day]=self.non_pui_low_arrival_day[day-1]
        self.non_pui_low_arrival_hour[day]=np.multiply(self.non_pui_low_arrival_day[day],self.non_pui_low_arrival_hour_distribution)

    def update_census(self, day):
        arrival_2day=np.concatenate((self.non_pui_low_arrival_hour[day-1],self.non_pui_low_arrival_hour[day]))
        for hour in range(0,24):
            self.non_pui_low_census[day,hour]=sum(arrival_2day[hour+24-self.los+1:hour+24+1])
    def update_admission(self,day):
        self.non_pui_low_admission[day]=np.multiply(self.non_pui_low_arrival_day[day],self.admission_fraction)

    def return_data(self):
        return [self.non_pui_low_arrival_day,self.non_pui_low_arrival_hour,
                self.non_pui_low_census,self.non_pui_low_admission]

def run_simulation(n_total_days = 10, doubling_time = 25, admission_fraction_list=[[0.2,0.8],[0,0.2],[0.2,0.8],[0,0.2]],
                    los_list=[6,3,4,3],
                    arrival_hour_distribution=[[1/24]*24,[1/24]*24,[1/24]*24,[1/24]*24],                    
                    dt_change_days_shared=[], # e.g. [1, 10, 15]
                    dt_change_dts_shared=[],
                    pui_high_census_0=0,
                    pui_high_cumulative_0=0,
                    pui_low_census_0=0,
                    pui_low_cumulative_0=0,
                    non_pui_high_census_0=0,
                    non_pui_low_census_0=0,
                    df_input_pui_high_arrival= None,
                    pui_high_arrival_day_mean=10,
                    df_input_pui_low_arrival= None,
                    pui_low_arrival_day_mean=10,
                    df_input_non_pui_high_arrival=None,
                    non_pui_high_arrival_day_mean=10,
                    df_input_non_pui_low_arrival=None,
                    non_pui_low_arrival_day_mean=10):
    #create pui high simulator and run simulation, return data.
    pui_high=PUI_High_Simulator(n_total_days,doubling_time,admission_fraction=admission_fraction_list[0],los_pui_high=los_list[0],
                    pui_high_arrival_hour_distribution=arrival_hour_distribution[0],
                    dt_change_days =dt_change_days_shared,
                    dt_change_dts = dt_change_dts_shared,
                    )
    pui_high.state_init(pui_high_census_0, df_input_pui_high_arrival,pui_high_arrival_day_mean,pui_high_cumulative_0)
    pui_high.run()
    pui_high_data=pui_high.return_data()

    #create pui low simulator and run simulation, return data.
    pui_low=PUI_Low_Simulator(n_total_days,doubling_time,admission_fraction=admission_fraction_list[1],los_pui_low=los_list[1],
                    pui_low_arrival_hour_distribution=arrival_hour_distribution[1],
                    dt_change_days =dt_change_days_shared,
                    dt_change_dts = dt_change_dts_shared,
                    )
    pui_low.state_init(pui_low_census_0, df_input_pui_low_arrival,pui_low_arrival_day_mean,pui_low_cumulative_0)
    pui_low.run()
    pui_low_data=pui_low.return_data()

    #create non pui high simulator and run simulation, return data.
    non_pui_high=Non_PUI_High_Simulator(n_total_days,admission_fraction=admission_fraction_list[2],los_non_pui_high=los_list[2],
                    non_pui_high_arrival_hour_distribution=arrival_hour_distribution[2])
    non_pui_high.state_init(non_pui_high_census_0, df_input_non_pui_high_arrival,non_pui_high_arrival_day_mean)
    non_pui_high.run()
    non_pui_high_data=non_pui_high.return_data()

    #create non pui low simulator and run simulation, return data.
    non_pui_low=Non_PUI_Low_Simulator(n_total_days,admission_fraction=admission_fraction_list[3],los_non_pui_low=los_list[3],
                    non_pui_low_arrival_hour_distribution=arrival_hour_distribution[3])
    non_pui_low.state_init(non_pui_low_census_0, df_input_non_pui_low_arrival,non_pui_low_arrival_day_mean)
    non_pui_low.run()
    non_pui_low_data=non_pui_low.return_data()
    #return pui_low
    return [pui_high_data,pui_low_data,non_pui_high_data,non_pui_low_data]