# read the file and parse it
# first separate into PUI and non-PUI

import pandas as pd
import datetime

def hourly_distribution_true(hourly_distribution):
	# user put in an hourly distributio
	# already as integer
	hourly_distribution_file = pd.read_csv(hourly_distribution)
	print(hourly_distribution_file)
	hourly_distribution_df = hourly_distribution_file.iloc[:,1:25]
	return hourly_distribution_df.values.tolist()

def merge_hourly_distribution(high_pui, low_pui, high_nonpui, low_nonpui):
	return [high_pui, low_pui, high_nonpui, low_nonpui]

def hourly_distribution_false(df):
	# if the user provides CSV files
	total_hours = df.iloc[:,1:25].sum(axis=0)
	my_sum = sum(df['Total'])
	hourly_distribution = total_hours/my_sum
	return hourly_distribution.values.tolist()


def parse_day_change(day_change_1_model, day_change_2_model, day_change_3_model):
	if day_change_1_model is not None or day_change_2_model is not None or day_change_3_model is not None:
		#return []
	#else:
		return [day_change_1_model, day_change_2_model, day_change_3_model]
	else:
		return []

def parse_double_change(double_change_1_model, double_change_2_model, double_change_3_model):
	if double_change_1_model is not None or not double_change_2_model is not None or double_change_3_model is not None:
		#return []
	#else:
		return [double_change_1_model, double_change_2_model, double_change_3_model]
	else:
		return []

def pui_day_mean_calculator(pui_high_arrival):
	# get the last date of historical projections
	my_sum = pui_high_arrival.iloc[-1,-1]# subtract the last value
	return float(my_sum)

def nonpui_day_mean_calculator(non_pui_arrival):
	my_sum = 0
	num_rows = 0
	my_sum = sum(non_pui_arrival['Total'])
	for row in non_pui_arrival.iterrows():
		num_rows = num_rows + 1
	return float(my_sum/num_rows)

def readfiles_pui_high_arrival(PUI_file):
	#print("PUI")
	# cut-off values for PUI and non-PUI acuity
	PUI_file = pd.read_csv(PUI_file)
	pui_high_percent = 0.15
	#PUI High Percent
	df_input_pui_high_arrival=pd.DataFrame()
	df_input_pui_high_arrival['Date'] = pd.to_datetime(PUI_file.iloc[:, 0])
	for x in range(1,25):
		df_input_pui_high_arrival[x] = PUI_file.iloc[:,x] * .15
	df_input_pui_high_arrival['Total']=df_input_pui_high_arrival.iloc[:,1:25].sum(axis=1)
	start_date = "2020-04-01"
	df_input_pui_high_arrival = df_input_pui_high_arrival[df_input_pui_high_arrival['Date'] >= '2020-04-01']
	return df_input_pui_high_arrival

def readfiles_pui_low_arrival(PUI_file):
	PUI_file = pd.read_csv(PUI_file)
	pui_low_percent = 0.85
	#PUI Low Percent
	df_input_pui_low_arrival=pd.DataFrame()
	df_input_pui_low_arrival['Date'] = pd.to_datetime(PUI_file.iloc[:, 0])
	for x in range(1,25):
		df_input_pui_low_arrival[x] = PUI_file.iloc[:,x] * .85
	df_input_pui_low_arrival['Total']=df_input_pui_low_arrival.iloc[:,1:25].sum(axis=1)
	start_date = "2020-04-01"
	df_input_pui_low_arrival = df_input_pui_low_arrival[df_input_pui_low_arrival['Date'] >= '2020-04-01']
	return df_input_pui_low_arrival


def readfiles_nonpui_high_arrival(nonPUI_file):
	PUI_file = pd.read_csv(nonPUI_file)
	nonpui_high_percent = 0.7651
	df_input_nonpui_high_arrival=pd.DataFrame()
	df_input_nonpui_high_arrival['Date'] = pd.to_datetime(PUI_file.iloc[:, 0])
	for x in range(1,25):
		df_input_nonpui_high_arrival[x] = PUI_file.iloc[:,x] * .7651
	df_input_nonpui_high_arrival['Total']=df_input_nonpui_high_arrival.iloc[:,1:25].sum(axis=1)
	start_date = "2020-04-01"
	df_input_nonpui_high_arrival = df_input_nonpui_high_arrival[df_input_nonpui_high_arrival['Date'] >= '2020-04-01']
	return df_input_nonpui_high_arrival

def readfiles_nonpui_low_arrival(nonPUI_file):
	PUI_file = pd.read_csv(nonPUI_file)
	nonpui_low_percent = 0.2349
	df_input_nonpui_low_arrival=pd.DataFrame()
	df_input_nonpui_low_arrival['Date'] = pd.to_datetime(PUI_file.iloc[:, 0])
	for x in range(1,25):
		df_input_nonpui_low_arrival[x] = PUI_file.iloc[:,x] * 0.2349
	df_input_nonpui_low_arrival['Total']=df_input_nonpui_low_arrival.iloc[:,1:25].sum(axis=1)
	start_date = "2020-04-01"
	df_input_nonpui_low_arrival = df_input_nonpui_low_arrival[df_input_nonpui_low_arrival['Date'] >= '2020-04-01']
	return df_input_nonpui_low_arrival



