library(shiny)

library(data.table)
library(dplyr)
library(lubridate)
library(ggplot2)
library(reticulate)
library(gridExtra)
library(grid)
library(RGraphics)
library(shinyMatrix)



shiny_project <- ""
shiny_project <- ""
prod <- paste0("/srv/shiny-server/", shiny_project)
dev <- paste0("~/shiny-server/", shiny_project)
if (!dir.exists(prod) & !dir.exists(dev)) {
  message(" using getwd() for shiny_path")
  shiny_path <- getwd()
} else {
  .libPaths(c(.libPaths(), "/srv/.R/library"))
  options(java.parameters = "-Xmx8048m")
  if (dir.exists(prod)) message("prod"); shiny_path <- prod
  if (dir.exists(dev)) message("dev"); shiny_path <- dev
}

library(viridis)
library(plotly)
library(reshape2)

ui <- shinyUI(
  list(
    HTML('<div style = "display: block; width: 100%; height: 60px; background: url(banner.png); background-repeat: no-repeat; background-size: auto; background-position: right center; background-size: contain; margin-right: 15px;"></div>'),
    navbarPage("Emergency Department COVID-19 Projection Model",
               tabPanel("Hourly Distribution Data",
                        sidebarPanel(
                          h4("Hourly Distribution Data"),
                          p("Please download and load the CSV files for all PUI and Non-PUI Arrivals per day. The row represents number of arrivals by hour, and the column represents the dates. All dates must contain 4/1/2020"),
                          fileInput("PUI_file", a(href = "https://bit.ly/2VyDZfA", "Template for PUI Arrivals", target = "_blank"),
                                    accept = c(".csv")
                          ),
                          fileInput("nonPUI_file", a(href = "https://bit.ly/2S40siq", "Template for Non PUI Arrivals", target = "_blank"),
                                    accept = c(".csv")
                          ),
                          hr(),
                          checkboxInput("hourly_distribution_bool", "Please select if you'd like to input your own hourly distribution in the table below instead of provide CSVs", value = FALSE, width = NULL),
                          hr(),
                          fileInput("hourly_distribution_pui_file", a(href = "https://bit.ly/2Y3UiCZ", "Hourly Distribution cases for PUI ", target = "_blank"),
                                    accept = c(".csv")
                          ),
                          hr(),
                          fileInput("hourly_distribution_nonpui_file", a(href = "https://bit.ly/2zq9TSW", "Hourly Distribution cases for Non-PUI", target = "_blank"),
                                    accept = c(".csv")
                          ),
                          hr(),
                          p("If you inputted hourly distributions instead of daily arrivals, please provide an estimation of your daily arrival means. The default is 10"),
                          numericInput("pui_high_mean", "PUI High Acuity Arrival Day Mean", 
                                       min = 0.00000, value = 10, step = 0.01),
                          numericInput("pui_low_mean", "PUI Low Acuity Arrival Day Mean", 
                                       min = 0.00000, value = 10, step = 0.01),
                          numericInput("nonpui_high_mean", "Non-PUI High Acuity Arrival Day Mean", 
                                       min = 0.00000, value = 10, step = 0.01),
                          numericInput("nonpui_low_mean", "Non-PUI Low Acuity Arrival Day Mean", 
                                       min = 0.00000, value = 10, step = 0.01),
                          p("If you inputted your own CSV files, the two data frames are displayed on the right hand of the screen. Please check that the data are outputted correctly before continuing.")
                        ),
                        mainPanel(
                          div('These models are planning tools and not predictions. They are based on data from Stanford and several public sources. The tools include assumptions that are changing as more information becomes available and will continue to evolve.',
                              style = 'margin-bottom: 15px'),
                          hr(),
                          tabsetPanel(type = "tabs",
                                      tabPanel("PUI CSV", tableOutput("pui_case")),
                                      tabPanel("Non-PUI CSV", tableOutput("non_pui_case"))
                                      
                          ),
                        ),
               ),
               tabPanel("Calculator",
                        sidebarPanel(
                          fluidRow(
                            column(12, 
                                   h4("General Parameters"),
                                   numericInput("num_days_simulation", "Number Days for Projection:", min = 1, value = 10, step = 1),
                                   numericInput( "doubling_time","Doubling Time for Cumulative PUIs In Hospital:",
                                                 min = 1,
                                                 value = 25, step = 0.5),
                                   h4("Simulation of Intervention"),
                                   HTML("To simulate the effects of interventions (e.g. social distancing), select up to three new doubling times and start times (days from today)"),
                                   fluidRow(
                                     column(6, tags$b("On Day")),
                                     column(6, tags$b("New Doubling Time"))
                                   ),
                                   fluidRow(
                                     column(6,
                                            numericInput("day_change_1", label=NULL, value=NA,NA, min = 1),
                                            numericInput("day_change_2", label=NULL, value=NA,NA, min = 1),
                                            numericInput("day_change_3", label=NULL, value=NA,NA, min = 1)
                                     ),
                                     column(6,
                                            numericInput("double_change_1", label=NULL, value=NA,NA, min = 1),
                                            numericInput("double_change_2", label=NULL, value=NA,NA, min = 1),
                                            numericInput("double_change_3", label=NULL, value=NA,NA, min = 1)
                                     ),
                                     column(12, style="display:center-align", 
                                            actionButton("load_dt_change_examples", "Fill Example Values"),
                                            actionButton("clear", "Clear"), br(),
                                            HTML('<a href="https://penn-chime.phl.io/" target="_blank" style="font-size:0.75em;">(Tool to estimate DT changes with changes in social interaction)</a>')
                                     ),
                                   ),
                                   numericInput( "potential_decrease_3","Potential Decrease Percentage for Not PUI and High Acuity (Index 3):",
                                                 min = 0.00000,
                                                 value = 1, step = 0.01),
                                   numericInput( "potential_decrease_4","Potential Decrease Percentage for Not PUI and Low Acuity (Index 4):",
                                                 min = 0.00000,
                                                 value = 0.9, step = 0.01),
                                   numericInput( "annual_growth_nonPUI","Annual Growth for All Non-PUI:", # percentage or not? 
                                                 min = 0.00000,
                                                 value = .03, step = 0.01),
                                   numericInput("pui_high_cumulative_0", "PUI High Cumulative 0", 
                                                min = 0.00000, value = 100, step = 0.00001),
                                   numericInput("pui_low_cumulative_0", "PUI Low Cumulative 0", 
                                                min = 0.00000, value = 100, step = 0.00001),
                                   numericInput("pui_high_census_0", "PUI High Census 0", 
                                                min = 0.00000, value = 0, step = 0.00001),
                                   numericInput("pui_low_census_0", "PUI Low Census 0", 
                                                min = 0.00000, value = 0, step = 0.00001),
                                   numericInput("non_pui_high_census_0", "PUI High Census 0", 
                                                min = 0.00000, value = 0, step = 0.00001),
                                   numericInput("non_pui_low_census_0", "PUI High Census 0", 
                                                min = 0.00000, value = 0, step = 0.00001),
                                   dateInput("selected_date", "Selected Day for Hourly Census and Hourly Arrival Rate. Default is day after current day. Please put in format MM/DD/YYYY:", value = NULL, min = NULL, max = NULL,
                                             format = "mm/dd/yyyy", startview = "month", weekstart = 0,
                                             language = "en", width = NULL, autoclose = TRUE,
                                             datesdisabled = NULL, daysofweekdisabled = NULL),
                            )
                          ),
                          width = 3
                        ),
                        mainPanel( # There will be four tabs as outputs
                          div('These models are planning tools and not predictions. They are based on data from Stanford and several public sources. The tools include assumptions that are changing as more information becomes available and will continue to evolve.',
                              style = 'margin-bottom: 15px'),
                          hr(),
                          tabsetPanel(type = "tabs",
                                      tabPanel("Hourly Census and Hourly Arrival Rate", plotOutput("byhour", width ="900px", height ="650px")),
                                      tabPanel("Daily Admissions", plotOutput("dailyadmissions", width ="900px", height ="650px")), 
                                      tabPanel("Daily Arrivals", plotOutput("dailyarrivals", width ="900px", height ="650px")) 
                          )
                        )
               ),
               tabPanel("Documentation",
                        p("For their help, we thank:"),
                        p("Johannes Ferstad, Andy Shin, Raymond Ye Lee, Sehj Kashyap, Shum Kenny, Saurabh Gombar, Nigam Shah")
               ),
               tabPanel("About",
                        fluidPage(
                          mainPanel(
                            h2("About"),
                            fluidRow(
                              column(width = 2, 
                                     h4(a(href = "https://profiles.stanford.edu/linying-yang", "Linying Yang", target = "_blank"), align = 'center'),
                                     HTML('<center><img src="linying.png" style="width:150px;height:150px;"></center>')),
                              column(width = 2, 
                                     h4(a(href = "https://www.linkedin.com/in/jin-xie-697a3b129/", "Jin Xie", target = "_blank"), align = 'center'),
                                     HTML('<center><img src="jin.png" style="width:150px;height:150px;"></center>')),
                              column(width = 2, 
                                     h4(a(href = "https://profiles.stanford.edu/blanchet", "Ian Brown", target = "_blank"), align = 'center'),
                                     HTML('<center><img src="ian.png" style="width:150px;height:150px;"></center>')),
                              column(width = 2, 
                                     h4(a(href = "https://profiles.stanford.edu/sam-shen", "Sam Shen", target = "_blank"), align = 'center'),
                                     HTML('<center><img src="sam.png" style="width:150px;height:150px;"></center>')),
                            ),
                            br(),
                            fluidRow(
                              column(width = 2, 
                                     h4(a(href = "https://web.stanford.edu/~glynn/", "Peter Glynn", target = "_blank"), align = 'center'),
                                     HTML('<center><img src="peter.png" style="width:150px;height:150px;"></center>')),
                              column(width = 2, 
                                     h4(a(href = "https://www.linkedin.com/in/fan423liu", "Fan Liu", target = "_blank"), align = 'center'),
                                     HTML('<center><img src="fan.png" style="width:150px;height:150px;"></center>')),
                              column(width = 2, 
                                     h4(a(href = "https://profiles.stanford.edu/david-scheinker", "David Scheinker", target = "_blank"), align = 'center'),
                                     HTML('<center><img src="david.png" style="width:150px;height:150px;"></center>'))
                            ),
                            width = 11
                          )
                        )
               )
               
    )
    
  )
)

server <- function(input, output, session) {
  
  observeEvent(input$clear, {
    updateNumericInput(session, "day_change_1", value = NA)
    updateNumericInput(session, "day_change_2", value = NA)
    updateNumericInput(session, "day_change_3", value = NA)
    updateNumericInput(session, "double_change_1", value = NA)
    updateNumericInput(session, "double_change_2", value = NA)
    updateNumericInput(session, "double_change_3", value = NA)
  })
  
  observeEvent(input$load_dt_change_examples, {
    updateNumericInput(session, "day_change_1", value = 1)
    updateNumericInput(session, "day_change_2", value = 10)
    updateNumericInput(session, "day_change_3", value = 15)
    updateNumericInput(session, "double_change_1", value = 9)
    updateNumericInput(session, "double_change_2", value = 12)
    updateNumericInput(session, "double_change_3", value = 14)
  })
  
  output$pui_case <- renderTable({
    if (!input$hourly_distribution_bool){
      inFile1 <- input$PUI_file
      if (is.null(inFile1)) return(NULL)
      data1 <- read.csv(inFile1$datapath, header = TRUE)
      colnames(data1) <- c("","0","1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15",
                           "16", "17", "18", "19", "20", "21", "22", "23")
      data1
    } else {
      inFile1 <- input$hourly_distribution_pui_file
      if (is.null(inFile1)) return(NULL)
      data1 <- read.csv(inFile1$datapath, header = TRUE)
      colnames(data1) <- c("Hour","0","1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15",
                           "16", "17", "18", "19", "20", "21", "22", "23")
      #colnames(data1) <- c("","0","1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15",
                           #"16", "17", "18", "19", "20", "21", "22", "23")
      data1
    }
  })
  
  output$non_pui_case <- renderTable({
    if (!input$hourly_distribution_bool){
      inFile1 <- input$nonPUI_file
      if (is.null(inFile1)) return(NULL)
      data1 <- read.csv(inFile1$datapath, header = TRUE)
      colnames(data1) <- c("","0","1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15",
                           "16", "17", "18", "19", "20", "21", "22", "23")
      data1
    } else {
      inFile1 <- input$hourly_distribution_nonpui_file
      if (is.null(inFile1)) return(NULL)
      data1 <- read.csv(inFile1$datapath, header = TRUE)
      colnames(data1) <- c("Hour","0","1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15",
                           "16", "17", "18", "19", "20", "21", "22", "23")
      data1
    }
  })
  
  output$byhour <- renderPlot({
    source_python("ed_simulator_newest.py")
    source_python("ed_file.py")
    
    num_days_simulation <- as.integer(input$num_days_simulation)
    my_doubling_time <- as.integer(input$doubling_time)
    potential_decrease_3 <- as.numeric(input$potential_decrease_3)
    potential_decrease_4 <- as.numeric(input$potential_decrease_4)
    annual_growth_nonPUI <- as.numeric(input$annual_growth_onPUI)
    
    # Day change models - already as to integer
    day_change_1_model=as.integer(input$day_change_1)
    day_change_2_model=as.integer(input$day_change_2)
    day_change_3_model=as.integer(input$day_change_3)
    double_change_1_model=as.integer(input$double_change_1)
    double_change_2_model=as.integer(input$double_change_2)
    double_change_3_model=as.integer(input$double_change_3)
    
    # Changing Doubling Time Parameters
    # CASE 1: everything is Null 
    if (is.na(day_change_1_model) &&  is.na(day_change_2_model) && is.na(day_change_3_model)
        && is.na(double_change_1_model) && is.na(double_change_2_model) && is.na(double_change_3_model)){
      days_changed = list(as.integer(0),as.integer(0),as.integer(0))
      dts_changed = list(my_doubling_time, my_doubling_time, my_doubling_time)
    }
    
    # CASE 2: only one row is filled
    if (!is.na(day_change_1_model) &&  is.na(day_change_2_model) && is.na(day_change_3_model)
        && !is.na(double_change_1_model) && is.na(double_change_2_model) && is.na(double_change_3_model)){
      days_changed = list(as.integer(0),as.integer(0),day_change_1_model)
      dts_changed = list(my_doubling_time, my_doubling_time, double_change_1_model)
    }
    
    if (is.na(day_change_1_model) &&  !is.na(day_change_2_model) && is.na(day_change_3_model)
        && is.na(double_change_1_model) && !is.na(double_change_2_model) && is.na(double_change_3_model)){
      days_changed = list(as.integer(0),as.integer(0),day_change_2_model)
      dts_changed = list(my_doubling_time, my_doubling_time, double_change_2_model)
    }
    
    if (is.na(day_change_1_model) &&  is.na(day_change_2_model) && !is.na(day_change_3_model)
        && is.na(double_change_1_model) && is.na(double_change_2_model) && !is.na(double_change_3_model)){
      days_changed = list(as.integer(0),as.integer(0),day_change_3_model)
      dts_changed = list(my_doubling_time, my_doubling_time, double_change_3_model)
    }
    
    # CASE 2: 
    if (!is.na(day_change_1_model) &&  !is.na(day_change_2_model) && is.na(day_change_3_model)
        && !is.na(double_change_1_model) && !is.na(double_change_2_model) && is.na(double_change_3_model)){
      # making them grouped together
      #print("CASE 2")
      if (day_change_1_model > day_change_2_model){
        my_max = c(day_change_1_model,double_change_1_model)
        my_min = c(day_change_2_model, double_change_2_model)
        #print(my_min)
      } else {
        my_min = c(day_change_1_model,double_change_1_model)
        my_max = c(day_change_2_model, double_change_2_model)
      }
      days_changed = list(as.integer(0), my_min[1], my_max[1])
      dts_changed = list(my_doubling_time, my_min[2], my_max[2])
    }
    
    if (!is.na(day_change_1_model) &&  is.na(day_change_2_model) && !is.na(day_change_3_model)
        && !is.na(double_change_1_model) && is.na(double_change_2_model) && !is.na(double_change_3_model)){
      # making them grouped together
      if (day_change_3_model > day_change_1_model){
        my_min = c(day_change_1_model, double_change_1_model)
        my_max = c(day_change_3_model, double_change_3_model)
      } else {
        my_min = c(day_change_3_model, double_change_3_model)
        my_max = c(day_change_1_model, double_change_1_model)
      }
      days_changed = list(as.integer(0), my_min[1], my_max[1])
      dts_changed = list(my_doubling_time, my_min[2], my_max[2])
    }
    
    if (is.na(day_change_1_model) &&  !is.na(day_change_2_model) && !is.na(day_change_3_model)
        && is.na(double_change_1_model) && !is.na(double_change_2_model) && !is.na(double_change_3_model)){
      # making them grouped together
      if (day_change_2_model > day_change_3_model){
        my_min = c(day_change_3_model, double_change_3_model)
        my_max = c(day_change_2_model, double_change_2_model)
      } else {
        my_min = c(day_change_2_model, double_change_2_model)
        my_max = c(day_change_3_model, double_change_3_model)
      }
      #print(my_min[0])
      days_changed = list(as.integer(0), my_min[1], my_max[1])
      dts_changed = list(my_doubling_time, my_min[2], my_max[2])
    }
    
    # CASE 4: all rows are filled out
    if (!is.na(day_change_1_model) &&  !is.na(day_change_2_model) && !is.na(day_change_3_model)
        && !is.na(double_change_1_model) && !is.na(double_change_2_model) && !is.na(double_change_3_model)){
      a <- c(day_change_1_model, double_change_1_model)
      b <- c(day_change_2_model, double_change_2_model)
      c <- c(day_change_3_model, double_change_3_model)
      my_sorted_list = list(a, b, c)
      my_sorted_list = my_sorted_list[order(sapply(my_sorted_list,'[[',1))]
      days_changed = list(my_sorted_list[[1]][1], my_sorted_list[[2]][1], my_sorted_list[[3]][1])
      dts_changed = list(my_sorted_list[[1]][2], my_sorted_list[[2]][2], my_sorted_list[[3]][2])
    }
    
    # PUI Census 0
    my_pui_high_census_0 <- as.integer(input$pui_high_census_0)
    my_pui_low_census_0 <- as.integer(input$pui_low_census_0)
    my_non_pui_high_census_0 <- as.integer(input$non_pui_high_census_0)
    my_non_pui_low_census_0 <- as.integer(input$non_pui_low_census_0)
    
    # calculating & finding hourly distribution
    hourly_distribution_bool <- input$hourly_distribution_bool
    if (input$hourly_distribution_bool){
      hourly_distribution_pui_file <- input$hourly_distribution_pui_file$datapath
      hourly_distribution_nonpui_file <- input$hourly_distribution_nonpui_file$datapath
      hourly_distribution_pui <- hourly_distribution_true(hourly_distribution_pui_file)
      hourly_distribution_nonpui <- hourly_distribution_true(hourly_distribution_nonpui_file)
      hourly_distribution_total = merge_hourly_distribution(hourly_distribution_pui, hourly_distribution_pui, hourly_distribution_nonpui, hourly_distribution_nonpui)
      puihigh_arrival_day_mean = input$pui_high_mean 
      puilow_arrival_day_mean = input$pui_low_mean
      nonpui_high_arrival_day_mean = input$nonpui_high_mean
      nonpui_low_arrival_day_mean = input$nonpui_low_mean
      pui_cum_high_0 = as.integer(input$pui_high_cumulative_0)
      pui_cum_low_0 = as.integer(input$pui_low_cumulative_0)
      data = run_simulation(pui_high_cumulative_0 = pui_cum_high_0, pui_low_cumulative_0 = pui_cum_low_0, pui_high_arrival_day_mean = puihigh_arrival_day_mean, pui_low_arrival_day_mean = puilow_arrival_day_mean, non_pui_high_arrival_day_mean = nonpui_high_arrival_day_mean, 
                            non_pui_low_arrival_day_mean = nonpui_low_arrival_day_mean, doubling_time = my_doubling_time, arrival_hour_distribution= hourly_distribution_total, 
                            dt_change_days_shared = days_changed, n_total_days = num_days_simulation, 
                            dt_change_dts_shared= dts_changed, pui_high_census_0 = my_pui_high_census_0,
                            pui_low_census_0 = my_pui_low_census_0, non_pui_high_census_0 = my_non_pui_high_census_0, non_pui_high_census_0 = my_non_pui_low_census_0)
    } 
    
    if (!input$hourly_distribution_bool){
      PUI_file <- input$PUI_file$datapath
      nonPUI_file <- input$nonPUI_file$datapath
      pui_high_arrival <- readfiles_pui_high_arrival(PUI_file)
      pui_low_arrival <- readfiles_pui_low_arrival(PUI_file)
      non_pui_low_arrival <- readfiles_nonpui_low_arrival(nonPUI_file)
      non_pui_high_arrival <- readfiles_nonpui_high_arrival(nonPUI_file)
      hourly_dist_pui_high = hourly_distribution_false(pui_high_arrival)
      hourly_dist_pui_low = hourly_distribution_false(pui_low_arrival)
      hourly_dist_nonpui_high = hourly_distribution_false(non_pui_high_arrival)
      hourly_dist_nonpui_low = hourly_distribution_false(non_pui_low_arrival)
      hourly_distribution_total = merge_hourly_distribution(hourly_dist_pui_high, hourly_dist_pui_low, hourly_dist_nonpui_high, hourly_dist_nonpui_low)
      # Mean calcualations
      puihigh_arrival_day_mean = pui_day_mean_calculator(pui_high_arrival)
      puilow_arrival_day_mean = pui_day_mean_calculator(pui_low_arrival)
      historical_nonpui_high = nonpui_day_mean_calculator(non_pui_high_arrival)
      historical_nonpui_low = nonpui_day_mean_calculator(non_pui_low_arrival)
      nonpui_high_arrival_day_mean = historical_nonpui_high * input$potential_decrease_3 * (1 + input$annual_growth_nonPUI)
      nonpui_low_arrival_day_mean = historical_nonpui_low * input$potential_decrease_4 * (1 + input$annual_growth_nonPUI)
      pui_cum_high_0 = as.integer(input$pui_high_cumulative_0)
      pui_cum_low_0 = as.integer(input$pui_low_cumulative_0)
      data = run_simulation(pui_high_cumulative_0 = pui_cum_high_0, pui_low_cumulative_0 = pui_cum_low_0, pui_high_arrival_day_mean = puihigh_arrival_day_mean, pui_low_arrival_day_mean = puilow_arrival_day_mean,
                            non_pui_high_arrival_day_mean = nonpui_high_arrival_day_mean, 
                            non_pui_low_arrival_day_mean = nonpui_low_arrival_day_mean, dt_change_days_shared = days_changed, # e.g. [1, 10, 15]
                            dt_change_dts_shared= dts_changed, n_total_days = num_days_simulation,
                            doubling_time = my_doubling_time, df_input_pui_high_arrival = pui_high_arrival, df_input_pui_low_arrival= pui_low_arrival,
                            df_input_non_pui_high_arrival= non_pui_high_arrival, df_input_non_pui_low_arrival=non_pui_low_arrival,
                            pui_high_census_0 = my_pui_high_census_0,
                            pui_low_census_0 = my_pui_low_census_0, non_pui_high_census_0 = my_non_pui_high_census_0, non_pui_high_census_0 = my_non_pui_low_census_0)
      # updating the time
      added_time <- seq(pui_high_arrival$Date[length(pui_high_arrival$Date)] + 1, by = "day", length.out = num_days_simulation)
      updated_time <- c(pui_high_arrival$Date, added_time)
    }
   
    # obtain row of that selected day
    selected_date <- input$selected_date
    default_date <- mdy("04/01/2020")
    my_int <- interval(default_date, selected_date)
    num_days <- time_length(my_int, "days") + 1
    time_char <- as.character(selected_date)
    
    # HOURLY ARRIVALS PER DAY
    PUI_high <- data[[1]][[2]][num_days,]
    PUI_low <- data[[2]][[2]][num_days,]
    Non_PUI_high <- data[[3]][[2]][num_days,]
    Non_PUI_low <- data[[3]][[2]][num_days,]
    updated_time <- 0:23
    
    #DAILY ARRIVALS TOTAL
    hourly_arrivals_total <- as.data.frame(PUI_high) + as.data.frame(PUI_low) + as.data.frame(Non_PUI_high) + as.data.frame(Non_PUI_low)
    df_harrivals <- cbind(updated_time, as.data.frame(PUI_high), as.data.frame(PUI_low), as.data.frame(Non_PUI_high), as.data.frame(Non_PUI_low), hourly_arrivals_total)
    names(df_harrivals) <- c("updated_time", "PUI_high", "PUI_low", "Non_PUI_high", "Non_PUI_low", "hourly_arrivals_total")
    arrivals_plot <- ggplot(data = df_harrivals, aes(x= updated_time)) + 
      geom_line(aes(y = PUI_high, col = "PUI High Acuity Arrival", group = 1)) + geom_point(aes(y = PUI_high, col = "PUI High Acuity Arrival")) +
      geom_line(aes(y = PUI_low, col = "PUI Low Acuity Arrival", group = 1))  + geom_point(aes(y = PUI_low, col = "PUI Low Acuity Arrival")) +
      geom_line(aes(y = Non_PUI_high, col = "Non-PUI High Acuity Arrival", group = 1)) + geom_point(aes(y= Non_PUI_high, col = "Non-PUI High Acuity Arrival")) +
      geom_line(aes(y = Non_PUI_low, col = "Non-PUI Low Acuity Arrival", group = 1)) + geom_point(aes(y = Non_PUI_low, col = "Non-PUI Low Acuity Arrival")) +
      geom_line(aes(y = hourly_arrivals_total, col = "Total", group = 1)) + geom_point(aes(y = hourly_arrivals_total, col = "Total")) + ggtitle(paste("ED Arrivals by Hour on", time_char)) + labs(x = "by Hour", y = "Number of Patients")
    
    # HOURLY CENSUS PER DAY
    PUI_high <- data[[1]][[3]][num_days,]
    PUI_low <- data[[2]][[3]][num_days,]
    Non_PUI_high <- data[[3]][[3]][num_days,]
    Non_PUI_low <- data[[3]][[3]][num_days,]
    
    # DAILY ADMISSIONS TOTAL
    hourly_arrivals_total <- as.data.frame(PUI_high) + as.data.frame(PUI_low) + as.data.frame(Non_PUI_high) + as.data.frame(Non_PUI_low)
    df_harrivals <- cbind(updated_time, as.data.frame(PUI_high), as.data.frame(PUI_low), as.data.frame(Non_PUI_high), as.data.frame(Non_PUI_low), hourly_arrivals_total)
    names(df_harrivals) <- c("updated_time", "PUI_high", "PUI_low", "Non_PUI_high", "Non_PUI_low", "hourly_arrivals_total")
    admissions_plot <- ggplot(data = df_harrivals, aes(x= updated_time)) +
      geom_line(aes(y = PUI_high, col = "PUI High Acuity Census", group = 1)) + geom_point(aes(y = PUI_high, col = "PUI High Acuity Census")) +
      geom_line(aes(y = PUI_low, col = "PUI Low Acuity Census", group = 1))  + geom_point(aes(y = PUI_low, col = "PUI Low Acuity Census")) +
      geom_line(aes(y = Non_PUI_high, col = "Non-PUI High Acuity Census", group = 1)) + geom_point(aes(y= Non_PUI_high, col = "Non-PUI High Acuity Census")) +
      geom_line(aes(y = Non_PUI_low, col = "Non-PUI Low Acuity Census", group = 1)) + geom_point(aes(y = Non_PUI_low, col = "Non-PUI Low Acuity Census")) +
      geom_line(aes(y = hourly_arrivals_total, col = "Total", group = 1)) + geom_point(aes(y = hourly_arrivals_total, col = "Total")) + ggtitle(paste("ED Census by Hour on", time_char)) + labs(x = "by Hour", y = "Number of Patients")
    grid.arrange(arrivals_plot, admissions_plot)
  })
  
  
  output$dailyarrivals <- renderPlot({
    source_python("ed_simulator_newest.py")
    source_python("ed_file.py")
    
    num_days_simulation <- as.integer(input$num_days_simulation)
    my_doubling_time <- as.integer(input$doubling_time)
    potential_decrease_3 <- as.numeric(input$potential_decrease_3)
    potential_decrease_4 <- as.numeric(input$potential_decrease_4)
    annual_growth_nonPUI <- as.numeric(input$annual_growth_onPUI)
    
    # Day change models - already as to integer
    day_change_1_model=as.integer(input$day_change_1)
    day_change_2_model=as.integer(input$day_change_2)
    day_change_3_model=as.integer(input$day_change_3)
    double_change_1_model=as.integer(input$double_change_1)
    double_change_2_model=as.integer(input$double_change_2)
    double_change_3_model=as.integer(input$double_change_3)
    
    # Changing Doubling Time Parameters
    # CASE 1: everything is Null 
    if (is.na(day_change_1_model) &&  is.na(day_change_2_model) && is.na(day_change_3_model)
        && is.na(double_change_1_model) && is.na(double_change_2_model) && is.na(double_change_3_model)){
      days_changed = list(as.integer(0),as.integer(0),as.integer(0))
      dts_changed = list(my_doubling_time, my_doubling_time, my_doubling_time)
    }
    
    # CASE 2: only one row is filled
    if (!is.na(day_change_1_model) &&  is.na(day_change_2_model) && is.na(day_change_3_model)
        && !is.na(double_change_1_model) && is.na(double_change_2_model) && is.na(double_change_3_model)){
      days_changed = list(as.integer(0),as.integer(0),day_change_1_model)
      dts_changed = list(my_doubling_time, my_doubling_time, double_change_1_model)
    }
    
    if (is.na(day_change_1_model) &&  !is.na(day_change_2_model) && is.na(day_change_3_model)
        && is.na(double_change_1_model) && !is.na(double_change_2_model) && is.na(double_change_3_model)){
      days_changed = list(as.integer(0),as.integer(0),day_change_2_model)
      dts_changed = list(my_doubling_time, my_doubling_time, double_change_2_model)
    }
    
    if (is.na(day_change_1_model) &&  is.na(day_change_2_model) && !is.na(day_change_3_model)
        && is.na(double_change_1_model) && is.na(double_change_2_model) && !is.na(double_change_3_model)){
      days_changed = list(as.integer(0),as.integer(0),day_change_3_model)
      dts_changed = list(my_doubling_time, my_doubling_time, double_change_3_model)
    }
    
    # CASE 2: 
    if (!is.na(day_change_1_model) &&  !is.na(day_change_2_model) && is.na(day_change_3_model)
        && !is.na(double_change_1_model) && !is.na(double_change_2_model) && is.na(double_change_3_model)){
      # making them grouped together
      #print("CASE 2")
      if (day_change_1_model > day_change_2_model){
        my_max = c(day_change_1_model,double_change_1_model)
        my_min = c(day_change_2_model, double_change_2_model)
        #print(my_min)
      } else {
        my_min = c(day_change_1_model,double_change_1_model)
        my_max = c(day_change_2_model, double_change_2_model)
      }
      days_changed = list(as.integer(0), my_min[1], my_max[1])
      dts_changed = list(my_doubling_time, my_min[2], my_max[2])
    }
    
    if (!is.na(day_change_1_model) &&  is.na(day_change_2_model) && !is.na(day_change_3_model)
        && !is.na(double_change_1_model) && is.na(double_change_2_model) && !is.na(double_change_3_model)){
      # making them grouped together
      if (day_change_3_model > day_change_1_model){
        my_min = c(day_change_1_model, double_change_1_model)
        my_max = c(day_change_3_model, double_change_3_model)
      } else {
        my_min = c(day_change_3_model, double_change_3_model)
        my_max = c(day_change_1_model, double_change_1_model)
      }
      days_changed = list(as.integer(0), my_min[1], my_max[1])
      dts_changed = list(my_doubling_time, my_min[2], my_max[2])
    }
    
    if (is.na(day_change_1_model) &&  !is.na(day_change_2_model) && !is.na(day_change_3_model)
        && is.na(double_change_1_model) && !is.na(double_change_2_model) && !is.na(double_change_3_model)){
      # making them grouped together
      if (day_change_2_model > day_change_3_model){
        my_min = c(day_change_3_model, double_change_3_model)
        my_max = c(day_change_2_model, double_change_2_model)
      } else {
        my_min = c(day_change_2_model, double_change_2_model)
        my_max = c(day_change_3_model, double_change_3_model)
      }
      #print(my_min[0])
      days_changed = list(as.integer(0), my_min[1], my_max[1])
      dts_changed = list(my_doubling_time, my_min[2], my_max[2])
    }
    
    # CASE 4: all rows are filled out
    if (!is.na(day_change_1_model) &&  !is.na(day_change_2_model) && !is.na(day_change_3_model)
        && !is.na(double_change_1_model) && !is.na(double_change_2_model) && !is.na(double_change_3_model)){
      a <- c(day_change_1_model, double_change_1_model)
      b <- c(day_change_2_model, double_change_2_model)
      c <- c(day_change_3_model, double_change_3_model)
      my_sorted_list = list(a, b, c)
      my_sorted_list = my_sorted_list[order(sapply(my_sorted_list,'[[',1))]
      days_changed = list(my_sorted_list[[1]][1], my_sorted_list[[2]][1], my_sorted_list[[3]][1])
      dts_changed = list(my_sorted_list[[1]][2], my_sorted_list[[2]][2], my_sorted_list[[3]][2])
    }
    
    # PUI Census 0
    my_pui_high_census_0 <- as.integer(input$pui_high_census_0)
    my_pui_low_census_0 <- as.integer(input$pui_low_census_0)
    my_non_pui_high_census_0 <- as.integer(input$non_pui_high_census_0)
    my_non_pui_low_census_0 <- as.integer(input$non_pui_low_census_0)
    
    # calculating & finding hourly distribution
    hourly_distribution_bool <- input$hourly_distribution_bool
    if (input$hourly_distribution_bool){
      hourly_distribution_pui_file <- input$hourly_distribution_pui_file$datapath
      hourly_distribution_nonpui_file <- input$hourly_distribution_nonpui_file$datapath
      hourly_distribution_pui <- hourly_distribution_true(hourly_distribution_pui_file)
      hourly_distribution_nonpui <- hourly_distribution_true(hourly_distribution_nonpui_file)
      hourly_distribution_total = merge_hourly_distribution(hourly_distribution_pui, hourly_distribution_pui, hourly_distribution_nonpui, hourly_distribution_nonpui)
      puihigh_arrival_day_mean = input$pui_high_mean 
      puilow_arrival_day_mean = input$pui_low_mean
      nonpui_high_arrival_day_mean = input$nonpui_high_mean
      nonpui_low_arrival_day_mean = input$nonpui_low_mean
      pui_cum_high_0 = input$pui_high_cumulative_0
      pui_cum_low_0 = input$pui_low_cumulative_0
      data = run_simulation(pui_high_cumulative_0 = pui_cum_high_0, pui_low_cumulative_0 = pui_cum_low_0, pui_high_arrival_day_mean = puihigh_arrival_day_mean, pui_low_arrival_day_mean = puilow_arrival_day_mean, non_pui_high_arrival_day_mean = nonpui_high_arrival_day_mean, 
                            non_pui_low_arrival_day_mean = nonpui_low_arrival_day_mean, doubling_time = my_doubling_time, arrival_hour_distribution= hourly_distribution_total, 
                            dt_change_days_shared = days_changed, n_total_days = num_days_simulation, 
                            dt_change_dts_shared= dts_changed, pui_high_census_0 = my_pui_high_census_0,
                            pui_low_census_0 = my_pui_low_census_0, non_pui_high_census_0 = my_non_pui_high_census_0, non_pui_high_census_0 = my_non_pui_low_census_0)
      updated_time <- seq(as.Date("2020/04/01"), by = "day", length.out = num_days_simulation + 1)
      print(updated_time)
      print(length(updated_time))
      } 
    
    if (!input$hourly_distribution_bool){
      PUI_file <- input$PUI_file$datapath
      nonPUI_file <- input$nonPUI_file$datapath
      pui_high_arrival <- readfiles_pui_high_arrival(PUI_file)
      pui_low_arrival <- readfiles_pui_low_arrival(PUI_file)
      non_pui_low_arrival <- readfiles_nonpui_low_arrival(nonPUI_file)
      non_pui_high_arrival <- readfiles_nonpui_high_arrival(nonPUI_file)
      hourly_dist_pui_high = hourly_distribution_false(pui_high_arrival)
      hourly_dist_pui_low = hourly_distribution_false(pui_low_arrival)
      hourly_dist_nonpui_high = hourly_distribution_false(non_pui_high_arrival)
      hourly_dist_nonpui_low = hourly_distribution_false(non_pui_low_arrival)
      hourly_distribution_total = merge_hourly_distribution(hourly_dist_pui_high, hourly_dist_pui_low, hourly_dist_nonpui_high, hourly_dist_nonpui_low)
      # Mean calcualations
      puihigh_arrival_day_mean = pui_day_mean_calculator(pui_high_arrival)
      puilow_arrival_day_mean = pui_day_mean_calculator(pui_low_arrival)
      historical_nonpui_high = nonpui_day_mean_calculator(non_pui_high_arrival)
      historical_nonpui_low = nonpui_day_mean_calculator(non_pui_low_arrival)
      nonpui_high_arrival_day_mean = historical_nonpui_high * input$potential_decrease_3 * (1 + input$annual_growth_nonPUI)
      nonpui_low_arrival_day_mean = historical_nonpui_low * input$potential_decrease_4 * (1 + input$annual_growth_nonPUI)
      pui_cum_high_0 = input$pui_high_cumulative_0
      pui_cum_low_0 = input$pui_low_cumulative_0
      data = run_simulation(pui_high_cumulative_0 = pui_cum_high_0, pui_low_cumulative_0 = pui_cum_low_0, pui_high_arrival_day_mean = puihigh_arrival_day_mean, pui_low_arrival_day_mean = puilow_arrival_day_mean,
                            non_pui_high_arrival_day_mean = nonpui_high_arrival_day_mean, 
                            non_pui_low_arrival_day_mean = nonpui_low_arrival_day_mean, dt_change_days_shared = days_changed, # e.g. [1, 10, 15]
                            dt_change_dts_shared= dts_changed, n_total_days = num_days_simulation,
                            doubling_time = my_doubling_time, df_input_pui_high_arrival = pui_high_arrival, df_input_pui_low_arrival= pui_low_arrival,
                            df_input_non_pui_high_arrival= non_pui_high_arrival, df_input_non_pui_low_arrival=non_pui_low_arrival,
                            pui_high_census_0 = my_pui_high_census_0,
                            pui_low_census_0 = my_pui_low_census_0, non_pui_high_census_0 = my_non_pui_high_census_0, non_pui_high_census_0 = my_non_pui_low_census_0)
      # updating the time
      added_time <- seq(pui_high_arrival$Date[length(pui_high_arrival$Date)] + 1, by = "day", length.out = num_days_simulation)
      updated_time <- c(pui_high_arrival$Date, added_time)
    }
  
    #DAILY ARRIVALS TOTAL
    print(data[[1]][[1]])
    print(length(data[[1]][[1]]))
    daily_arrivals_total <- as.data.frame(data[[1]][[1]]) + as.data.frame(data[[2]][[1]]) + as.data.frame(data[[3]][[1]]) + as.data.frame(data[[4]][[1]])
    df_arrivals <- cbind(updated_time, as.data.frame(data[[1]][[1]]), as.data.frame(data[[2]][[1]]), as.data.frame(data[[3]][[1]]), as.data.frame(data[[4]][[1]]), daily_arrivals_total)
    names(df_arrivals) <- c("updated_time", "data[[1]][[1]]", "data[[2]][[1]]", "data[[3]][[1]]", "data[[4]][[1]]", "daily_arrivals_total")
    plot1 <- ggplot(data = df_arrivals, aes(x= updated_time)) + geom_line(aes(y = data[[1]][[1]], col = "PUI High Arrival", group = 1)) +
      geom_point(aes(y = data[[1]][[1]], col = "PUI High Arrival")) +
      geom_line(aes(y = data[[3]][[1]], col = "Non-PUI High Arrival", group = 1))  + geom_point(aes(y = data[[3]][[1]], col = "Non-PUI High Arrival"))  +
      geom_line(aes(y = data[[4]][[1]], col = "Non-PUI Low Arrival", group = 1)) + geom_point(aes(y= data[[4]][[1]], col = "Non-PUI Low Arrival")) +
      geom_line(aes(y = data[[2]][[1]], col = "PUI Low Arrival", group = 1)) + geom_point(aes(y = data[[2]][[1]], col = "PUI Low Arrival")) +
      geom_line(aes(y = daily_arrivals_total, col = "Total", group = 1)) + geom_point(aes(y = daily_arrivals_total, col = "Total")) + ggtitle("ED Arrivals per Day by Patient Classification, Adults") + labs(x = "Date", y = NULL)
    
    
    # DAILY ARRIVALS PUI ONLY
    pui_df_arrivals <- cbind(updated_time, as.data.frame(data[[1]][[1]]), as.data.frame(data[[2]][[1]]), as.data.frame(data[[1]][[1]] + data[[2]][[1]]))
    names(pui_df_arrivals) <- c("updated_time", "data[[1]][[1]]", "data[[2]][[1]]", "daily_arrivals_total")
    plot2 <- ggplot(data = pui_df_arrivals, aes(x= updated_time)) + geom_line(aes(y = data[[1]][[1]], col = "PUI High Arrival", group = 1)) +
      geom_point(aes(y = data[[1]][[1]], col = "PUI High Arrival")) +
      geom_line(aes(y = data[[2]][[1]], col = "PUI Low Arrival", group = 1)) + geom_point(aes(y = data[[2]][[1]], col = "PUI Low Arrival")) +
      geom_line(aes(y = daily_arrivals_total, col = "Total", group = 1)) + geom_point(aes(y = daily_arrivals_total, col = "Total")) + labs(x = "Date", y = NULL) + ggtitle("ED Arrivals per Day by Patient Classification, PUI, Adults")
    
    # DAILY ARRIVALS NON-PUI ONLY
    nonpui_df_arrivals <- cbind(updated_time, as.data.frame(data[[3]][[1]]), as.data.frame(data[[4]][[1]]), as.data.frame(data[[3]][[1]] + data[[4]][[1]]))
    names(nonpui_df_arrivals) <- c("updated_time", "data[[3]][[1]]", "data[[4]][[1]]", "daily_arrivals_total")
    #nonpui_df_arrivals$daily_arrivals_total <- as.data.frame(data[[3]][[1]] + data[[4]][[1]])
    plot3 <- ggplot(data = nonpui_df_arrivals, aes(x= updated_time)) + geom_line(aes(y = data[[3]][[1]], col = "Non-PUI High Arrival", group = 1)) +
      geom_point(aes(y = data[[3]][[1]], col = "Non-PUI High Arrival")) +
      geom_line(aes(y = data[[4]][[1]], col = "Non-PUI Low Arrival", group = 1)) + geom_point(aes(y = data[[4]][[1]], col = "Non-PUI Low Arrival")) +
      geom_line(aes(y = daily_arrivals_total, col = "Total", group = 1)) + geom_point(aes(y = daily_arrivals_total, col = "Total")) + labs(x = "Date", y = NULL) + ggtitle("ED Arrivals per Day by Patient Classification, Non-PUI, Adults")
    
    grid.arrange(plot1, plot2, plot3)
    
  })
  
  output$dailyadmissions <- renderPlot({
    source_python("ed_simulator_newest.py")
    source_python("ed_file.py")
    
    num_days_simulation <- as.integer(input$num_days_simulation)
    my_doubling_time <- as.integer(input$doubling_time)
    potential_decrease_3 <- as.numeric(input$potential_decrease_3)
    potential_decrease_4 <- as.numeric(input$potential_decrease_4)
    annual_growth_nonPUI <- as.numeric(input$annual_growth_onPUI)
    
    # Day change models - already as to integer
    day_change_1_model=as.integer(input$day_change_1)
    day_change_2_model=as.integer(input$day_change_2)
    day_change_3_model=as.integer(input$day_change_3)
    double_change_1_model=as.integer(input$double_change_1)
    double_change_2_model=as.integer(input$double_change_2)
    double_change_3_model=as.integer(input$double_change_3)
    
    # Changing Doubling Time Parameters
    # CASE 1: everything is Null 
    if (is.na(day_change_1_model) &&  is.na(day_change_2_model) && is.na(day_change_3_model)
        && is.na(double_change_1_model) && is.na(double_change_2_model) && is.na(double_change_3_model)){
      days_changed = list(as.integer(0),as.integer(0),as.integer(0))
      dts_changed = list(my_doubling_time, my_doubling_time, my_doubling_time)
    }
    
    # CASE 2: only one row is filled
    if (!is.na(day_change_1_model) &&  is.na(day_change_2_model) && is.na(day_change_3_model)
        && !is.na(double_change_1_model) && is.na(double_change_2_model) && is.na(double_change_3_model)){
      days_changed = list(as.integer(0),as.integer(0),day_change_1_model)
      dts_changed = list(my_doubling_time, my_doubling_time, double_change_1_model)
    }
    
    if (is.na(day_change_1_model) &&  !is.na(day_change_2_model) && is.na(day_change_3_model)
        && is.na(double_change_1_model) && !is.na(double_change_2_model) && is.na(double_change_3_model)){
      days_changed = list(as.integer(0),as.integer(0),day_change_2_model)
      dts_changed = list(my_doubling_time, my_doubling_time, double_change_2_model)
    }
    
    if (is.na(day_change_1_model) &&  is.na(day_change_2_model) && !is.na(day_change_3_model)
        && is.na(double_change_1_model) && is.na(double_change_2_model) && !is.na(double_change_3_model)){
      days_changed = list(as.integer(0),as.integer(0),day_change_3_model)
      dts_changed = list(my_doubling_time, my_doubling_time, double_change_3_model)
    }
    
    # CASE 2: 
    if (!is.na(day_change_1_model) &&  !is.na(day_change_2_model) && is.na(day_change_3_model)
        && !is.na(double_change_1_model) && !is.na(double_change_2_model) && is.na(double_change_3_model)){
      # making them grouped together
      #print("CASE 2")
      if (day_change_1_model > day_change_2_model){
        my_max = c(day_change_1_model,double_change_1_model)
        my_min = c(day_change_2_model, double_change_2_model)
        #print(my_min)
      } else {
        my_min = c(day_change_1_model,double_change_1_model)
        my_max = c(day_change_2_model, double_change_2_model)
      }
      days_changed = list(as.integer(0), my_min[1], my_max[1])
      dts_changed = list(my_doubling_time, my_min[2], my_max[2])
    }
    
    if (!is.na(day_change_1_model) &&  is.na(day_change_2_model) && !is.na(day_change_3_model)
        && !is.na(double_change_1_model) && is.na(double_change_2_model) && !is.na(double_change_3_model)){
      # making them grouped together
      if (day_change_3_model > day_change_1_model){
        my_min = c(day_change_1_model, double_change_1_model)
        my_max = c(day_change_3_model, double_change_3_model)
      } else {
        my_min = c(day_change_3_model, double_change_3_model)
        my_max = c(day_change_1_model, double_change_1_model)
      }
      days_changed = list(as.integer(0), my_min[1], my_max[1])
      dts_changed = list(my_doubling_time, my_min[2], my_max[2])
    }
    
    if (is.na(day_change_1_model) &&  !is.na(day_change_2_model) && !is.na(day_change_3_model)
        && is.na(double_change_1_model) && !is.na(double_change_2_model) && !is.na(double_change_3_model)){
      # making them grouped together
      if (day_change_2_model > day_change_3_model){
        my_min = c(day_change_3_model, double_change_3_model)
        my_max = c(day_change_2_model, double_change_2_model)
      } else {
        my_min = c(day_change_2_model, double_change_2_model)
        my_max = c(day_change_3_model, double_change_3_model)
      }
      #print(my_min[0])
      days_changed = list(as.integer(0), my_min[1], my_max[1])
      dts_changed = list(my_doubling_time, my_min[2], my_max[2])
    }
    
    # CASE 4: all rows are filled out
    if (!is.na(day_change_1_model) &&  !is.na(day_change_2_model) && !is.na(day_change_3_model)
        && !is.na(double_change_1_model) && !is.na(double_change_2_model) && !is.na(double_change_3_model)){
      a <- c(day_change_1_model, double_change_1_model)
      b <- c(day_change_2_model, double_change_2_model)
      c <- c(day_change_3_model, double_change_3_model)
      my_sorted_list = list(a, b, c)
      my_sorted_list = my_sorted_list[order(sapply(my_sorted_list,'[[',1))]
      days_changed = list(my_sorted_list[[1]][1], my_sorted_list[[2]][1], my_sorted_list[[3]][1])
      dts_changed = list(my_sorted_list[[1]][2], my_sorted_list[[2]][2], my_sorted_list[[3]][2])
    }
    
    # PUI Census 0
    my_pui_high_census_0 <- as.integer(input$pui_high_census_0)
    my_pui_low_census_0 <- as.integer(input$pui_low_census_0)
    my_non_pui_high_census_0 <- as.integer(input$non_pui_high_census_0)
    my_non_pui_low_census_0 <- as.integer(input$non_pui_low_census_0)
    
    # calculating & finding hourly distribution
    hourly_distribution_bool <- input$hourly_distribution_bool
    if (input$hourly_distribution_bool){
      hourly_distribution_pui_file <- input$hourly_distribution_pui_file$datapath
      hourly_distribution_nonpui_file <- input$hourly_distribution_nonpui_file$datapath
      hourly_distribution_pui <- hourly_distribution_true(hourly_distribution_pui_file)
      hourly_distribution_nonpui <- hourly_distribution_true(hourly_distribution_nonpui_file)
      hourly_distribution_total = merge_hourly_distribution(hourly_distribution_pui, hourly_distribution_pui, hourly_distribution_nonpui, hourly_distribution_nonpui)
      puihigh_arrival_day_mean = input$pui_high_mean 
      puilow_arrival_day_mean = input$pui_low_mean
      nonpui_high_arrival_day_mean = input$nonpui_high_mean
      nonpui_low_arrival_day_mean = input$nonpui_low_mean
      pui_cum_high_0 = input$pui_high_cumulative_0
      pui_cum_low_0 = input$pui_low_cumulative_0
      data = run_simulation(pui_high_cumulative_0 = pui_cum_high_0, pui_low_cumulative_0 = pui_cum_low_0, pui_high_arrival_day_mean = puihigh_arrival_day_mean, pui_low_arrival_day_mean = puilow_arrival_day_mean, non_pui_high_arrival_day_mean = nonpui_high_arrival_day_mean, 
                            non_pui_low_arrival_day_mean = nonpui_low_arrival_day_mean, doubling_time = my_doubling_time, arrival_hour_distribution= hourly_distribution_total, 
                            dt_change_days_shared = days_changed, n_total_days = num_days_simulation, 
                            dt_change_dts_shared= dts_changed, pui_high_census_0 = my_pui_high_census_0,
                            pui_low_census_0 = my_pui_low_census_0, non_pui_high_census_0 = my_non_pui_high_census_0, non_pui_high_census_0 = my_non_pui_low_census_0)
      updated_time <- seq(as.Date("2020/04/01"), by = "day", length.out = num_days_simulation + 1)
    } 
    
    if (!input$hourly_distribution_bool){
      PUI_file <- input$PUI_file$datapath
      nonPUI_file <- input$nonPUI_file$datapath
      pui_high_arrival <- readfiles_pui_high_arrival(PUI_file)
      pui_low_arrival <- readfiles_pui_low_arrival(PUI_file)
      non_pui_low_arrival <- readfiles_nonpui_low_arrival(nonPUI_file)
      non_pui_high_arrival <- readfiles_nonpui_high_arrival(nonPUI_file)
      hourly_dist_pui_high = hourly_distribution_false(pui_high_arrival)
      hourly_dist_pui_low = hourly_distribution_false(pui_low_arrival)
      hourly_dist_nonpui_high = hourly_distribution_false(non_pui_high_arrival)
      hourly_dist_nonpui_low = hourly_distribution_false(non_pui_low_arrival)
      hourly_distribution_total = merge_hourly_distribution(hourly_dist_pui_high, hourly_dist_pui_low, hourly_dist_nonpui_high, hourly_dist_nonpui_low)
      # Mean calcualations
      puihigh_arrival_day_mean = pui_day_mean_calculator(pui_high_arrival)
      puilow_arrival_day_mean = pui_day_mean_calculator(pui_low_arrival)
      historical_nonpui_high = nonpui_day_mean_calculator(non_pui_high_arrival)
      historical_nonpui_low = nonpui_day_mean_calculator(non_pui_low_arrival)
      nonpui_high_arrival_day_mean = historical_nonpui_high * input$potential_decrease_3 * (1 + input$annual_growth_nonPUI)
      nonpui_low_arrival_day_mean = historical_nonpui_low * input$potential_decrease_4 * (1 + input$annual_growth_nonPUI)
      pui_cum_high_0 = input$pui_high_cumulative_0
      pui_cum_low_0 = input$pui_low_cumulative_0
      data = run_simulation(pui_high_cumulative_0 = pui_cum_high_0, pui_low_cumulative_0 = pui_cum_low_0, pui_high_arrival_day_mean = puihigh_arrival_day_mean, pui_low_arrival_day_mean = puilow_arrival_day_mean,
                            non_pui_high_arrival_day_mean = nonpui_high_arrival_day_mean, 
                            non_pui_low_arrival_day_mean = nonpui_low_arrival_day_mean, dt_change_days_shared = days_changed, # e.g. [1, 10, 15]
                            dt_change_dts_shared= dts_changed, n_total_days = num_days_simulation,
                            doubling_time = my_doubling_time, df_input_pui_high_arrival = pui_high_arrival, df_input_pui_low_arrival= pui_low_arrival,
                            df_input_non_pui_high_arrival= non_pui_high_arrival, df_input_non_pui_low_arrival=non_pui_low_arrival,
                            pui_high_census_0 = my_pui_high_census_0,
                            pui_low_census_0 = my_pui_low_census_0, non_pui_high_census_0 = my_non_pui_high_census_0, non_pui_high_census_0 = my_non_pui_low_census_0)
      # updating the time
      added_time <- seq(pui_high_arrival$Date[length(pui_high_arrival$Date)] + 1, by = "day", length.out = num_days_simulation)
      updated_time <- c(pui_high_arrival$Date, added_time)
    }
    
    # finding the admissions by case
    PUI_ICU <- as.data.frame(data[[1]][[4]][,1]) + as.data.frame(data[[2]][[4]][,1])
    PUI_Med <- as.data.frame(data[[1]][[4]][,2]) + as.data.frame(data[[2]][[4]][,2])
    Non_PUI_ICU <- as.data.frame(data[[3]][[4]][,1]) + as.data.frame(data[[4]][[4]][,1])
    Non_PUI_Med <- as.data.frame(data[[3]][[4]][,2]) + as.data.frame(data[[4]][[4]][,2])
    df_admissions_total <- PUI_ICU + PUI_Med + Non_PUI_ICU + Non_PUI_Med
    
    # ADMISSIONS TOTAL
    df_admissions <- cbind(updated_time, PUI_ICU, PUI_Med, Non_PUI_ICU, Non_PUI_Med, df_admissions_total)
    names(df_admissions) <- c("updated_time", "PUI_ICU", "PUI_Med", "Non_PUI_ICU", "Non_PUI_Med", "df_admissions_total")
    plot1 <- ggplot(data = df_admissions, aes(x= updated_time)) + geom_line(aes(y = PUI_ICU, col = "PUI ICU", group = 1)) +
      geom_point(aes(y = PUI_ICU, col = "PUI ICU")) +
      geom_line(aes(y = PUI_Med, col = "PUI Med", group = 1))  + geom_point(aes(y = PUI_Med, col = "PUI Med"))  +
      geom_line(aes(y = Non_PUI_ICU, col = "Non-PUI ICU", group = 1)) + geom_point(aes(y= Non_PUI_ICU, col = "Non-PUI ICU")) +
      geom_line(aes(y = Non_PUI_Med, col = "Non-PUI Med", group = 1)) + geom_point(aes(y = Non_PUI_Med, col = "Non-PUI Med")) +
      geom_line(aes(y = df_admissions_total, col = "Total", group = 1)) + geom_point(aes(y = df_admissions_total, col = "Total")) + ggtitle("IP Admissions per Day by Patient Classification, Adults") + labs(x = "Date", y = NULL)
    
    # ADMISSIONS FOR PUI ONLY
    pui_df_admissions <- cbind(updated_time, PUI_Med, PUI_ICU, PUI_Med+PUI_ICU)
    names(pui_df_admissions) <- cbind("updated_time", "PUI_Med", "PUI_ICU", "daily_arrivals_total")
    plot2 <- ggplot(data = pui_df_admissions, aes(x= updated_time)) + geom_line(aes(y = PUI_Med, col = "PUI Med", group = 1)) +
      geom_point(aes(y = PUI_Med, col = "PUI Med")) +
      geom_line(aes(y = PUI_ICU, col = "PUI ICU", group = 1)) + geom_point(aes(y = PUI_ICU, col = "PUI ICU")) +
      geom_line(aes(y = daily_arrivals_total, col = "Total", group = 1)) + geom_point(aes(y = daily_arrivals_total, col = "Total")) + labs(x = "Date", y = NULL) + ggtitle("IP Admissions per Day by Patient Classification, PUI, Adults")
    
    # ADMISSIONS FOR NON-PUI ONLY
    non_pui_df_admissions <- cbind(updated_time, Non_PUI_Med, Non_PUI_ICU, Non_PUI_Med+Non_PUI_ICU)
    names(non_pui_df_admissions) <- cbind("updated_time", "Non_PUI_Med", "Non_PUI_ICU", "daily_arrivals_total")
    plot3 <- ggplot(data = non_pui_df_admissions, aes(x= updated_time)) + geom_line(aes(y = Non_PUI_Med, col = "Non-PUI Med", group = 1)) +
      geom_point(aes(y = Non_PUI_Med, col = "Non-PUI Med")) +
      geom_line(aes(y = Non_PUI_ICU, col = "Non-PUI ICU", group = 1)) + geom_point(aes(y = Non_PUI_ICU, col = "Non-PUI ICU")) +
      geom_line(aes(y = daily_arrivals_total, col = "Total", group = 1)) + geom_point(aes(y = daily_arrivals_total, col = "Total")) + labs(x = "Date", y = NULL) + ggtitle("IP Admissions per Day by Patient Classification, Non-PUI, Adults")
    
    grid.arrange(plot1, plot2, plot3)
  })
}
shinyApp(ui, server)