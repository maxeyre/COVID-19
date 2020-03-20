# ui.R

library(shiny)
library(dplyr)
library(tidyverse)

# read in Andrew Lilley data
data <- read_csv("https://raw.githubusercontent.com/andrewlilley/tool_COVID-19/master/output_data/country_level.csv?token=ANJMHSDBYB4LQYORA77LET26PTC74")

data <- data %>%
  rename(country = Region, total_cases = Transmissions, total_deaths = Deaths, total_recovered = Recoveries, total_active = Active)

data$date = as.Date(data$date, "%Y-%m-%d")

data <- data[order(data$country),]

# list of countries
data$country <- as.character(data$country)
country.list <- c(unique(data$country))
list.50 <- list()
for (i in 1:length(country.list)){
  list.50[i] <- country.list[i]
}
names(list.50) <- country.list

# read in UK county data
data.county <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vTNL88hteOvWd8ZafF_h0-38Mg1Z3PJdcBkNI3fvORLbuqIlWzc3btPPmD1sB_xqL6jCHcgU1i50ukX/pub?gid=0&single=true&output=csv"
data.county <- read_csv(data.county)
data.county <- gather(data.county, key="date", value="cases",3:ncol(data.county))
data.county$date = as.Date(data.county$date, "%d/%m/%Y")

# get list of counties
data.county$county_UA <- as.character(data.county$county_UA)
county_LA.list <- c(unique(data.county$county_UA))
list.county <- list()
for (i in 1:length(county_LA.list)){
  list.county[i] <- county_LA.list[i]
}
names(list.county) <- county_LA.list

# read in England region data
data.region <- read_csv("https://raw.githubusercontent.com/maxeyre/COVID-19/master/Data%20visualisation/UK%20data/england_region.csv")
data.region <- gather(data.region, key="date", value="cases",3:ncol(data.region))
data.region$date = as.Date(data.region$date, "%d/%m/%Y")
data.region <- data.region %>%
  rename(region = NHSRNm)
# get list of regions
data.region$region <- as.character(data.region$region)
region.list <- c(unique(data.region$region))
list.region <- list()
for (i in 1:length(region.list)){
  list.region[i] <- region.list[i]
}
names(list.region) <- region.list

# Testing data
data.test <- read_csv("https://raw.githubusercontent.com/maxeyre/COVID-19/master/Data%20visualisation/UK%20data/UK_testing.csv")
data.test <- data.test %>%
  select(date, total_tested = tested)
data.test$date = as.Date(data.test$date, "%d/%m/%Y")
data.test$new_tested <- c(NA,diff(data.test$total_tested))
data.test <- gather(data.test, key="type", value="number",2:ncol(data.test))

# Define UI 
shinyUI(
  navbarPage("Navbar",
  # Application title
  # Sidebar with controls to select the variable to plot against mpg
  # and to specify whether outliers should be included
    tabPanel("By country",
             headerPanel("COVID-19 Data Visualisation"),   
             h3("Live epidemic curves by country"),
              h6("Data source: Collected directly from JHU CSSE sources by Andrew Lilley (updated every 24hrs)"),
              h6("Please note: Confirmed case data is entirely dependent on testing rates and will significantly underestimate actual number of infected individuals"),
                sidebarLayout(
                  sidebarPanel(
                    selectInput("country", "Country:",list.50),
                    
                    checkboxGroupInput("checkGroup", "", choices = list("Cases (daily)" = "new_cases", 
                                                  "Cases (total)" = "total_cases", 
                                                  "Deaths (daily)" = "new_deaths",
                                                  "Deaths (total)" = "total_deaths",
                                                  "Recovered (daily)" ="new_recovered",
                                                  "Recovered (total)" = "total_recovered"),
                                       selected = 1),
                    dateRangeInput("dateRange", "Date range",
                               start  = min(data$date),
                               end    = max(data$date), 
                               min    = min(data$date),
                               max    = max(data$date)),
                    radioButtons("log", "y-axis scale:",
                                 choices=c('Linear'="log_no",
                                           'Log'='log_yes'))
                  ),
                mainPanel( # don't want this in the side bar!
                  h3(textOutput("caption")),
                  textOutput("startdate"),
                  plotOutput("countryPlot"),
                  h6("Made by Max Eyre"),
                  h6("Any comments, questions or suggestions please contact via twitter or max.eyre@lstmed.ac.uk"),
                  uiOutput("twitter"),
                  uiOutput("data_source"),
                  uiOutput("data_source_andrew")
                  )
                )
             
    ),
  tabPanel("NHS England regions",
           headerPanel("COVID-19 Data Visualisation"),   
           h3("Live epidemic curves by NHS England regions"),
           h6("Data source: Public Health England (updated every 24hrs)"),
           h6("Please note: Confirmed cases in the UK are now generally individuals presenting at hospitals"),
           sidebarLayout(
             sidebarPanel(
               checkboxGroupInput("checkGroup_region", "", choices = list("Cases (daily)" = "new_cases", 
                                                                          "Cases (total)" = "total_cases"),selected = 2),
               dateRangeInput("dateRange_region", "Date range",
                              start  = min(data.region$date),
                              end    = max(data.region$date), 
                              min    = min(data.region$date),
                              max    = max(data.region$date)),
               radioButtons("pop", "Cases",
                            choices=c('Number of cases'="pop_no",
                                      'Per 100,000 population'='pop_yes'))
             ),
             mainPanel(
               plotOutput("EnglandRegionPlot"),
               h6("Made by Max Eyre"),
               h6("Any comments, questions or suggestions please contact via twitter or max.eyre@lstmed.ac.uk"),
               uiOutput("twitter2"),
               uiOutput("data_source2"),
               h6("Population data source - Office for National Statistics")
             )
           )
  ),
           
    tabPanel("England counties",
           headerPanel("COVID-19 Data Visualisation"),   
           h3("Live epidemic curves for counties of England (Unitary Areas)"),
           h6("Data source: Public Health England (updated every 24hrs)"),
           h6("Please note:"),
           h6("1. PHE data sometimes lose cases between days (hence negative new cases), hopefully this will improve with time."),
           h6("2. Confirmed case data is entirely dependent on testing rates and will significantly underestimate actual number of infected individuals"),
            sidebarLayout(
             sidebarPanel(
               selectInput("county", "County (UA):",list.county),
               checkboxGroupInput("checkGroup_county", "", choices = list("Cases (daily)" = "new_cases", 
                                                                   "Cases (total)" = "total_cases"),selected = 1),
               
               dateRangeInput("dateRange_county", "Date range",
                              start  = min(data.county$date),
                              end    = max(data.county$date), #max(data.confirmed$date)
                              min    = min(data.county$date),
                              max    = max(data.county$date))
               ),
             mainPanel(
               h3(textOutput("caption_county")),
               plotOutput("englandcountyPlot"),
               h6("Made by Max Eyre"),
               h6("Any comments, questions or suggestions please contact via twitter or max.eyre@lstmed.ac.uk"),
               uiOutput("twitter3"),
               uiOutput("data_source3")
               )
           )
           ),
  tabPanel("UK Testing",
           headerPanel("COVID-19 Data Visualisation"),   
           h3("Live diagnostic testing rates for UK"),
           h6("Data source: Public Health England (updated every 24hrs)"),
           sidebarLayout(
             sidebarPanel(
               h4("Testing rates"),
               checkboxGroupInput("checkGroup_test", "", choices = list("Tested (daily)" = "new_tested", 
                                                                          "Tested (total)" = "total_tested"),selected = 1),
               dateRangeInput("dateRange_test", "Date range",
                              start  = min(data.test$date),
                              end    = max(data.test$date), 
                              min    = min(data.test$date),
                              max    = max(data.test$date)),
               radioButtons("log_test", "y-axis scale:",
                            choices=c('Linear'="log_no",
                                      'Log'='log_yes'))
             ),
             mainPanel(plotOutput("UKtestingPlot")
                       )
           ),
           br(),
           sidebarLayout(
             sidebarPanel(
               h4("Proportion positive"),
               checkboxGroupInput("checkGroup_test2", "", choices = list("% positive (daily)" = "new_prop_pos", 
                                                                        "% positive (total)" = "total_prop_pos"),selected = 1),
               dateRangeInput("dateRange_test2", "Date range",
                              start  = min(data.test$date),
                              end    = max(data.test$date), 
                              min    = min(data.test$date),
                              max    = max(data.test$date))
             ),
             mainPanel(plotOutput("UKtestingPlot2"),
                       h6("Made by Max Eyre"),
                       h6("Any comments, questions or suggestions please contact via twitter or max.eyre@lstmed.ac.uk"),
                       uiOutput("twitter4"),
                       uiOutput("data_source4")
             )
           )
           )
  )
  )


