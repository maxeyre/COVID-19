# ui.R

library(shiny)
library(dplyr)
library(tidyverse)

# read in global data
data <- read_csv("https://raw.githubusercontent.com/maxeyre/COVID-19/master/data_scraper/data/processed/JHU_full.csv")
data.100 <- read_csv("https://raw.githubusercontent.com/maxeyre/COVID-19/master/data_scraper/data/processed/JHU_100-cases.csv")
data.deaths10 <- read_csv("https://raw.githubusercontent.com/maxeyre/COVID-19/master/data_scraper/data/processed/JHU_5-deaths.csv")

# list of countries
country.list <- c(unique(data$country))
list.50 <- list()
for (i in 1:length(country.list)){
  list.50[i] <- country.list[i]
}
names(list.50) <- country.list

# list of countries with >100 cases
country.list.100 <- c(unique(data.100$country))
list.100 <- list()
for (i in 1:length(country.list.100)){
  list.100[i] <- country.list.100[i]
}
names(list.100) <- country.list.100

# UK data
UK.data <- read_csv("https://raw.githubusercontent.com/maxeyre/COVID-19/master/data_scraper/data/processed/UK_total.csv")

# UK breakdown data
UK_by_country <- read_csv("https://raw.githubusercontent.com/maxeyre/COVID-19/master/data_scraper/data/processed/UK_by_country.csv")

# UK county data
# read in UK county data
data.county <- read_csv("https://raw.githubusercontent.com/maxeyre/COVID-19/master/data_scraper/data/processed/england_UTLA.csv")

# get list of counties
data.county$county_UA <- as.character(data.county$county_UA)
county_LA.list <- c(unique(data.county$county_UA))
list.county <- list()
for (i in 1:length(county_LA.list)){
  list.county[i] <- county_LA.list[i]
}
names(list.county) <- county_LA.list

# read in England region data
data.region <- read_csv("https://raw.githubusercontent.com/maxeyre/COVID-19/master/data_scraper/data/processed/NHS_england_regions.csv")
data.region.pop <- read_csv("https://raw.githubusercontent.com/maxeyre/COVID-19/master/data_scraper/data/processed/NHS_england_regions_pop.csv")

# Testing data
data.test <- read_csv("https://raw.githubusercontent.com/maxeyre/COVID-19/master/Data%20visualisation/UK%20data/UK_testing.csv")
data.test <- data.test %>%
  select(date, total_tested = tested)
data.test$date = as.Date(data.test$date, "%d/%m/%Y")
data.test$new_tested <- c(NA,diff(data.test$total_tested))

# code to deal with mismatch in lengths for testing and UK data
if(length(UK.data$number[UK.data$type=="total_cases" & UK.data$date>="2020-03-17"]) < nrow(data.test)){
  data.test <- data.test[1:(length(UK.data$number[UK.data$type=="total_cases" & UK.data$date>="2020-03-17"])),]
}

if(length(UK.data$number[UK.data$type=="total_cases" & UK.data$date>="2020-03-17"]) > nrow(data.test)){
  x <- length(UK.data$number[UK.data$type=="total_cases" & UK.data$date>="2020-03-17"]) - nrow(data.test)
  data.test[((nrow(data.test)+1):length(UK.data$number[UK.data$type=="total_cases" & UK.data$date>="2020-03-17"])),] <- NA
  for (i in (nrow(data.test)-x+1):nrow(data.test)){
    data.test[i,1]<- data.test[(i-1),1] + 1
  }
}

data.test$total_cases <- UK.data$number[UK.data$type=="total_cases" & UK.data$date>="2020-03-17"]
data.test$new_cases <- UK.data$number[UK.data$type=="new_cases" & UK.data$date>="2020-03-17"]

data.test$total_prop_pos <- 100*data.test$total_cases/data.test$total_tested
data.test$new_prop_pos <- 100*data.test$new_cases/data.test$new_tested

data.test <- data.test %>%
  gather(key="type", value="number",-date)

# Brazil data
data.brazil <- read_csv("")

# Define UI 
shinyUI(fluidPage(
  headerPanel("COVID-19 Data Visualisation"),
  navlistPanel(widths=c(2,9),
  # Application title
  # Sidebar with controls to select the variable to plot against mpg
  # and to specify whether outliers should be included
    "Worldwide",  
    tabPanel("By country",
             h5("Please use the menu bar on the left to navigate to different sections"),
             
             h3("Live epidemic curves by country"),
              h6("Data source: Automatically collected from JHU CSSE (updated every 24hrs)"),
              h6("Please note: Confirmed case data is entirely dependent on testing rates and will significantly underestimate actual number of infected individuals"),
                sidebarLayout(
                  sidebarPanel(
                    selectInput("country", "Country:",list.50),
                    
                    checkboxGroupInput("checkGroup", "", choices = list("Cases (daily)" = "new_cases", 
                                                  "Cases (total)" = "total_cases", 
                                                  "Deaths (daily)" = "new_deaths",
                                                  "Deaths (total)" = "total_deaths",
                                                  "Recoveries (daily)" = "new_recoveries",
                                                  "Recoveries (total)" = "total_recoveries"),
                                       selected = 1),
                    dateRangeInput("dateRange", "Date range",
                               start  = min(data$date),
                               end    = max(data$date), 
                               min    = min(data$date),
                               max    = max(data$date)),
                    radioButtons("log", "y-axis scale:",
                                 choices=c('Linear'="log_no",
                                           'Log'='log_yes')),
                    radioButtons("pop_country", "y-axis value",
                                 choices=c('Number of cases'="pop_no",
                                           'Per 100,000 population'='pop_yes'))
                  ),
                mainPanel( 
                  h3(textOutput("caption")),
                  textOutput("startdate"),
                  plotOutput("countryPlot"),
                  h6("Made by Max Eyre"),
                  h6("Any comments, questions or suggestions please contact via twitter or max.eyre@lstmed.ac.uk"),
                  uiOutput("twitter"),
                  uiOutput("git1"),
                  uiOutput("data_source"),
                  h6("Population data source - United Nations Population Division estimates (2019)"),
                  h5(textOutput("counter"))
                  )
                ),
    ),
  tabPanel("Country comparison",
           h5("Please use the menu bar on the left to navigate to different sections"),    
           h3("Live comparison of countries from beginning of outbreaks"),
               h6("Data source: Automatically collected from JHU CSSE (updated every 24hrs)"),
               h6("Please note: Confirmed case data is entirely dependent on testing rates and will significantly underestimate actual number of infected individuals"),
             sidebarLayout(
               sidebarPanel(
                 radioButtons("compare_by", "Compare by",
                              choices=c('Cases'="cases",
                                        'Deaths'='deaths')),
                 sliderInput("dateRange.100", "Number of days into outbreak (range)", 
                             min = min(data.100$date_rel), 
                             max = max(data.100$date_rel), value = c(min(data.100$date_rel), (max(data.100$date_rel)-20))),
                 radioButtons("log_compare", "y-axis scale:",
                              choices=c('Linear'="log_no",
                                        'Log'='log_yes')),
                 radioButtons("compare_pop", "y-axis value",
                              choices=c('Number of cases'="pop_no",
                                        'Per 100,000 population'='pop_yes')),
                 
                 checkboxGroupInput("checkGroup_countryCompare", "Countries", choices = list.100, selected = 1)
               ), 
               mainPanel(h5("Explanation:"), 
                         h6("Only available for countries with at least 100 cases or 5 deaths (some countries only have one or the other and will only show for cases or deaths)"),
                         h6("Cases: day 0 is the first day >100 cases were reported (or 1 per 100,000 if viewing by pop. rate)"),
                         h6("Deaths: day 0 is the first day >10 deaths were reported (or 0.5 per 100,000 if viewing by pop. rate)"),
                         plotOutput("countryPlot_compare"),
                         h6("Made by Max Eyre"),
                         h6("Any comments, questions or suggestions please contact via twitter or max.eyre@lstmed.ac.uk"),
                         uiOutput("twitter_comp"),
                         uiOutput("git2"),
                         uiOutput("data_source_comp"),
                         h6("Population data source - United Nations Population Division estimates (2019)")
                )
             ),
  ),
  "United Kingdom",
  tabPanel("UK overview",
           h5("Please use the menu bar on the left to navigate to different sections"),
           h3("UK Overview"),
           h6("Data source: Public Health England (updated every 24hrs)"),
           h6("Please note: Confirmed cases in the UK are now generally individuals presenting at hospitals and deaths can be reported with delays"),
                         sidebarPanel(
                           checkboxGroupInput("checkGroup_UK", "", choices = list("Cases (daily)" = "new_cases", 
                                                                                  "Cases (total)" = "total_cases",
                                                                                  "Deaths (daily)" = "new_deaths",
                                                                                  "Deaths (total)" = "total_deaths"),selected = 2),
                           dateRangeInput("dateRange_UK", "Date range",
                                          start  = min(UK.data$date),
                                          end    = max(UK.data$date), 
                                          min    = min(UK.data$date),
                                          max    = max(UK.data$date)),
                           radioButtons("log_UK", "y-axis scale:",
                                        choices=c('Linear'="log_no",
                                                  'Log'='log_yes')),
                           radioButtons("pop_UK", "Cases",
                                        choices=c('Number of cases'="pop_no",
                                                  'Per 100,000 population'='pop_yes'))
                         ),
                        mainPanel(tabsetPanel(type = "tabs", id="tabs_UK",
                           tabPanel("UK - total", value = 1,
                                    h3("Live epidemic curve of UK"),
                                    plotOutput("UKPlot")
                                    ),
                           tabPanel("UK - countries", value = 2,
                                    h3("Live epidemic curve of UK by country"),
                                    h6("Death data only available from 27/03/2020"),
                                    plotOutput("UKPlot_by_country")
                                    )
                          ),
                          h6("Made by Max Eyre"),
                          h6("Any comments, questions or suggestions please contact via twitter or max.eyre@lstmed.ac.uk"),
                          uiOutput("twitter_UK"),
                          uiOutput("git3"),
                          uiOutput("data_source_UK"),
                          h6("Population data source - Office for National Statistics")
                       ),
               
             ),
           
  tabPanel("NHS England regions",
           h5("Please use the menu bar on the left to navigate to different sections"),
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
                                      'Per 100,000 population'='pop_yes')),
               radioButtons("log_region", "y-axis scale:",
                            choices=c('Linear'="log_no",
                                      'Log'='log_yes'))
             ),
             mainPanel(
               plotOutput("EnglandRegionPlot"),
               h6("Made by Max Eyre"),
               h6("Any comments, questions or suggestions please contact via twitter or max.eyre@lstmed.ac.uk"),
               uiOutput("twitter2"),
               uiOutput("git4"),
               uiOutput("data_source2"),
               h6("Population data source - Office for National Statistics")
             )
           )
  ),
    tabPanel("England - Local authorities",
             h5("Please use the menu bar on the left to navigate to different sections"),
           h3("Live epidemic curves for Local Authorities of England"),
           h6("Data source: Public Health England (updated every 24hrs)"),
           h6("Please note: Confirmed case data is entirely dependent on testing rates and will significantly underestimate actual number of infected individuals"),
            sidebarLayout(
             sidebarPanel(
               selectInput("county", "County (UA):",list.county),
               checkboxGroupInput("checkGroup_county", "", choices = list("Cases (daily)" = "new_cases", 
                                                                   "Cases (total)" = "total_cases"),selected = 1),
               
               dateRangeInput("dateRange_county", "Date range",
                              start  = min(data.county$date),
                              end    = max(data.county$date),
                              min    = min(data.county$date),
                              max    = max(data.county$date))
               ),
             mainPanel(
               h5(textOutput("county_newcase_update")),
               h5(textOutput("county_totalcase_update")),
               h3(textOutput("caption_county")),
               plotOutput("englandcountyPlot"),
               h6("Made by Max Eyre"),
               h6("Any comments, questions or suggestions please contact via twitter or max.eyre@lstmed.ac.uk"),
               uiOutput("twitter3"),
               uiOutput("git5"),
               uiOutput("data_source3")
               )
           )
           ),
  tabPanel("UK Testing",
           h5("Please use the menu bar on the left to navigate to different sections"),
           h3("Live diagnostic testing rates for UK"),
           h6("Data source: Public Health England (updated every 24hrs)"),
           sidebarLayout(
             sidebarPanel(
               h4("Testing rates"),
               h6("This is the number of individuals tested (not the number of tests carried out)"),
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
               h4("Proportion of tested individuals positive"),
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
                       uiOutput("git6"),
                       uiOutput("data_source4")
             )
           )
           )
  )
)
)
  


