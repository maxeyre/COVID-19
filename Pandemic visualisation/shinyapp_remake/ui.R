# ui.R

library(shiny)
library(dplyr)
library(tidyverse)

# read in global data (from World in Data) - no longer using
# data <- "http://cowid.netlify.com/data/full_data.csv"
# data <- read_csv(data)
# data[is.na(data)] <- 0
# 
# data$date = as.Date(data$date, "%Y-%m-%d")
# 
# data <- data %>%
#   rename(country=location)

# read in Andrew Lilley data
data <- read_csv("https://raw.githubusercontent.com/andrewlilley/tool_COVID-19/master/output_data/country_level.csv?token=ANJMHSDBYB4LQYORA77LET26PTC74")

data <- data %>%
  rename(country = Region, total_cases = Transmissions, total_deaths = Deaths, total_recovered = Recoveries, total_active = Active)

data$date = as.Date(data$date, "%Y-%m-%d")

data <- data[order(data$country),]

# list of countries
data$country <- as.character(data$country)

country.list <- c(unique(data$country))
#country.list <- c(country.list[(1:(which(country.list=="Others"))-1)],country.list[(which(country.list=="Others")+1):length(country.list)])


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

# get list of countries with more than 50 cases
data.county$county_UA <- as.character(data.county$county_UA)
county_LA.list <- c(unique(data.county$county_UA))
list.county <- list()
for (i in 1:length(county_LA.list)){
  list.county[i] <- county_LA.list[i]
}
names(list.county) <- county_LA.list

# Define UI 
shinyUI(
  navbarPage("Navbar",
  # Application title
  # Sidebar with controls to select the variable to plot against mpg
  # and to specify whether outliers should be included
    tabPanel("By country",
             headerPanel("COVID-19 Epidemic Curves"),   
             h3("Live epidemic curves"),
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
                  plotOutput("countryPlot")
                  )
                )
             
    ),
             
    #tabPanel("Country comparisons"),
    tabPanel("UK counties",
           headerPanel("COVID-19 Epidemic Curves"),   
           h3("Live epidemic curves for counties of England (Unitary Areas)"),
           h6("Data source: Public Health England (updated every 24hrs)"),
           h6("Please note: Confirmed case data is entirely dependent on testing rates and will significantly underestimate actual number of infected individuals"),
            sidebarLayout(
             sidebarPanel(
               selectInput("county", "County (UA):",list.county),
               checkboxGroupInput("checkGroup_county", "", choices = list("Cases (daily)" = "new_cases", 
                                                                   "Cases (total)" = "total_cases"),selected = 1),
               dateRangeInput("dateRange", "Date range",
                              start  = min(data.county$date),
                              end    = max(data.county$date), #max(data.confirmed$date)
                              min    = min(data.county$date),
                              max    = max(data.county$date))
               ),
             mainPanel(
               h3(textOutput("caption_county")),
               textOutput("startdate_county"),
               plotOutput("countyPlot"))
           )
           )
  )
)

#h6("Made by Max Eyre"),
#h6("Any comments, questions or suggestions please contact via twitter or max.eyre@lstmed.ac.uk"),
#uiOutput("twitter"),
#uiOutput("data_source")
