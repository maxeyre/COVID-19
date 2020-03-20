# ui.R

library(shiny)
library(dplyr)
library(tidyverse)

# read in data
data.county <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vTNL88hteOvWd8ZafF_h0-38Mg1Z3PJdcBkNI3fvORLbuqIlWzc3btPPmD1sB_xqL6jCHcgU1i50ukX/pub?gid=0&single=true&output=csv"
data.county <- read_csv(data.county)

data.county <- gather(data.county, key="date", value="cases",3:ncol(data.county))

data.county$date = as.Date(data.county$date, "%d/%m/%Y")

# get list of countries with more than 50 cases

data.county$county_UA <- as.character(data.county$county_UA)

county_LA.list <- c(unique(data.county$county_UA))


list.50 <- list()

for (i in 1:length(county_LA.list)){
  list.50[i] <- county_LA.list[i]
}
names(list.50) <- county_LA.list


# Define UI 
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("COVID-19 Epidemic Curves"),
  # Sidebar with controls to select the variable to plot against mpg
  # and to specify whether outliers should be included
  sidebarPanel(
    checkboxGroupInput("county_LA", "County Unitary Authority:",
                list.50),
    checkboxGroupInput("checkGroup", "",
                       choices = list("Cases (daily)" = "new_cases", 
                                      "Cases (total)" = "total_cases"),
                                      #"Recovered (daily)" ="new_recovered",
                                      #"Recovered (total)" = "recovered"),
                       selected = 1),
    dateRangeInput("dateRange", "Date range",
                  start  = min(data.county$date),
                  end    = max(data.county$date), #max(data.confirmed$date)
                  min    = min(data.county$date),
                  max    = max(data.county$date)) #max(data.confirmed$date)
  ),
  
  mainPanel(
    h3("Live epidemic curves for England's County Unitary Authorities"),
    h6("Data source: Public Health England"),
    h6(""),

    h3(textOutput("caption")),
    textOutput("startdate"),
    plotOutput("countryPlot"),
    h6("Made by Max Eyre"),
    h6("Any comments, questions or suggestions please contact via twitter or max.eyre@lstmed.ac.uk"),
    uiOutput("twitter"),
    uiOutput("data_source")
  )
))
