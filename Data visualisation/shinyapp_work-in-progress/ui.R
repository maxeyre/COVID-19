# ui.R

library(shiny)
library(dplyr)
library(tidyverse)

# read in Andrew Lilley data
data <- read_csv("https://raw.githubusercontent.com/andrewlilley/tool_COVID-19/master/output_data/country_level.csv?token=ANJMHSDBYB4LQYORA77LET26PTC74")

data <- data %>%
  rename(country = Region, total_cases = Transmissions, total_deaths = Deaths)

data$date = as.Date(data$date, "%Y-%m-%d")

data$country[data$country=="US"]<-"United States"
data$country[data$country=="UK"]<-"United Kingdom"
data$country[data$country=="UAE"]<-"United Arab Emirates"

data <- data[order(data$country),]

# list of countries
data$country <- as.character(data$country)
country.list <- c(unique(data$country))
list.50 <- list()
for (i in 1:length(country.list)){
  list.50[i] <- country.list[i]
}
names(list.50) <- country.list

# list of countries with >100 cases
data.100 <- data[data$total_cases>=100,]
country.list.100 <- c(unique(data.100$country))
list.100 <- list()
for (i in 1:length(country.list.100)){
  list.100[i] <- country.list.100[i]
}
names(list.100) <- country.list.100

data <- gather(data, key="type", value="number",4:ncol(data))

# relative dates
data.100 <- data[data$type=="total_cases",]
data.100 <- data.100[data.100$number>=100,]
uni.country.100 <- c(unique(data.100$country))
data.100.out <- NULL
date.100 <- c(as.Date("2020-01-01","%Y-%m-%d"))
for (i in 1:length(uni.country.100)){
  x <- data.100[data.100$country==uni.country.100[i],]
  out <- as.Date(x$date[which(x$number>=100)],"%Y-%m-%d")
  out <- min(out)
  x$date_rel <- x$date - out
  x <- x[x$date_rel>=0,]
  data.100.out <- rbind(data.100.out, x)
}
data.100 <- data.100.out
data.100$date_rel <- as.numeric(data.100$date_rel)

# read in UK county data
data.county <- "https://raw.githubusercontent.com/maxeyre/COVID-19/master/Data%20visualisation/UK%20data/england_countyUA.csv"
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
shinyUI(fluidPage(
  headerPanel("COVID-19 Data Visualisation - being updated (complete at 11:00 GMT)"),
)
)
  


