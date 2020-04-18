# LOAD REQUIRED PACKAGES AND FUNCTIONS -----------------------------------------
if (!require("pacman")) install.packages("pacman")
pkgs = c("dplyr") # package names
pacman::p_load(pkgs, character.only = T)
library(tidyverse)

#=======#### READ DATA IN ####=======#
# Confirmed cases
cases <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")

# Deaths
deaths <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")

# Recoveries
recoveries <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv")


#=======#### DATA PROCESSING ####=======#

## Sort and combine into dataframe
cases <- cases %>%
  select(-Lat, -Long) %>% 
  rename(country=`Country/Region`,region=`Province/State`) %>%
  gather(key="date", value="total_cases", -country, -region) %>%
  mutate(date=as.Date(date, "%m/%d/%y"))

deaths <- deaths %>%
  select(-Lat, -Long) %>% 
  rename(country=`Country/Region`,region=`Province/State`) %>%
  gather(key="date", value="total_deaths", -country, -region) %>%
  mutate(date=as.Date(date, "%m/%d/%y"))

recoveries <- recoveries %>%
  select(-Lat, -Long) %>% 
  rename(country=`Country/Region`,region=`Province/State`) %>%
  gather(key="date", value="total_recoveries", -country, -region) %>%
  mutate(date=as.Date(date, "%m/%d/%y"))

# bind together
data <- left_join(cases,deaths,by=c("region","country","date"))
data <- left_join(data,recoveries,by=c("region","country","date"))

## Sort Australia, China and Canada numbers

# Australia
aus<- data %>%
  filter(country=="Australia") %>%
  group_by(country,date) %>%
  summarise(total_cases=sum(total_cases),total_deaths=sum(total_deaths),total_recoveries=sum(total_recoveries))

# China
china <- data %>%
  filter(country=="China") %>%
  group_by(country,date) %>%
  summarise(total_cases=sum(total_cases),total_deaths=sum(total_deaths),total_recoveries=sum(total_recoveries))

# Canada
# list of names
canada.provinces <- c("Alberta","British Columbia","Manitoba","New Brunswick",
                      "Newfoundland and Labrador","Nova Scotia","Ontario","Prince Edward Island",
                      "Quebec","Saskatchewan")

canada <- data %>%
  filter(region %in% canada.provinces) %>%
  group_by(country,date) %>%
  summarise(total_cases=sum(total_cases),total_deaths=sum(total_deaths),total_recoveries=sum(total_recoveries))

# For rest of the countries, just take the region = NA value (i.e. exclude overseas territories)
problem.countries <- c("Australia", "China", "Canada")
`%notin%` <- Negate(`%in%`)
data <- data %>%
  filter(country %notin% problem.countries, is.na(region)==TRUE) %>%
  select(-region)

data <- bind_rows(data,aus,china,canada)
data$country[data$country=="US"] <- "United States"
data$country[data$country=="Taiwan*"] <- "Taiwan"
data$country[data$country=="Korea, South"] <- "South Korea"
data$country[data$country=="Saint Kitts and Nevis"] <- "Saint Kitts & Nevis"
data$country[data$country=="Sao Tome and Principe"] <- "Sao Tome & Principe"
data$country[data$country=="Burma"] <- "Myanmar"

data <- data[order(data$country),]

# Calculate new cases, deaths and recoveries
# calculate new daily cases, deaths, recoveries
data$new_cases <- c()
data$new_deaths <- c()
data$new_recoveries <- c()

uni.country <- unique(data$country)
out.cases <- c()
out.deaths <- c()
out.recoveries <- c()

for (i in 1:length(uni.country)){
  x <- data[data$country==uni.country[i],]
  out.cases <- c(out.cases,0,diff(x$total_cases))
  out.deaths <- c(out.deaths,0,diff(x$total_deaths))
  out.recoveries <- c(out.recoveries,0,diff(x$total_recoveries))
}
data$new_cases <- out.cases
data$new_deaths <- out.deaths
data$new_recoveries <- out.recoveries
data <- gather(data, key="type", value="number",-country,-date)



#=======#### BY POPULATION & COUNTRIES WITH >=100 CASES & >=5 DEATHS####=======#
country.pop.data <- read_csv("https://raw.githubusercontent.com/maxeyre/COVID-19/master/Data%20visualisation/Other/country_pop.csv")
data <- left_join(data,country.pop.data, by="country")
data$number_pop <- 100000*data$number/data$pop

### SORT UK
UK.JHU <- data[data$country=="United Kingdom",]
UK.JHU2 <- UK.JHU[UK.JHU$date>"2020-04-13",]
UK.JHU2 <- UK.JHU2[UK.JHU2$type %in% c("new_cases","total_cases","new_deaths","total_deaths"),]
UK.JHU_recov <- UK.JHU[UK.JHU$type %in% c("new_recoveries","total_recoveries"),]
UK.JHU_recov$number <- NA
UK.JHU_recov$number_pop <- NA

UK.correct <- read_csv("https://raw.githubusercontent.com/maxeyre/COVID-19/master/data_scraper/data/original/UK_total.csv")
UK.correct$country <- "United Kingdom"
UK.correct$pop <- 67886011
UK.correct$number_pop <- UK.correct$number/UK.correct$pop

UK.data<- bind_rows(UK.JHU2,UK.correct, UK.JHU_recov)

data <- data[data$country!="United Kingdom",]
data <- bind_rows(data, UK.data)

data <- data[order(data$country),]

# list of countries with >=100 cases
data.100 <- data[data$type=="total_cases",]
data.100 <- data.100[data.100$number>=100,]
uni.country.100 <- c(unique(data.100$country))
data.100.out <- NULL
date.100 <- c(as.Date("2020-01-01","%Y-%m-%d"))
for (i in 1:length(uni.country.100)){
  x <- data.100[data.100$country==uni.country.100[i],]
  out <- as.Date(x$date[which(x$number>=100)],"%Y-%m-%d")
  out_pop <- as.Date(x$date[which(x$number_pop>=1)],"%Y-%m-%d")
  out <- min(out)
  x$date_rel <- x$date - out
  if(length(out_pop)==0){
    x$date_rel_pop <- c(rep(NA, length(x$date)))
  } else{
    out_pop <- min(out_pop)
    x$date_rel_pop <- x$date - out_pop
  }
  x <- x[x$date_rel>=0,]
  data.100.out <- rbind(data.100.out, x)
}
data.100 <- data.100.out

# relative dates - deaths
data.deaths10 <- data[data$type=="total_deaths",]
data.deaths10 <- data.deaths10[data.deaths10$number>=5,]
uni.country.deaths10 <- c(unique(data.deaths10$country))
data.deaths10.out <- NULL
date.deaths10 <- c(as.Date("2020-01-01","%Y-%m-%d"))
for (i in 1:length(uni.country.deaths10)){
  x <- data.deaths10[data.deaths10$country==uni.country.deaths10[i],]
  out <- as.Date(x$date[which(x$number>=10)],"%Y-%m-%d")
  out_pop <- as.Date(x$date[which(x$number_pop>=0.5)],"%Y-%m-%d")
  out <- min(out)
  x$date_rel <- x$date - out
  if(length(out_pop)==0){
    x$date_rel_pop <- c(rep(NA, length(x$date)))
  } else{
    out_pop <- min(out_pop)
    x$date_rel_pop <- x$date - out_pop
  }
  x <- x[x$date_rel>=0,]
  data.deaths10.out <- rbind(data.deaths10.out, x)
}
data.deaths10 <- data.deaths10.out
data.deaths10$date_rel <- as.numeric(data.deaths10$date_rel)
data.deaths10$date_rel_pop <- as.numeric(data.deaths10$date_rel_pop)

# 
# # SAVE OUTPUT ------------------------------------------------------------------
readr::write_csv(data, 
                  paste0("data/processed/JHU_full.csv"),append=FALSE)
readr::write_csv(data.100, 
                 paste0("data/processed/JHU_100-cases.csv"),append=FALSE)
readr::write_csv(data.deaths10, 
                 paste0("data/processed/JHU_5-deaths.csv"),append=FALSE)


# DELETE OLD VERSIOSN ----------------------------------------------------------
# case_data_files <- list.files("data/processed/", 
# 															pattern = ".csv",
# 															full.names = T)
# to_delete <- case_data_files[!grepl(last_day, case_data_files)]
# file.remove(to_delete)