# LOAD REQUIRED PACKAGES AND FUNCTIONS -----------------------------------------
if (!require("pacman")) install.packages("pacman")
pkgs = c("dplyr") # package names
pacman::p_load(pkgs, character.only = T)

library(tidyverse)

#=======#### DOWNLOAD DATA FROM PHE ####=======#

# Download file and load it in R
UK.cases <- read_csv("https://coronavirus.data.gov.uk/downloads/csv/coronavirus-cases_latest.csv")
#UK.deaths <- read_csv("https://coronavirus.data.gov.uk/downloads/csv/coronavirus-deaths_latest.csv")

write_csv(UK.cases, "data/original/coronavirus-cases.csv")
#write_csv(UK.deaths, "data/original/coronavirus-deaths.csv")

UK.cases <- UK.cases %>%
  select(area=`Area name`,id = `Area code`, division=`Area type`,date=`Specimen date`,new_cases=`Daily lab-confirmed cases`,total_cases=`Cumulative lab-confirmed cases`) %>%
  gather(key="type", value="number", -area, -id, -division, -date)

#UK.deaths <- UK.deaths %>%
  #select(area=`Area name`,id = `Area code`, division=`Area type`,date=`Reporting date`,new_deaths=`Daily change in deaths`,total_deaths=`Cumulative deaths`) %>%
  #gather(key="type", value="number", -area, -id, -division, -date)

UK <- UK.cases
UK$division[UK$division=="utla"] <- "UTLA"
UK$division[UK$division=="region"] <- "Region"
UK$division[UK$division=="nation"] <- "Nation"
	
#=======#### DATA PROCESSING ####=======#
# Just adding population estimates to each entry
pop_UTLA <- read_csv("https://raw.githubusercontent.com/maxeyre/COVID-19/master/data_scraper/data/original/area_populations.csv")
pop_country <- data.frame(country=c("England","Scotland","Wales","Northern Ireland"), population=c(55980000,5438000,3139000,1882000))
pop_NHSregion <- read_csv("https://raw.githubusercontent.com/maxeyre/COVID-19/master/data_scraper/data/original/NHS_england_regions_pop.csv") %>%
  rename(population=pop)

UK.UTLA <- UK[UK$division=="UTLA",]
UK.NHSregion <- UK[UK$division=="Region",]
UK.country <- UK[UK$division=="Nation",]
UK.country$division[UK.country$division=="Nation"] <- "Country"
#UK.UK <- UK[UK$division=="UK",]

UK.UTLA <- left_join(UK.UTLA, pop_UTLA, by=c("id"="area_code"))
UK.NHSregion<- left_join(UK.NHSregion, pop_NHSregion, by=c("area"="region"))
UK.country<- left_join(UK.country, pop_country, by=c("area"="country"))
#UK.UK$population <- 66650000

UK <- bind_rows(UK.UTLA,UK.NHSregion,UK.country)
UK$number_pop <- 100000*UK$number/UK$population

#=======#### DATA OUTPUTTING ####=======#
readr::write_csv(UK, "data/processed/UK.csv")
