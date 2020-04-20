# COVID-19 data visualisation
## This repository contains: 
- Scripts for an interactive R Shiny app for visualising the COVID-19 pandemic
- Scripts for daily data scraping from Johns Hopkins CSSE and Public Health England data sources (each day at 6pm and 9pm BST time)
- Daily updated data files for global COVID-19 cases
- Daily updated data files for UK COVID-19 cases (broken down into multiple .csv files)
 
 ## App 
 Please visit https://maxeyre.shinyapps.io/shinyapp/ to use the app.
 
 The following viewing features are currently available:
#### Global 
  - View time series data for cumulative and daily figures of confirmed cases, deaths and recoveries for all countries reporting at least one confirmed case (can view per 100,000 population)
  - Compare time series data for cumulative and daily figures of confirmed cases, deaths and recoveries of multiple countries starting from the day when: 100 cases/5 deaths/1 case per 100,000/0.5 deaths per 100,000 were reached
#### UK
  - View time series data for cumulative and daily figures of confirmed deaths for the UK by each country: England, Wales, Northern Ireland and Scotland (can view per 100,000 population)
  - View time series data for cumulative and daily figures of confirmed cases for each NHS England region (can view per 100,000 population)
  - View time series data for cumulative and daily figures of confirmed cases for each UTLA - Local Authority in England (can view per 100,000 population)
  - View time series data for cumulative and daily figures of number of people tested for COVID-19 in the UK and the proportion of these individuals who were confirmed as being positive.
#### Brazil
- View time series data by Brazilian state (data source: [Wesley Cota](https://github.com/wcota/covid19br))
 
 ## Data
 Please feel free to use any data in this public repository. You will find the daily scraped data in tidy form in [this folder](https://github.com/maxeyre/COVID-19/tree/master/data_scraper/data/processed) as follows:
 #### Johns Hopkins CSSE data
 - [Tidied full version](https://github.com/maxeyre/COVID-19/blob/master/data_scraper/data/processed/JHU_full.csv)
 - [For countries with over 100 confirmed cases (dates relative to the date they reach this number)](https://github.com/maxeyre/COVID-19/blob/master/data_scraper/data/processed/JHU_100-cases.csv)
 - [For countries with over 5 confirmed deaths (dates relative to the date they reach this number)](https://github.com/maxeyre/COVID-19/blob/master/data_scraper/data/processed/JHU_5-deaths.csv)
 #### Public Health England data
 - [Tidy full UK dataset](https://github.com/maxeyre/COVID-19/blob/master/data_scraper/data/processed/UK.csv)

 
