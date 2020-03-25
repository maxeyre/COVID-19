# server.R
library(shiny)
library(datasets)
library(dplyr)
library(readr)
library(tidyverse)
library(grid)
library(ggplot2)
library(gridExtra)
library(gtable)

# read in global data
data <- read_csv("https://raw.githubusercontent.com/andrewlilley/tool_COVID-19/master/output_data/country_level.csv?token=ANJMHSDBYB4LQYORA77LET26PTC74")

data <- data %>%
  select(Region, date, Transmissions, Deaths) %>%
  rename(country = Region, total_cases = Transmissions, total_deaths = Deaths)

data$country[data$country=="US"]<-"United States"
data$country[data$country=="UK"]<-"United Kingdom"
data$country[data$country=="UAE"]<-"United Arab Emirates"

data$date = as.Date(data$date, "%Y-%m-%d")

data <- data[order(data$country),]




# calculate new daily cases, deaths, recoveries
data$new_cases <- c()
data$new_deaths <- c()

uni.country <- unique(data$country)
out.cases <- c()
out.deaths <- c()

for (i in 1:length(uni.country)){
  x <- data[data$country==uni.country[i],]
  out.cases <- c(out.cases,0,diff(x$total_cases))
  out.deaths <- c(out.deaths,0,diff(x$total_deaths))

}

data$new_cases <- out.cases
data$new_deaths <- out.deaths

# read in country population data
country.pop.data <- read_csv("https://raw.githubusercontent.com/maxeyre/COVID-19/master/Data%20visualisation/Other/country_pop.csv")
data <- left_join(data,country.pop.data, by="country")
data <- data %>%
  mutate(total_cases = 100000*total_cases/pop, new_cases=100000*new_cases/pop)

data <- gather(data, key="type", value="number",3:ncol(data))

# list of countries with >=100 cases
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

# UK county data
# read in UK county data
data.county <- "https://raw.githubusercontent.com/maxeyre/COVID-19/master/Data%20visualisation/UK%20data/england_countyUA.csv"
data.county <- read_csv(data.county)
data.county <- gather(data.county, key="date", value="total_cases",3:ncol(data.county))
data.county$date = as.Date(data.county$date, "%d/%m/%Y")

data.county <- data.county[order(data.county$county_UA),]

# calculate new daily cases
data.county$new_case <- c()
uni.county <- unique(data.county$county_UA)
out <- c()
for (i in 1:length(uni.county)){
  x <- data.county[data.county$county_UA==uni.county[i],]
  out <- c(out,0,diff(x$total_cases))
}
data.county$new_cases <- out
#data.county$new_cases[data.county$new_cases<0]<- 0
data.county <- gather(data.county,key="type",value="number",4:ncol(data.county))

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
  rename(region = NHSRNm, total_cases =cases)
data.region <- data.region[order(data.region$region),]
# get list of regions
data.region$region <- as.character(data.region$region)
region.list <- c(unique(data.region$region))
list.region <- list()
for (i in 1:length(region.list)){
  list.region[i] <- region.list[i]
}
names(list.region) <- region.list
# calculate new daily cases
data.region$new_case <- c()
uni.region <- unique(data.region$region)
out <- c()
for (i in 1:length(uni.region)){
  x <- data.region[data.region$region==uni.region[i],]
  out <- c(out,0,diff(x$total_cases))
}
data.region$new_cases <- out
# get per 100,000 population results
region.pop.data <- read_csv("https://raw.githubusercontent.com/maxeyre/COVID-19/master/Data%20visualisation/UK%20data/NHS_england_regions_pop.csv")
data.region.pop <- left_join(data.region,region.pop.data, by="region")
data.region.pop <- data.region.pop %>%
  mutate(total_cases = 100000*total_cases/pop, new_cases=100000*new_cases/pop)

data.region <- gather(data.region,key="type",value="number",4:ncol(data.region))
data.region.pop <- gather(data.region.pop,key="type",value="number",4:(ncol(data.region.pop)-1))

# Testing data
data.test <- read_csv("https://raw.githubusercontent.com/maxeyre/COVID-19/master/Data%20visualisation/UK%20data/UK_testing.csv")
data.test <- data.test %>%
  select(date, total_tested = tested, total_cases=cases, new_cases)
data.test$date = as.Date(data.test$date, "%d/%m/%Y")
data.test$new_tested <- c(NA,diff(data.test$total_tested))
data.test$total_prop_pos <- 100*data.test$total_cases/data.test$total_tested
data.test$new_prop_pos <- 100*data.test$new_cases/data.test$new_tested

data.test <- gather(data.test, key="type", value="number",2:ncol(data.test))

# UK data


# Define server logic required to plot various variables against mpg
shinyServer(function(input, output) {
  
  # Compute the forumla text in a reactive expression since it is 
  # shared by the output$caption and output$mpgPlot expressions
  formulaText <- reactive({
    paste(input$country)
  })
  
  formulaText_county <- reactive({
    paste(input$county)
  })
  
  output$startdate <- renderText({
    paste("Date range: ",as.character(input$dateRange[1])," to ",as.character(input$dateRange[2]),sep="")
  })
  
  # Return the formula text for printing as a caption
  output$caption <- renderText({
    formulaText()
  })
  
  output$caption_county <- renderText({
    formulaText_county()
  })
  
  red <- data.county[data.county$date == max(data.county$date) & data.county$type == "new_cases",]
  red <- red[order(red$number,decreasing=TRUE),]
  
  red2 <- data.county[data.county$date == max(data.county$date) & data.county$type == "total_cases",]
  red2 <- red2[order(red2$number,decreasing=TRUE),]
  
  output$county_newcase_update <- renderText({
    paste("Top 5 highest new daily cases: ", as.character(red$county_UA[1])," (", red$number[1],"), ",
          as.character(red$county_UA[2])," (", red$number[2],"), ",
          as.character(red$county_UA[3])," (", red$number[3],"), ",
          as.character(red$county_UA[4])," (", red$number[4],"), ",
          as.character(red$county_UA[5])," (", red$number[5],"), ", sep="")
  })
  
  output$county_totalcase_update <- renderText({
    paste("Top 5 highest total cases: ", as.character(red2$county_UA[1])," (", red2$number[1],"), ",
          as.character(red2$county_UA[2])," (", red2$number[2],"), ",
          as.character(red2$county_UA[3])," (", red2$number[3],"), ",
          as.character(red2$county_UA[4])," (", red2$number[4],"), ",
          as.character(red2$county_UA[5])," (", red2$number[5],"), ", sep="")
  })
  
  
  url <- a("Twitter", href="https://twitter.com/maxeyre3")
  
  output$twitter <- renderUI({
    tagList(url)
    })
  
  output$twitter2 <- renderUI({
    tagList(url)
  })
  output$twitter_comp <- renderUI({
    tagList(url)
  })
  output$twitter3 <- renderUI({
    tagList(url)
  })
  output$twitter4 <- renderUI({
    tagList(url)
  })
  
  url_data <- a("JHU CSSE Data sources", href="https://github.com/CSSEGISandData/COVID-19")
  url_data_andrew <- a("Thanks to Andrew Lilley for scraping international data", href="https://twitter.com/alil9145")
  url_data2 <- a("Data source", href="https://www.gov.uk/guidance/coronavirus-covid-19-information-for-the-public")
  
  output$data_source <- renderUI({
    tagList(url_data)
  })
  output$data_source_comp <- renderUI({
    tagList(url_data)
  })
  
  output$data_source_andrew <- renderUI({
    tagList(url_data_andrew)
  })
  output$data_source_andrew_comp <- renderUI({
    tagList(url_data_andrew)
  })
  
  output$data_source2 <- renderUI({
    tagList(url_data2)
  })
  output$data_source3 <- renderUI({
    tagList(url_data2)
  })
  output$data_source4 <- renderUI({
    tagList(url_data2)
  })
  
  output$checkGroup <- renderText({
    paste(as.character(length(input$checkGroup)))
  })
  
  output$checkGroup_county <- renderText({
    paste(as.character(c(input$checkGroup_county)))
  })
  output$checkGroup_region <- renderText({
    paste(as.character(c(input$checkGroup_region)))
  })

  output$dateRange.100 <- renderPrint({ input$dateRange.100 })  
    
  
})

