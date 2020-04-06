# server.R
library(shiny)
library(datasets)
library(tidyverse)
library(dplyr)
library(readr)
library(grid)
library(ggplot2)
library(gridExtra)
library(gtable)

# read in global data
data <- read_csv("https://raw.githubusercontent.com/andrewlilley/tool_COVID-19/master/output_data/country_level.csv?token=ANJMHSDWF7URVBXFWA5I47K6SAWOG")

if(any(colnames(data)=="X1")){
  data <- data %>%
    select(-X1)
}

data <- data %>%
  rename(country = Region, total_cases = Transmissions, total_deaths = Deaths)

data <- data[data$country!="UK",]
data$date = as.Date(data$date, "%Y-%m-%d")

UK.data <- read_csv("https://raw.githubusercontent.com/maxeyre/COVID-19/master/Data%20visualisation/UK%20data/UK_total.csv")
UK.data$date = as.Date(UK.data$date, "%d/%m/%Y")
UK.data$country <- "United Kingdom"

data <- rbind(data, UK.data)

data$country[data$country=="US"]<-"United States"
data$country[data$country=="UAE"]<-"United Arab Emirates"

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
data <- gather(data, key="type", value="number",3:ncol(data))

UK.data <- data[data$country=="United Kingdom",]

# UK breakdown data
UK_by_country_cases <- read_csv("https://raw.githubusercontent.com/maxeyre/COVID-19/master/Data%20visualisation/UK%20data/UK_countries_cases.csv")
UK_by_country_pop <- data.frame(country=c("England","Scotland","Wales","Northern Ireland"), pop=c(55980000,5438000,3139000,1882000))
UK_by_country_cases$pop <- UK_by_country_pop$pop
UK_by_country_cases <- gather(UK_by_country_cases, key="date", value="total_cases",2:(ncol(UK_by_country_cases)-1))

UK_by_country_deaths <- read_csv("https://raw.githubusercontent.com/maxeyre/COVID-19/master/Data%20visualisation/UK%20data/UK_countries_deaths.csv")
UK_by_country_deaths <- UK_by_country_deaths[,c(1,3:6)]
UK_by_country_deaths <- gather(UK_by_country_deaths, key="country", value="total_deaths",2:(ncol(UK_by_country_deaths)))
UK_by_country_deaths <- UK_by_country_deaths %>%
  rename(date = Date)
  
UK_by_country <- left_join(UK_by_country_cases,UK_by_country_deaths,by=c("date","country"))
UK_by_country <- UK_by_country[order(UK_by_country$country),]

out.cases <- c()
out.deaths <- c()
for (i in 1:nrow(UK_by_country_pop)){
  x <- UK_by_country[UK_by_country$country==UK_by_country_pop$country[order(UK_by_country_pop$country)][i],]
  out.cases <- c(out.cases,0,diff(x$total_cases))
  out.deaths <- c(out.deaths,NA,diff(x$total_deaths))
}
UK_by_country$new_cases <- out.cases
UK_by_country$new_deaths <- out.deaths

UK_by_country <- gather(UK_by_country, key="type", value="number", 4:7)
UK_by_country$date <- as.Date(UK_by_country$date, "%d/%m/%Y")



# read in country population data
country.pop.data <- read_csv("https://raw.githubusercontent.com/maxeyre/COVID-19/master/Data%20visualisation/Other/country_pop.csv")
data <- left_join(data,country.pop.data, by="country")
data$number_pop <- 100000*data$number/data$pop

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
data.deaths10 <- data.deaths10[data.deaths10$number>=10,]
uni.country.deaths10 <- c(unique(data.deaths10$country))
data.deaths10.out <- NULL
date.deaths10 <- c(as.Date("2020-01-01","%Y-%m-%d"))
for (i in 1:length(uni.country.deaths10)){
  x <- data.deaths10[data.deaths10$country==uni.country.deaths10[i],]
  out <- as.Date(x$date[which(x$number>=10)],"%Y-%m-%d")
  out_pop <- as.Date(x$date[which(x$number_pop>=0.5)],"%Y-%m-%d")
  if(length(out_pop)==0) out_pop <- 0
  out <- min(out)
  out_pop <- min(out_pop)
  x$date_rel <- x$date - out
  x$date_rel_pop <- x$date - out_pop
  x <- x[x$date_rel>=0,]
  x_pop <- x[x$date_rel_pop>=0,]
  data.deaths10.out <- rbind(data.deaths10.out, x, x_pop)
}
data.deaths10 <- data.deaths10.out
data.deaths10$date_rel <- as.numeric(data.deaths10$date_rel)
data.deaths10$date_rel_pop <- as.numeric(data.deaths10$date_rel_pop)

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
data.test <- data.test[,1:4]
data.test <- data.test %>%
  select(date, total_tested = tested, total_cases=cases, new_cases)
data.test$date = as.Date(data.test$date, "%d/%m/%Y")
data.test$new_tested <- c(NA,diff(data.test$total_tested))
data.test$total_prop_pos <- 100*data.test$total_cases/data.test$total_tested
data.test$new_prop_pos <- 100*data.test$new_cases/data.test$new_tested

data.test <- gather(data.test, key="type", value="number",2:ncol(data.test))


# Define server logic required to plot various variables against mpg
shinyServer(function(input, output, session) {
  
  # Change date range for by country UK graphs
  
  observe({
    val <- input$checkGroup_UK
    if(length(val)<3 & input$tabs_UK==2){
      x <- sum("new_deaths" %in% val, "total_deaths" %in% val)
      if(x==length(val)) {
        updateDateRangeInput(session, "dateRange_UK", 
                             start  = as.Date("27/03/2020", "%d/%m/%Y"),
                             end    = max(UK.data$date), 
                             min    = as.Date("27/03/2020", "%d/%m/%Y"),
                             max    = max(UK.data$date))
      } else{
        updateDateRangeInput(session, "dateRange_UK", 
                             start  = as.Date("09/03/2020", "%d/%m/%Y"),
                             end    = max(UK.data$date), 
                             min    = as.Date("09/03/2020", "%d/%m/%Y"),
                             max    = max(UK.data$date))
        
      }
    }
  })
  
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
  
  output$twitter_UK <- renderUI({
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
  output$data_source_UK <- renderUI({
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
  
  output$counter <- renderText({
    library(rdrop2)
    token <- readRDS("token.rds")
    counter <- drop_read_csv("counter.csv",dtoken = token)
    counter$count <- counter$count + 1
    counter <- counter%>%
      select(count)
    write.csv(counter, file = "counter.csv")
    drop_upload("counter.csv",dtoken = token)
    paste0(counter$count," site visits", sep="")
  })
    
  # Single country plots
  output$countryPlot <- renderPlot({
    lines <- c(as.character(input$checkGroup))
    
    data<- data[data$type %in% lines, ]
    if(input$pop_country=="pop_yes"){
      p <- ggplot(data[data$country==paste(formulaText(),sep=""),]) + geom_point(aes(x=date, y=number_pop, col=type),size=1.5) +
        geom_line(aes(x=date, y=number_pop, col=type),size=1) +
        scale_x_date(limits=c(input$dateRange[1],input$dateRange[2])) + xlab(label = "") +ylab(label="Cases (per 100,000)") +
        theme_classic()+
        theme(axis.text=element_text(size=13),
              axis.title=element_text(size=16), 
              axis.title.x = element_text(vjust=-1.5),
              axis.title.y = element_text(vjust=2),
              legend.text = element_text(size=13),
              legend.position = 'top', 
              legend.spacing.x = unit(0.4, 'cm'),
              panel.grid.major.y=element_line(size=0.05)) +
        scale_colour_manual(name="",values = c("total_cases" = "#000000", "new_cases" = "#e41a1c", "total_deaths"="#ff7f00", 
                                               "new_deaths"="#a65628"),
                            breaks=c("new_cases","total_cases","new_deaths","total_deaths"),
                            labels=c("Cases (daily)", "Cases (total)", "Deaths (daily)","Deaths (total)")) +
        guides(linetype = guide_legend(override.aes = list(size = 20)))
      if(input$log=='log_yes'){
        p <- p + scale_y_log10(labels = scales::comma)
      }
      }
    else{
        p <- ggplot(data[data$country==paste(formulaText(),sep=""),]) + geom_point(aes(x=date, y=number, col=type),size=1.5) +
          geom_line(aes(x=date, y=number, col=type),size=1) +
          scale_x_date(limits=c(input$dateRange[1],input$dateRange[2])) + xlab(label = "") +ylab(label="Cases") +
          theme_classic()+
          theme(axis.text=element_text(size=13),
                axis.title=element_text(size=16), 
                axis.title.x = element_text(vjust=-1.5),
                axis.title.y = element_text(vjust=2),
                legend.text = element_text(size=13),
                legend.position = 'top', 
                legend.spacing.x = unit(0.4, 'cm'),
                panel.grid.major.y=element_line(size=0.05)) +
          scale_colour_manual(name="",values = c("total_cases" = "#000000", "new_cases" = "#e41a1c", "total_deaths"="#ff7f00", 
                                                 "new_deaths"="#a65628"),
                              breaks=c("new_cases","total_cases","new_deaths","total_deaths"),
                              labels=c("Cases (daily)", "Cases (total)", "Deaths (daily)","Deaths (total)")) +
          guides(linetype = guide_legend(override.aes = list(size = 20)))
        if(input$log=='log_yes'){
          p <- p + scale_y_log10(labels = scales::comma) 
      }
    }
      p
  })
  
  # country comparisons
  output$countryPlot_compare <- renderPlot({
    lines2 <- c(as.character(input$checkGroup_countryCompare))
    
    if(input$compare_by=="cases"){
      data.100<- data.100[data.100$country %in% lines2, ]
      y_min <- 1
    }else{
      data.100<- data.deaths10[data.deaths10$country %in% lines2, ]
      y_min <- 0.5
    }
    
    if(input$compare_pop=="pop_no"){
      p2 <- ggplot(data.100) + geom_point(aes(x=date_rel, y=number, col=country),size=1.5) +
        geom_line(aes(x=date_rel, y=number, col=country),size=1) +
        scale_x_continuous(limits=c(input$dateRange.100[1],input$dateRange.100[2])) + xlab(label = "Days") +
        ylab(label="Cases") +
        theme_classic()+
        theme(axis.text=element_text(size=13),
              axis.title=element_text(size=16), 
              axis.title.x = element_text(vjust=-0.5),
              axis.title.y = element_text(vjust=2),
              legend.title = element_blank(),
              legend.text = element_text(size=13),
              legend.position = 'top', 
              legend.spacing.x = unit(0.4, 'cm'),
              panel.grid.major.y=element_line(size=0.05)) +
        guides(linetype = guide_legend(override.aes = list(size = 20))) 
      if(input$log_compare=='log_yes'){
        p2 <- p2 + scale_y_log10(labels = scales::comma)
      }
    } else{
      p2 <- ggplot(data.100) + geom_point(aes(x=date_rel_pop, y=number_pop, col=country, ymin=y_min),size=1.5) +
        geom_line(aes(x=date_rel_pop, y=number_pop, col=country),size=1) +
        scale_x_continuous(limits=c(input$dateRange.100[1],input$dateRange.100[2])) + xlab(label = "Days") +
        ylab(label="Cases (per 100,000)") +
        theme_classic()+
        theme(axis.text=element_text(size=13),
              axis.title=element_text(size=16), 
              axis.title.x = element_text(vjust=-0.5),
              axis.title.y = element_text(vjust=2),
              legend.title = element_blank(),
              legend.text = element_text(size=13),
              legend.position = 'top', 
              legend.spacing.x = unit(0.4, 'cm'),
              panel.grid.major.y=element_line(size=0.05)) +
        guides(linetype = guide_legend(override.aes = list(size = 20))) 
      if(input$log_compare=='log_yes'){
        p2 <- p2 + scale_y_log10(labels = scales::comma)
      }
      }
      
    p2
  })

  
  # UK plot
  output$UKPlot <- renderPlot({
    lines <- c(as.character(input$checkGroup_UK))
    
    UK.data<- UK.data[UK.data$type %in% lines, ]
    if (input$pop_UK=="pop_yes"){
      p <- ggplot(UK.data) + geom_point(aes(x=date, y=100000*number/66440000, col=type),size=1.5) +
        geom_line(aes(x=date, y=100000*number/66440000, col=type),size=1) +
        scale_x_date(limits=c(input$dateRange_UK[1],input$dateRange_UK[2])) + xlab(label = "") +ylab(label="Cases (per 100,000)") +
        theme_classic()+
        theme(axis.text=element_text(size=13),
              axis.title=element_text(size=16), 
              axis.title.x = element_text(vjust=-1.5),
              axis.title.y = element_text(vjust=2),
              legend.text = element_text(size=13),
              legend.position = 'top', 
              legend.spacing.x = unit(0.4, 'cm'),
              panel.grid.major.y=element_line(size=0.05)) +
        scale_colour_manual(name="",values = c("total_cases" = "#000000", "new_cases" = "#e41a1c", "total_deaths"="#ff7f00", 
                                               "new_deaths"="#a65628"),
                            breaks=c("new_cases","total_cases","new_deaths","total_deaths"),
                            labels=c("Cases (daily)", "Cases (total)", "Deaths (daily)","Deaths (total)")) +
        guides(linetype = guide_legend(override.aes = list(size = 20)))
      if(input$log_UK=='log_yes'){
        p <- p + scale_y_log10(labels = scales::comma)
      }
      } else{
        p <- ggplot(UK.data) + geom_point(aes(x=date, y=number, col=type),size=1.5) +
          geom_line(aes(x=date, y=number, col=type),size=1) +
          scale_x_date(limits=c(input$dateRange_UK[1],input$dateRange_UK[2])) + xlab(label = "") +ylab(label="Cases") +
          theme_classic()+
          theme(axis.text=element_text(size=13),
                axis.title=element_text(size=16), 
                axis.title.x = element_text(vjust=-1.5),
                axis.title.y = element_text(vjust=2),
                legend.text = element_text(size=13),
                legend.position = 'top', 
                legend.spacing.x = unit(0.4, 'cm'),
                panel.grid.major.y=element_line(size=0.05)) +
          scale_colour_manual(name="",values = c("total_cases" = "#000000", "new_cases" = "#e41a1c", "total_deaths"="#ff7f00", 
                                                 "new_deaths"="#a65628"),
                              breaks=c("new_cases","total_cases","new_deaths","total_deaths"),
                              labels=c("Cases (daily)", "Cases (total)", "Deaths (daily)","Deaths (total)")) +
          guides(linetype = guide_legend(override.aes = list(size = 20)))
        if(input$log_UK=='log_yes'){
          p <- p + scale_y_log10(labels = scales::comma)
        }
      }
      p
  })
  
  # UK plot
  output$UKPlot_by_country <- renderPlot({
    lines <- c(as.character(input$checkGroup_UK))
    
    UK_by_country<- UK_by_country[UK_by_country$type %in% lines, ]
    
    val <- input$checkGroup_UK
    if(length(val)<3 & input$tabs_UK==2){
      x <- sum("new_deaths" %in% val, "total_deaths" %in% val)
      if(x==length(val)) {
        date.min <- as.Date("27/03/2020", "%d/%m/%Y")
      } else {
        date.min <- as.Date("09/03/2020", "%d/%m/%Y")
      }
      } else {
        date.min <- as.Date("09/03/2020", "%d/%m/%Y")
      }
    
    if (input$pop_UK=="pop_yes"){
      p <- ggplot(UK_by_country) + geom_point(aes(x=date, y=100000*number/pop, col=country),size=1.5) +
        geom_line(aes(x=date, y=100000*number/pop, col=country, linetype=type),size=1) +
        scale_x_date(limits=c(date.min,input$dateRange_UK[2])) + xlab(label = "") +ylab(label="Cases (per 100,000)") +
        theme_classic()+
        theme(axis.text=element_text(size=13),
              axis.title=element_text(size=16), 
              axis.title.x = element_text(vjust=-1.5),
              axis.title.y = element_text(vjust=2),
              legend.title = element_blank(),
              legend.text = element_text(size=13),
              legend.position = 'top', 
              legend.spacing.x = unit(0.4, 'cm'),
              panel.grid.major.y=element_line(size=0.05)) +  scale_linetype_manual(name="", values=c("total_cases"=1, "new_cases" = 2, "total_deaths" =3, "new_deaths"=4),
                                                                                   breaks=c("total_cases","new_cases","total_deaths","new_deaths"),
                                                                                   labels=c("Cases (total)","Cases (daily)","Deaths (total)","Deaths (daily)")) +
        guides(linetype = guide_legend(label.position = "top", keywidth = 2)) +
        theme(legend.direction = "horizontal",legend.box = "vertical")
      
      if(input$log_UK=='log_yes'){
        p <- p + scale_y_log10(labels = scales::comma)
      }
    }else{
      p <- ggplot(UK_by_country) + geom_point(aes(x=date, y=number, col=country),size=1.5) +
        geom_line(aes(x=date, y=number, col=country, linetype=type),size=1) +
        scale_x_date(limits=c(date.min,input$dateRange_UK[2])) + xlab(label = "") +ylab(label="Cases") +
        theme_classic()+
        theme(axis.text=element_text(size=13),
              axis.title=element_text(size=16), 
              axis.title.x = element_text(vjust=-1.5),
              axis.title.y = element_text(vjust=2),
              legend.title = element_blank(),
              legend.text = element_text(size=13),
              legend.position = 'top', 
              legend.spacing.x = unit(0.4, 'cm'),
              panel.grid.major.y=element_line(size=0.05)) +  scale_linetype_manual(name="", values=c("total_cases"=1, "new_cases" = 2, "total_deaths" =3, "new_deaths"=4),
                                                                                   breaks=c("total_cases","new_cases","total_deaths","new_deaths"),
                                                                                   labels=c("Cases (total)","Cases (daily)","Deaths (total)","Deaths (daily)")) +
        guides(linetype = guide_legend(label.position = "top", keywidth = 2)) +
        theme(legend.direction = "horizontal",legend.box = "vertical")
      
      if(input$log_UK=='log_yes'){
        p <- p + scale_y_log10(labels = scales::comma)
      }
      }
    
    p
  })
  
  # England NHS regions plots
  output$EnglandRegionPlot <- renderPlot({
    
    lines <- c(as.character(input$checkGroup_region))
    
    if (input$pop=="pop_yes"){
      data.region <- data.region.pop
    }
    data.region<- data.region[data.region$type %in% lines, ]
  
    p.pop <- ggplot(data.region) + geom_point(aes(x=date, y=number, col=region),size=1.5) +
      geom_line(aes(x=date, y=number, col=region, linetype=type),size=1) +
      scale_x_date(limits=c(input$dateRange_region[1],input$dateRange_region[2])) + xlab(label = "") +ylab(label="Cases") +
      theme_classic()+
      theme(axis.text=element_text(size=13),
            axis.title=element_text(size=16), 
            axis.title.x = element_text(vjust=-1.5),
            axis.title.y = element_text(vjust=2),
            legend.title = element_blank(),
            legend.text = element_text(size=13),
            legend.position = 'top', 
            legend.spacing.x = unit(0.4, 'cm'),
            panel.grid.major.y=element_line(size=0.05)) + scale_linetype_manual(name="", values=c("total_cases"=1, "new_cases" = 2),
                                                                                breaks=c("total_cases","new_cases"),
                                                                                labels=c("Cases (total)","Cases (daily)")) +
      guides(linetype = guide_legend(label.position = "top", keywidth = 2)) +
      theme(legend.direction = "horizontal",legend.box = "vertical")
    if (input$pop=="pop_yes"){
      p.pop <- p.pop +  ylab(label="Cases (per 100,000)")
    }
    
    if(input$log_region=='log_yes'){
      p.pop <- p.pop + scale_y_log10(labels = scales::comma)
    }
      p.pop
  })
  
  # England county plots
  output$englandcountyPlot <- renderPlot({
    lines <- c(as.character(input$checkGroup_county))
    
    data.county <- data.county[data.county$type %in% lines, ]
    
    ggplot(data.county[data.county$county_UA==paste(formulaText_county(),sep=""),]) + geom_point(aes(x=date, y=number, col=type),size=1.5) +
      geom_line(aes(x=date, y=number, col=type),size=1) +
      scale_x_date(limits=c(input$dateRange_county[1],input$dateRange_county[2])) + xlab(label = "") +ylab(label="Cases") +
      theme_classic()+
      theme(axis.text=element_text(size=13),
            axis.title=element_text(size=16), 
            axis.title.x = element_text(vjust=-1.5),
            axis.title.y = element_text(vjust=2),
            legend.text = element_text(size=13),
            legend.position = 'top', 
            legend.spacing.x = unit(0.4, 'cm'),
            panel.grid.major.y=element_line(size=0.05)) +
      scale_colour_manual(name="",values = c("total_cases" = "#000000", "new_cases" = "#e41a1c"),
                          breaks=c("new_cases","total_cases"),
                          labels=c("Cases (daily)", "Cases (total)")) +
      guides(linetype = guide_legend(override.aes = list(size = 20)))
  })
  
  # UK testing plot
  output$UKtestingPlot <- renderPlot({
    
    lines <- c(as.character(input$checkGroup_test))
    
    data.test <- data.test[data.test$type %in% lines, ]
    
    p.test <- ggplot(data.test) + geom_point(aes(x=date, y=number, col=type),size=1.5)+ 
      geom_line(aes(x=date, y=number, col=type, group=type),size=1) +
      scale_x_date(limits=c(input$dateRange_test[1],input$dateRange_test[2])) + xlab(label = "") +ylab(label="Number tested") +
      theme_classic()+
      theme(axis.text=element_text(size=13),
            axis.title=element_text(size=16), 
            axis.title.x = element_text(vjust=-1.5),
            axis.title.y = element_text(vjust=2),
            legend.text = element_text(size=13),
            legend.position = 'top', 
            legend.spacing.x = unit(0.4, 'cm'),
            panel.grid.major.y=element_line(size=0.05)) +
      scale_colour_manual(name="",values = c("total_tested" = "#000000", "new_tested" = "#e41a1c"),
                          breaks=c("new_tested","total_tested"),
                          labels=c("Daily", "Total"))
    if(input$log_test=='log_yes'){
      p.test <- p.test + scale_y_log10(labels = scales::comma)
    }
    p.test
  })
  
  output$UKtestingPlot2 <- renderPlot({
    
    lines <- c(as.character(input$checkGroup_test2))
    
    data.test <- data.test[data.test$type %in% lines, ]
    
    p.test <- ggplot(data.test) + geom_point(aes(x=date, y=number, col=type),size=1.5)+ 
      geom_line(aes(x=date, y=number, col=type, group=type),size=1) +
      scale_x_date(limits=c(input$dateRange_test2[1],input$dateRange_test2[2])) + xlab(label = "") +ylab(label="Prop. positive (%)") +
      theme_classic()+
      theme(axis.text=element_text(size=13),
            axis.title=element_text(size=16), 
            axis.title.x = element_text(vjust=-1.5),
            axis.title.y = element_text(vjust=2),
            legend.text = element_text(size=13),
            legend.position = 'top', 
            legend.spacing.x = unit(0.4, 'cm'),
            panel.grid.major.y=element_line(size=0.05)) +
      scale_colour_manual(name="",values = c("total_prop_pos" = "#000000", "new_prop_pos" = "#e41a1c"),
                          breaks=c("new_prop_pos","total_prop_pos"),
                          labels=c("Daily", "Total"))
    p.test
    
  })
  
  
})

