# server.R
library(shiny)
library(datasets)
library(dplyr)
library(readr)
library(tidyverse)
library(grid)

# read in global data
data <- "http://cowid.netlify.com/data/full_data.csv"
data <- read_csv(data)
data[is.na(data)] <- 0

data$date = as.Date(data$date, "%Y-%m-%d")
data <- data %>%
  rename(country=location)
data <- data[order(data$country),]
data$country <- as.character(data$country)
country.list <- c(unique(data$country))
date=c(seq(as.Date("2020-01-21"), as.Date(max(data$date)), "days"))
extra.data <- data.frame(date=date, country=c(rep("blank",length(date))), 
                         new_cases=c(rep(0,length(date))),
                                     new_deaths=c(rep(0,length(date))),
                                                  total_cases=c(rep(0,length(date))),total_deaths=c(rep(0,length(date))))

data.list <- data.frame(date=c(),country=c(),new_cases=c(),new_deaths=c(),total_cases=c(),new_cases=c())
for (i in 1:length(country.list)){
  data.out <- extra.data %>%
    filter(date <min(data$date[data$country==paste(country.list[i])])) %>%
    mutate(country = paste(country.list[i]))
  
  data.list <- rbind(data.list,data.out)
}

data <- rbind(data,data.list)
data <- gather(data, key="type", value="number",3:ncol(data))

# UK county data
# read in UK county data
data.county <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vTNL88hteOvWd8ZafF_h0-38Mg1Z3PJdcBkNI3fvORLbuqIlWzc3btPPmD1sB_xqL6jCHcgU1i50ukX/pub?gid=0&single=true&output=csv"
data.county <- read_csv(data.county)
data.county <- gather(data.county, key="date", value="cases",3:ncol(data.county))
data.county$date = as.Date(data.county$date, "%d/%m/%Y")

data.county <- data.county[order(data.county$county_UA),]

# calculate new daily cases
data.county$new_case <- c()
uni.county <- unique(data.county$county_UA)
out <- c()
for (i in 1:length(uni.county)){
  x <- data.county[data.county$county_UA==uni.county[i],]
  out <- c(out,0,diff(x$cases))
}
data.county$new_cases <- out

# get list of counties
data.county$county_UA <- as.character(data.county$county_UA)
county_LA.list <- c(unique(data.county$county_UA))
list.county <- list()
for (i in 1:length(county_LA.list)){
  list.county[i] <- county_LA.list[i]
}
names(list.county) <- county_LA.list


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
  
  url <- a("Twitter", href="https://twitter.com/maxeyre3")
  
  output$twitter <- renderUI({
    tagList(url)
    })
  
  url_data <- a("Data source", href="https://ourworldindata.org/coronavirus-source-data")
  
  output$data_source <- renderUI({
    tagList(url_data)
  })
  
  output$checkGroup <- renderText({
    paste(as.character(length(input$checkGroup)))
  })
  
  output$checkGroup_county <- renderText({
    paste(as.character(length(input$checkGroup_county)))
  })
    
  # Single country plots
  output$countryPlot <- renderPlot({
    lines <- c(as.character(input$checkGroup))
    
    data <- data[data$type %in% lines, ]
    
      ggplot(data[data$country==paste(formulaText(),sep=""),]) + geom_point(aes(x=date, y=number, col=type),size=1.5) +
        geom_line(aes(x=date, y=number, col=type),size=1) +
      scale_x_date(limits=c(input$dateRange[1],input$dateRange[2])) + xlab(label = "") +ylab(label="Cases") +
        theme_classic()+
        theme(axis.text=element_text(size=13),
              axis.title=element_text(size=16), 
              axis.title.x = element_text(vjust=-1.5),
              axis.title.y = element_text(vjust=2),
              legend.text = element_text(size=16),
              legend.position = 'top', 
              legend.spacing.x = unit(0.4, 'cm'),
              panel.grid.major.y=element_line(size=0.05)) +
        scale_colour_manual(name="",values = c("total_cases" = "#000000", "new_cases" = "#e41a1c", "total_deaths"="#ff7f00", 
                                       "new_deaths"="#a65628"),
                            breaks=c("new_cases","total_cases","new_deaths","total_deaths"),
                            labels=c("Cases (daily)", "Cases (total)", "Deaths (daily)","Deaths (total)")) +
        guides(linetype = guide_legend(override.aes = list(size = 20)))
  })

  # UK country plots
  output$UKcountyPlot <- renderPlot({
    lines <- c(as.character(input$checkGroup_county))
    
    data.county <- data.county[data.county$type %in% lines, ]
    
    ggplot(data.county[data.county$country==paste(formulaText_county(),sep=""),]) + geom_point(aes(x=date, y=number, col=type),size=1.5) +
      geom_line(aes(x=date, y=number, col=type),size=1) +
      scale_x_date(limits=c(input$dateRange[1],input$dateRange[2])) + xlab(label = "") +ylab(label="Cases") +
      theme_classic()+
      theme(axis.text=element_text(size=13),
            axis.title=element_text(size=16), 
            axis.title.x = element_text(vjust=-1.5),
            axis.title.y = element_text(vjust=2),
            legend.text = element_text(size=16),
            legend.position = 'top', 
            legend.spacing.x = unit(0.4, 'cm'),
            panel.grid.major.y=element_line(size=0.05)) +
      scale_colour_manual(name="",values = c("total_cases" = "#000000", "new_cases" = "#e41a1c", "total_deaths"="#ff7f00", 
                                             "new_deaths"="#a65628"),
                          breaks=c("new_cases","total_cases","new_deaths","total_deaths"),
                          labels=c("Cases (daily)", "Cases (total)", "Deaths (daily)","Deaths (total)")) +
      guides(linetype = guide_legend(override.aes = list(size = 20)))
  })
  
  
})

