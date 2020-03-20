# server.R
library(shiny)
library(datasets)
library(dplyr)
library(readr)
library(tidyverse)
library(grid)

# read in data
data.county <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vTNL88hteOvWd8ZafF_h0-38Mg1Z3PJdcBkNI3fvORLbuqIlWzc3btPPmD1sB_xqL6jCHcgU1i50ukX/pub?gid=0&single=true&output=csv"
data.county <- read_csv(data.county)

data.county <- gather(data.county, key="date", value="cases",3:ncol(data.county))

data.county$date = as.Date(data.county$date, "%d/%m/%Y")

data.county$county_UA <- as.character(data.county$county_UA)

data.county <- data.county[order(data.county$county_UA),]

# Calcuate new cases
# calculate new daily cases
data.county$new_case <- c()
uni.county <- unique(data.county$county_UA)
out <- c()
for (i in 1:length(uni.county)){
  x <- data.county[data.county$county_UA==uni.county[i],]
  out <- c(out,0,diff(x$cases))
}

data.county$new_cases <- out


# tidy up

# Define server logic required to plot various variables against mpg
shinyServer(function(input, output) {
  
  # Compute the forumla text in a reactive expression since it is 
  # shared by the output$caption and output$mpgPlot expressions
  formulaText <- reactive({
    paste(input$country)
  })
  
  output$startdate <- renderText({
    paste("Date range: ",as.character(input$dateRange[1])," to ",as.character(input$dateRange[2]),sep="")
  })
  
  # Return the formula text for printing as a caption
  output$caption <- renderText({
    formulaText()
  })
  
  url <- a("Twitter", href="https://twitter.com/maxeyre3")
  
  output$twitter <- renderUI({
    tagList(url)
    })
  
  url_data <- a("Data source", href="https://www.arcgis.com/home/item.html?id=b684319181f94875a6879bbc833ca3a6")
  
  output$data_source <- renderUI({
    tagList(url_data)
  })
  
  output$checkGroup <- renderText({
    paste(as.character(length(input$checkGroup)))
  })
    
  # Generate a plot of the requested variable against mpg and only 
  # include outliers if requested
  output$countryPlot <- renderPlot({
    lines <- c(as.character(input$checkGroup))
    
    data <- data[data$type %in% lines, ]
    
      ggplot(data[data$country==paste(formulaText(),sep=""),]) + geom_point(aes(x=date, y=cases, col=county_UA),size=1.5) +
        geom_line(aes(x=date, y=cases, col=county_UA),size=1) +
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
        scale_colour_manual(name="",values = c("total_cases" = "#000000", "new_cases" = "#e41a1c"),
                            breaks=c("new_cases","total_cases"),
                            labels=c("Cases (daily)", "Cases (total)")) +
        guides(linetype = guide_legend(override.aes = list(size = 20)))

    
  })

})


ggplot(data[data$country=="UK",]) + geom_line(aes(x=date, y=data[,3]))

