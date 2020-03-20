# server.R
library(shiny)
library(datasets)
library(dplyr)
library(readr)
library(tidyverse)
library(grid)

# read in data
confirmed <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv"
data.confirmed <- read_csv(confirmed)

#data.no.US <- data.confirmed[data.confirmed$`Country/Region`!="US",]

#data.US <- data.confirmed[data.confirmed$`Country/Region`=="US",]

#state_names <- c(data.US$`Province/State`[1:52])

#data.US.states <- data.US[data.US$`Province/State` %in% state_names, ]
#data.US.counties <- data.US[!data.US$`Province/State` %in% state_names, ]

#data.confirmed <- data.US.clean

#data.confirmed <- data.no.US#rbind(data.no.US,data.US.clean)

# tidy up
data.confirmed <- gather(data.confirmed, key="date", value="cases",5:ncol(data.confirmed))

data.confirmed <- data.confirmed %>%
  rename(prov_state=`Province/State`, country=`Country/Region`) %>%
  mutate(date = as.Date(date, "%m/%d/%y"))

data.confirmed <- data.confirmed %>%
  group_by(country, date) %>%
  summarise(cases=sum(cases))

data.confirmed$date = as.Date(data.confirmed$date, "%Y-%m-%d")

# calculate new daily cases
data.confirmed$new_case <- c()
uni.country <- unique(data.confirmed$country)
out <- c()
for (i in 1:length(uni.country)){
  x <- data.confirmed[data.confirmed$country==uni.country[i],]
  out <- c(out,0,diff(x$cases))
}

data.confirmed$new_cases <- out

# deaths
deaths <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv"
deaths <- read_csv(deaths)

#data.no.US <- deaths[deaths$`Country/Region`!="US",]

#data.US <- deaths[deaths$`Country/Region`=="US",]

#state_names <- c(data.US$`Province/State`[1:52])

#data.US.clean <- data.US[!data.US$`Province/State` %in% state_names, ]

#deaths <- rbind(data.no.US,data.US.clean)

deaths <- gather(deaths, key="date", value="deaths",5:ncol(deaths))

deaths <- deaths %>%
  rename(prov_state=`Province/State`, country=`Country/Region`) %>%
  mutate(date = as.Date(date, "%m/%d/%y"))

deaths <- deaths %>%
  group_by(country, date) %>%
  summarise(deaths=sum(deaths))

deaths$date = as.Date(deaths$date, "%Y-%m-%d")

# calculate new daily deaths
deaths$new_deaths <- c()
uni.country <- unique(deaths$country)
out <- c()
for (i in 1:length(uni.country)){
  x <- deaths[deaths$country==uni.country[i],]
  out <- c(out,0,diff(x$deaths))
}
deaths$new_deaths <- out

data <- left_join(data.confirmed, deaths, by=c("country","date"))

# recovered
recovered <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv"
recovered <- read_csv(recovered)

#data.no.US <- recovered[recovered$`Country/Region`!="US",]

#data.US <- recovered[recovered$`Country/Region`=="US",]

#state_names <- c(data.US$`Province/State`[1:52])

#data.US.clean <- data.US[!data.US$`Province/State` %in% state_names, ]

#recovered <- rbind(data.no.US,data.US.clean)

recovered <- gather(recovered, key="date", value="recovered",5:ncol(recovered))

recovered <- recovered %>%
  rename(prov_state=`Province/State`, country=`Country/Region`) %>%
  mutate(date = as.Date(date, "%m/%d/%y"))

recovered <- recovered %>%
  group_by(country, date) %>%
  summarise(recovered=sum(recovered))

recovered$date = as.Date(recovered$date, "%Y-%m-%d")

# calculate new daily recovered
recovered$new_recovered <- c()
uni.country <- unique(recovered$country)
out <- c()
for (i in 1:length(uni.country)){
  x <- recovered[recovered$country==uni.country[i],]
  out <- c(out,0,diff(x$recovered))
}
recovered$new_recovered <- out

data <- left_join(data, recovered, by=c("country","date"))

# make tidy
data <- gather(data, key="type", value="number",3:ncol(data))

data$country[data$country=="Taiwan*"] <- "Taiwan"

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
  
  url_data <- a("Data source", href="https://github.com/CSSEGISandData/COVID-19")
  
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
        scale_colour_manual(name="",values = c("cases" = "#000000", "new_cases" = "#e41a1c", "deaths"="#ff7f00", 
                                       "new_deaths"="#a65628", "recovered"="#377eb8", "new_recovered"="#4daf4a"),
                            breaks=c("new_cases","cases","new_deaths","deaths","new_recovered","recovered"),
                            labels=c("Cases (daily)", "Cases (total)", "Deaths (daily)","Deaths (total)", "Recovered (daily)","Recovered (total)")) +
        guides(linetype = guide_legend(override.aes = list(size = 20)))

    
  })

})


ggplot(data[data$country=="UK",]) + geom_line(aes(x=date, y=data[,3]))

