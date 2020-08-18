# server.R
library(shiny)
library(tidyverse)
library(ggplot2)

# read in global data
data <- read_csv("https://raw.githubusercontent.com/maxeyre/COVID-19/master/data_scraper/data/processed/JHU_full.csv")
data$country[data$country=="Cote d'Ivoire"] <- "Cote d\'Ivoire"

data.100 <- read_csv("https://raw.githubusercontent.com/maxeyre/COVID-19/master/data_scraper/data/processed/JHU_100-cases.csv")
data.100$country[data.100$country=="Cote d'Ivoire"] <- "Cote d\'Ivoire"
data.deaths10 <- read_csv("https://raw.githubusercontent.com/maxeyre/COVID-19/master/data_scraper/data/processed/JHU_5-deaths.csv")
data.deaths10$country[data.deaths10$country=="Cote d'Ivoire"] <- "Cote d\'Ivoire"
# add GHS Index
ghs <- read_csv("https://raw.githubusercontent.com/maxeyre/COVID-19/master/Data%20visualisation/Other/GHS_index.csv") %>%
  mutate(rank=paste0("GHS:",rank))
ghs$country[106] <- "Cote d\'Ivoire"
ghs$country <- as.factor(ghs$country)

data.100 <- data.100 %>%
  left_join(ghs, by="country")

data.deaths10 <- data.deaths10  %>%
  left_join(ghs, by="country")
  
# UK data for epi curve
UK.data <- data[data$country=="United Kingdom",]

# PHE UK data
UK <- read_csv("https://raw.githubusercontent.com/maxeyre/COVID-19/master/data_scraper/data/processed/UK.csv")

# UK breakdown data
UK_by_country <- UK[UK$division=="Country",]

# UK county data
# read in UK county data
data.county <- UK[UK$division=="UTLA",]

# get list of counties
data.county$area <- as.character(data.county$area)
county_LA.list <- c(unique(data.county$area))
list.county <- list()
for (i in 1:length(county_LA.list)){
  list.county[i] <- county_LA.list[i]
}
names(list.county) <- county_LA.list

# read in England region data
data.region <- UK[UK$division=="Region",]

# Testing data
#data.test <- read_csv("https://raw.githubusercontent.com/tomwhite/covid-19-uk-data/master/data/covid-19-tests-uk.csv") %>%
 # select(date=Date, -Country, new_tests = DailyTestsPerformed, total_tests= TotalTestsPerformed, new_test_ppl = DailyPeopleTested, total_test_ppl = TotalPeopleTested) %>%
  #mutate(date=as.Date(date, "%Y-%m-%d")) 

# # code to deal with mismatch in lengths for testing and UK data
# if(length(UK.data$number[UK.data$type=="total_cases" & UK.data$date>="2020-03-17"]) < nrow(data.test)){
#   data.test <- data.test[1:(length(UK.data$number[UK.data$type=="total_cases" & UK.data$date>="2020-03-17"])),]
# }
# 
# if(length(UK.data$number[UK.data$type=="total_cases" & UK.data$date>="2020-03-17"]) > nrow(data.test)){
#   x <- length(UK.data$number[UK.data$type=="total_cases" & UK.data$date>="2020-03-17"]) - nrow(data.test)
#   data.test[((nrow(data.test)+1):length(UK.data$number[UK.data$type=="total_cases" & UK.data$date>="2020-03-17"])),] <- NA
#   for (i in (nrow(data.test)-x+1):nrow(data.test)){
#     data.test[i,1]<- data.test[(i-1),1] + 1
#   }
# }
# 
# data.test$total_cases <- UK.data$number[UK.data$type=="total_cases" & UK.data$date>="2020-03-17"]
# data.test$new_cases <- UK.data$number[UK.data$type=="new_cases" & UK.data$date>="2020-03-17"]
# 
# data.test$total_prop_pos <- 100*data.test$total_cases/data.test$total_test_ppl
# data.test$new_prop_pos <- 100*data.test$new_cases/data.test$new_test_ppl

#data.test <- data.test %>%
 # gather(key="type", value="number",-date)

# Brazil data
states.names <- tibble(ID=c("SP","RJ","CE","AM","SC","MG","PE","PR","RS","BA","DF","ES","MA","RN","PA","GO","AP","MT","MS","PB","RR","AC","AL","SE","PI","RO","TO"),
                       state_name=c("São Paulo","Rio de Janeiro","Ceará","Amazonas","Santa Catarina","Minas Gerais","Pernambuco","Paraná","Rio Grande do Sul",
                                    "Bahia","Distrito Federal","Espírito Santo","Maranhão","Rio Grande do Norte","Pará","Goiás","Amapá","Mato Grosso","Mato Grosso do Sul",
                                    "Paraíba","Roraima","Acre","Alagoas","Sergipe","Piauí","Rondônia","Tocantins"))
data.brazil <- read_csv("https://raw.githubusercontent.com/maxeyre/COVID-19/master/data_scraper/data/processed/brazil_full.csv")
data.brazil$date <- as.Date(data.brazil$date, "%Y-%m-%d")
data.brazil <- data.brazil %>%
  select(-state_name) %>%
  left_join(states.names, by=c("state"="ID"))

# get list of states
data.brazil$state_name <- as.character(data.brazil$state_name)
state.list <- c(unique(data.brazil$state_name))
state.list <- state.list[order(state.list)]
list.state <- list()
for (i in 1:length(state.list)){
  list.state[i] <- state.list[i]
}
names(list.state) <- state.list
  

# Define server logic required to plot various variables against mpg
shinyServer(function(input, output, session) {
  
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
  
  red <- data.county[data.county$date == max(data.county$date, na.rm=TRUE) & data.county$type == "new_cases",]
  red <- red[order(red$number,decreasing=TRUE),]
  
  red2 <- data.county[data.county$date == max(data.county$date) & data.county$type == "total_cases",]
  red2 <- red2[order(red2$number,decreasing=TRUE),]
  
  red3 <- data.county[data.county$date == max(data.county$date) & data.county$type == "total_cases",]
  red3 <- red3[order(red3$number_pop,decreasing=TRUE),]
  
  output$county_newcase_update <- renderText({
    paste("Top 5 highest new daily reported cases: ", as.character(red$area[1])," (", red$number[1],"), ",
          as.character(red$area[2])," (", red$number[2],"), ",
          as.character(red$area[3])," (", red$number[3],"), ",
          as.character(red$area[4])," (", red$number[4],"), ",
          as.character(red$area[5])," (", red$number[5],"), ", sep="")
  })
  
  output$county_totalcase_update <- renderText({
    paste("Top 5 highest total reported cases: ", as.character(red2$area[1])," (", red2$number[1],"), ",
          as.character(red2$area[2])," (", red2$number[2],"), ",
          as.character(red2$area[3])," (", red2$number[3],"), ",
          as.character(red2$area[4])," (", red2$number[4],"), ",
          as.character(red2$area[5])," (", red2$number[5],"), ", sep="")
  })
  
  output$county_totalcase_rate_update <- renderText({
    paste("Top 5 highest total reported cases per 100,000 population: ", as.character(red3$area[1])," (", round(red3$number_pop[1],0),"), ",
          as.character(red3$area[2])," (", round(red3$number_pop[2],0),"), ",
          as.character(red3$area[3])," (", round(red3$number_pop[3],0),"), ",
          as.character(red3$area[4])," (", round(red3$number_pop[4],0),"), ",
          as.character(red3$area[5])," (", round(red3$number_pop[5],0),"), ", sep="")
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
  
  output$twitter_br <- renderUI({
    tagList(url)
  })
  
  url_data <- a("JHU CSSE Data sources", href="https://github.com/CSSEGISandData/COVID-19")
  url_github <- a("GitHub", href="https://github.com/maxeyre/COVID-19")
  url_data2 <- a("Data source", href="https://www.gov.uk/guidance/coronavirus-covid-19-information-for-the-public")
  url_data_ghs <- a("GHS Index", href="https://www.ghsindex.org/about/")
  url_data_br <- a("Data source", href="https://github.com/wcota/covid19br")
  
  output$data_source <- renderUI({
    tagList(url_data)
  })
  output$data_source_comp <- renderUI({
    tagList(url_data)
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
  
  output$data_source_br <- renderUI({
    tagList(url_data_br)
  })
  
  output$git1 <- renderUI({
    tagList(url_github)
  })
  output$git2 <- renderUI({
    tagList(url_github)
  })
  output$git3 <- renderUI({
    tagList(url_github)
  })
  output$git4 <- renderUI({
    tagList(url_github)
  })
  output$git5 <- renderUI({
    tagList(url_github)
  })
  output$git6 <- renderUI({
    tagList(url_github)
  })
  output$git_br <- renderUI({
    tagList(url_github)
  })
  
  output$ghs <- renderUI({
    tagList(url_data_ghs)
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
        scale_x_date(limits=c(input$dateRange[1],input$dateRange[2])) + xlab(label = "") +ylab(label="Number (per 100,000)") +
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
                                               "new_deaths"="#a65628","total_recoveries"="#377eb8", "new_recoveries"="#4daf4a"),
                            breaks=c("new_cases","total_cases","new_deaths","total_deaths","new_recoveries","total_recoveries"),
                            labels=c("Cases (daily)", "Cases (total)", "Deaths (daily)","Deaths (total)","Recoveries (daily)","Recoveries (total)")) +
        guides(linetype = guide_legend(override.aes = list(size = 20))) +
        theme(legend.direction = "horizontal",legend.box = "vertical")
      if(input$log=='log_yes'){
        p <- p + scale_y_log10()
      }
      }
    else{
        p <- ggplot(data[data$country==paste(formulaText(),sep=""),]) + geom_point(aes(x=date, y=number, col=type),size=1.5) +
          geom_line(aes(x=date, y=number, col=type),size=1) +
          scale_x_date(limits=c(input$dateRange[1],input$dateRange[2])) + xlab(label = "") +ylab(label="Number") +
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
                                                 "new_deaths"="#a65628","total_recoveries"="#377eb8", "new_recoveries"="#4daf4a"),
                              breaks=c("new_cases","total_cases","new_deaths","total_deaths","new_recoveries","total_recoveries"),
                              labels=c("Cases (daily)", "Cases (total)", "Deaths (daily)","Deaths (total)","Recoveries (daily)","Recoveries (total)")) +
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
      lab_y <- "Cases"

    }else{
      data.100<- data.deaths10[data.deaths10$country %in% lines2, ]
      lab_y <- "Deaths"
    }
    
    if(input$compare_pop=="pop_no"){
      y_min <- min(data.100$number[data.100$date_rel==0], na.rm=TRUE)
      y_max <- max(data.100$number, na.rm=TRUE)
      
      p2 <- ggplot(data.100) + geom_point(aes(x=date_rel, y=number, col=country),size=1.5) +
        geom_line(aes(x=date_rel, y=number, col=country),size=1) +
        scale_x_continuous(limits=c(input$dateRange.100[1],input$dateRange.100[2])) + scale_y_continuous(labels= scales::comma) + 
        xlab(label = "Days") +
        ylab(label=paste(lab_y)) + geom_text(data = data.100 %>% arrange(country, date) %>% 
                                               group_by(country) %>% 
                                               summarise_all(last), aes(x=date_rel + 4, y=number, col=country, label=rank),show.legend = FALSE) +
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
        p2 <- p2 + scale_y_log10(limits=c(y_min,y_max), labels = scales::comma)
      }
    } else{
      y_min <- min(data.100$number_pop[data.100$date_rel_pop==0], na.rm=TRUE)
      y_max <- max(data.100$number_pop, na.rm=TRUE)
      
      p2 <- ggplot(data.100) + geom_point(aes(x=date_rel_pop, y=number_pop, col=country),size=1.5) +
        geom_line(aes(x=date_rel_pop, y=number_pop, col=country),size=1) +
        scale_x_continuous(limits=c(input$dateRange.100[1],input$dateRange.100[2])) + 
        geom_text(data = data.100 %>% arrange(country, date) %>% 
                    group_by(country) %>% 
                    summarise_all(last), aes(x=date_rel_pop +4 , y=number_pop, col=country, label=rank),show.legend = FALSE) +
        xlab(label = "Days") +
        ylab(label=paste(lab_y," (per 100,000)",sep="")) +
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
        p2 <- p2 + scale_y_log10(limits=c(y_min,y_max))
      }
      }
      
    p2
  })
  
  
  # UK plot
  output$UKPlot <- renderPlot({
    lines <- c(as.character(input$checkGroup_UK))
    
    UK.data<- UK.data[UK.data$type %in% lines, ]
    if (input$pop_UK=="pop_yes"){
      p <- ggplot(UK.data) + geom_point(aes(x=date, y=number_pop, col=type),size=1.5) +
        geom_line(aes(x=date, y=number_pop, col=type),size=1) +
        scale_x_date(limits=c(input$dateRange_UK[1],input$dateRange_UK[2])) + xlab(label = "") +ylab(label="Number (per 100,000)") +
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
        p <- p + scale_y_log10(labels = scales::number_format(accuracy = 0.01))
      }
      } else{
        p <- ggplot(UK.data) + geom_point(aes(x=date, y=number, col=type),size=1.5) +
          geom_line(aes(x=date, y=number, col=type),size=1) +
          scale_x_date(limits=c(input$dateRange_UK[1],input$dateRange_UK[2])) + xlab(label = "") +ylab(label="Number") +
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
          p <- p + scale_y_log10()
        }
      }
      p
  })
  
  # UK plot
  # output$UKPlot_by_country <- renderPlot({
  #   lines <- c(as.character(input$checkGroup_UK))
  #   UK_by_country <- UK_by_country[UK_by_country$type %in% c("total_deaths","new_deaths"),]
  #   UK_by_country<- UK_by_country[UK_by_country$type %in% lines, ]
  #   
  #   if (input$pop_UK=="pop_yes"){
  #     p <- ggplot(UK_by_country) + geom_point(aes(x=date, y=number_pop, col=area),size=1.5) +
  #       geom_line(aes(x=date, y=number_pop, col=area, linetype=type),size=1) +
  #       scale_x_date(limits=c(as.Date("2020-03-27","%Y-%m-%d"),input$dateRange_UK[2])) + xlab(label = "") +ylab(label="Deaths (per 100,000)") +
  #       theme_classic()+
  #       theme(axis.text=element_text(size=13),
  #             axis.title=element_text(size=16), 
  #             axis.title.x = element_text(vjust=-1.5),
  #             axis.title.y = element_text(vjust=2),
  #             legend.title = element_blank(),
  #             legend.text = element_text(size=13),
  #             legend.position = 'top', 
  #             legend.spacing.x = unit(0.4, 'cm'),
  #             panel.grid.major.y=element_line(size=0.05)) +  scale_linetype_manual(name="", values=c("total_deaths" =3, "new_deaths"=4),
  #                                                                                  breaks=c("total_deaths","new_deaths"),
  #                                                                                  labels=c("Deaths (total)","Deaths (daily)")) +
  #       guides(linetype = guide_legend(label.position = "top", keywidth = 2)) +
  #       theme(legend.direction = "horizontal",legend.box = "vertical")
  #     
  #     if(input$log_UK=='log_yes'){
  #       p <- p + scale_y_log10(labels = scales::number_format(accuracy = 0.01))
  #     }
  #   }else{
  #     p <- ggplot(UK_by_country) + geom_point(aes(x=date, y=number, col=area),size=1.5) +
  #       geom_line(aes(x=date, y=number, col=area, linetype=type),size=1) +
  #       scale_x_date(limits=c(as.Date("2020-03-27","%Y-%m-%d"),input$dateRange_UK[2])) + xlab(label = "") +ylab(label="Deaths") +
  #       theme_classic()+
  #       theme(axis.text=element_text(size=13),
  #             axis.title=element_text(size=16), 
  #             axis.title.x = element_text(vjust=-1.5),
  #             axis.title.y = element_text(vjust=2),
  #             legend.title = element_blank(),
  #             legend.text = element_text(size=13),
  #             legend.position = 'top', 
  #             legend.spacing.x = unit(0.4, 'cm'),
  #             panel.grid.major.y=element_line(size=0.05)) +  scale_linetype_manual(name="", values=c("total_cases"=1, "new_cases" = 2, "total_deaths" =3, "new_deaths"=4),
  #                                                                                  breaks=c("total_cases","new_cases","total_deaths","new_deaths"),
  #                                                                                  labels=c("Cases (total)","Cases (daily)","Deaths (total)","Deaths (daily)")) +
  #       guides(linetype = guide_legend(label.position = "top", keywidth = 2)) +
  #       theme(legend.direction = "horizontal",legend.box = "vertical")
  #     
  #     if(input$log_UK=='log_yes'){
  #       p <- p + scale_y_log10(labels = scales::comma)
  #     }
  #     }
  #   
  #   p
  # })
  
  # England NHS regions plots
  output$EnglandRegionPlot <- renderPlot({
    
    lines <- c(as.character(input$checkGroup_region))
    
    data.region<- data.region[data.region$type %in% lines, ]
  
    if (input$pop=="pop_yes"){
      p.pop <- ggplot(data.region) + geom_point(aes(x=date, y=number_pop, col=area),size=1.5) +
        geom_line(aes(x=date, y=number_pop, col=area, linetype=type),size=1) +
        scale_x_date(limits=c(input$dateRange_region[1],input$dateRange_region[2])) + xlab(label = "") +ylab(label="Cases (per 100,000)") +
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
      if(input$log_region=='log_yes'){
        p.pop <- p.pop + scale_y_log10(labels = scales::number_format(accuracy = 0.01))
      }
      
    } else{
      p.pop <- ggplot(data.region) + geom_point(aes(x=date, y=number, col=area),size=1.5) +
        geom_line(aes(x=date, y=number, col=area, linetype=type),size=1) +
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
      if(input$log_region=='log_yes'){
        p.pop <- p.pop + scale_y_log10()
      }
    }
    
      p.pop
  })
  
  # England county plots
  output$englandcountyPlot <- renderPlot({
    lines <- c(as.character(input$checkGroup_county))
    
    data.county <- data.county[data.county$type %in% lines, ]
    
    if (input$pop_utla=="pop_yes"){
      p.utla <- ggplot(data.county[data.county$area==paste(formulaText_county(),sep=""),]) + geom_point(aes(x=date, y=number_pop, col=type),size=1.5) +
        geom_line(aes(x=date, y=number_pop, col=type),size=1) +
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
      if(input$log_utla=='log_yes'){
        p.utla <- p.utla + scale_y_log10(labels = scales::number_format(accuracy = 0.01))
      }
    } else{
      p.utla <- ggplot(data.county[data.county$area==paste(formulaText_county(),sep=""),]) + geom_point(aes(x=date, y=number, col=type),size=1.5) +
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
      if(input$log_utla=='log_yes'){
        p.utla <- p.utla + scale_y_log10()
      }
    }
    p.utla
    
  })
  
  # UK testing plot
  # output$UKtestingPlot <- renderPlot({
  #   
  #   lines <- c(as.character(input$checkGroup_test))
  #   
  #   data.test <- data.test[data.test$type %in% lines, ]
  #   
  #   p.test <- ggplot(data.test) + geom_point(aes(x=date, y=number, col=type),size=1.5)+ 
  #     geom_line(aes(x=date, y=number, col=type, group=type),size=1) +
  #     scale_x_date(limits=c(input$dateRange_test[1],input$dateRange_test[2])) + 
  #     scale_y_continuous(labels = scales::comma) + xlab(label = "") +ylab(label="People tested") +
  #     theme_classic()+
  #     theme(axis.text=element_text(size=13),
  #           axis.title=element_text(size=16), 
  #           axis.title.x = element_text(vjust=-1.5),
  #           axis.title.y = element_text(vjust=2),
  #           legend.text = element_text(size=13),
  #           legend.position = 'top', 
  #           legend.spacing.x = unit(0.4, 'cm'),
  #           panel.grid.major.y=element_line(size=0.05)) +
  #     scale_colour_manual(name="",values = c("total_tests" = "#000000", "new_tests" = "#e41a1c","total_test_ppl"="#ff7f00", 
  #                                            "new_test_ppl"="#a65628"),
  #                         breaks=c("new_tests","total_tests","new_test_ppl","total_test_ppl"),
  #                         labels=c("Daily", "Cumulative", "Daily", "Cumulative"))
  #   p.test
  # })
  # 
  # output$UKtestingPlot2 <- renderPlot({
  #   
  #   lines <- c(as.character(input$checkGroup_test2))
  #   
  #   data.test <- data.test[data.test$type %in% lines, ]
  #   
  #   p.test <- ggplot(data.test) + geom_point(aes(x=date, y=number, col=type),size=1.5)+ 
  #     geom_line(aes(x=date, y=number, col=type, group=type),size=1) +
  #     scale_x_date(limits=c(input$dateRange_test[1],input$dateRange_test[2])) + 
  #     scale_y_continuous(labels = scales::comma) + xlab(label = "") +ylab(label="Tests performed") +
  #     theme_classic()+
  #     theme(axis.text=element_text(size=13),
  #           axis.title=element_text(size=16), 
  #           axis.title.x = element_text(vjust=-1.5),
  #           axis.title.y = element_text(vjust=2),
  #           legend.text = element_text(size=13),
  #           legend.position = 'top', 
  #           legend.spacing.x = unit(0.4, 'cm'),
  #           panel.grid.major.y=element_line(size=0.05)) +
  #     scale_colour_manual(name="",values = c("total_tests" = "#000000", "new_tests" = "#e41a1c","total_test_ppl"="#ff7f00", 
  #                                            "new_test_ppl"="#a65628"),
  #                         breaks=c("new_tests","total_tests","new_test_ppl","total_test_ppl"),
  #                         labels=c("Daily", "Cumulative", "Daily", "Cumulative"))
  #   p.test
  #   
  # })
  
  # Brazil state comparisons
  output$statePlot_compare_br <- renderPlot({
    lines_type <- c(as.character(input$checkGroup_br))
    
    data.br <- data.brazil[data.brazil$type %in% lines_type, ]
    
    lines2 <- c(as.character(input$checkGroup_stateCompare_br))
    
    data.br <- data.br[data.br$state_name %in% lines2, ]
    
    lab_y <- "Casos/óbitos relatados"

    
    if(input$compare_pop_br=="pop_no"){
      
      p2 <- ggplot(data.br) + geom_point(aes(x=date, y=number, col=state_name),size=1.5) +
        geom_line(aes(x=date, y=number, col=state_name,linetype=type),size=1) +
        scale_x_date(limits=c(input$dateRange_br[1],input$dateRange_br[2])) + scale_y_continuous(labels= scales::comma) + 
        ylab(label=paste(lab_y)) + xlab(label = "") +
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
        scale_linetype_manual(name="", values=c("total_cases"=1, "new_cases" = 2, "total_deaths" =3, "new_deaths"=4),
                                 breaks=c("total_cases","new_cases","total_deaths","new_deaths"),
                                 labels=c("Casos (total)","Casos (diários)","Óbitos (total)","Óbitos (diários)")) +
        guides(linetype = guide_legend(label.position = "top", keywidth = 2)) +
        theme(legend.direction = "horizontal",legend.box = "vertical") 
      if(input$log_compare_br=='log_yes'){
        p2 <- p2 + scale_y_log10()
      }
    } else{
      
      p2 <- ggplot(data.br) + geom_point(aes(x=date, y=number_pop, col=state_name),size=1.5) +
        geom_line(aes(x=date, y=number_pop, col=state_name,linetype=type),size=1) +
        scale_x_date(limits=c(input$dateRange_br[1],input$dateRange_br[2])) +
        ylab(label=paste(lab_y," (per 100,000)",sep="")) + xlab(label = "") +
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
        scale_linetype_manual(name="", values=c("total_cases"=1, "new_cases" = 2, "total_deaths" =3, "new_deaths"=4),
                              breaks=c("total_cases","new_cases","total_deaths","new_deaths"),
                              labels=c("Casos (total)","Casos (diários)","Óbitos (total)","Óbitos (diários)")) +
        guides(linetype = guide_legend(label.position = "top", keywidth = 2)) +
        theme(legend.direction = "horizontal",legend.box = "vertical")  
      if(input$log_compare_br=='log_yes'){
        p2 <- p2 + scale_y_log10()
      }
    }
    
    p2
  })
  
  
})



