# LOAD REQUIRED PACKAGES AND FUNCTIONS -----------------------------------------
if (!require("pacman")) install.packages("pacman")
pkgs = c("dplyr") # package names
pacman::p_load(pkgs, character.only = T)
library(tidyverse)

#=======#### READ DATA IN ####=======#
# Brazil states
data.brazil <- read_csv("https://raw.githubusercontent.com/wcota/covid19br/master/cases-brazil-states.csv")

#=======#### DATA PROCESSING ####=======#
# List of Brazilian states
states.names <- tibble(ID=c("SP","RJ","CE","AM","SC","MG","PE","PR","RS","BA","DF","ES","MA","RN","PA","GO","AP","MT","MS","PB","RR","AC","AL","SE","PI","RO","TO"),
                      state_name=c("São Paulo","Rio de Janeiro","Ceará","Amazonas","Santa Catarina","Minas Gerais","Pernambuco","Paraná","Rio Grande do Sul",
                      "Bahia","Distrito Federal","Espírito Santo","Maranhão","Rio Grande do Norte","Pará","Goiás","Amapá","Mato Grosso","Mato Grosso do Sul",
                      "Paraíba","Roraima","Acre","Alagoas","Sergipe","Piauí","Rondônia","Tocantins"),
                      pop=c(44396484,16550024,8904459,3938336,6819190,20869101,9345173,11163018,11247972,15203934,2914830,3929911,6904241,
                            3442175,8175113,6610681,766679,3265486,2651235,3972202,505665,803513,3340932,2242937,3204028,1768204,1515126))

# Add zeros for early dates
extra.dates <- data.brazil[1,]
extra.dates[1,2:ncol(extra.dates)] <- 0

date.min <- min(data.brazil$date)
min.dates <- data.brazil %>%
  group_by(state) %>%
  summarise(date = min(date), diff = min(date) - date.min)

date.max <- max(min.dates$date)
n <- as.numeric(date.max - date.min)

for (i in 1:n){
  extra.dates[1+i,2:ncol(extra.dates)] <- 0
  extra.dates[1+i,1] <- extra.dates[i,1] +1
}

out <- tibble()
for (i in 1:nrow(states.names)){
  x <- data.brazil[data.brazil$state==states.names$ID[i],]
  diff <- as.numeric(min.dates$diff[min.dates$state==states.names$ID[i]])
  if (diff>0){
    dat <- extra.dates[1:diff,]
    dat$state <- states.names$ID[i]
    x <- rbind(dat,x)
  }
  out <- rbind (out, x)
}

## Sort and combine into dataframe
data.brazil <- out %>%
  filter(state!="TOTAL")%>%
  left_join(states.names, by=c("state"="ID")) %>%
  select(date, state, state_name, pop, total_cases = totalCases, new_cases = newCases, total_deaths = deaths, new_deaths=newDeaths) %>%
  gather(key="type", value="number",-date, -state, -state_name, -pop) %>%
  mutate(number_pop=100000*number/pop)


# 
# # SAVE OUTPUT ------------------------------------------------------------------
readr::write_csv(data.brazil, 
                  paste0("data/processed/brazil_full.csv"),append=FALSE)


# DELETE OLD VERSIOSN ----------------------------------------------------------
# case_data_files <- list.files("data/processed/", 
# 															pattern = ".csv",
# 															full.names = T)
# to_delete <- case_data_files[!grepl(last_day, case_data_files)]
# file.remove(to_delete)