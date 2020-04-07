# LOAD REQUIRED PACKAGES AND FUNCTIONS -----------------------------------------
if (!require("pacman")) install.packages("pacman")
pkgs = c("dplyr") # package names
pacman::p_load(pkgs, character.only = T)

# HARVEST DATA FROM PHE --------------------------------------------------------

# Link to historical cumalative daily cases for UTLA
url <- "https://fingertips.phe.org.uk/documents/Historic%20COVID-19%20Dashboard%20Data.xlsx"

# Download file and load it in R
download.file(url, destfile = "data/original/PHE_main_data.xlsx")

# LOAD FROM PHE
UK_total <- readxl::read_xlsx(path = "data/original/PHE_main_data.xlsx", 
                           sheet = 2, 
                           skip = 8)
UK_countries_deaths <- readxl::read_xlsx(path = "data/original/PHE_main_data.xlsx", 
                                      sheet = 3, 
                                      skip = 7)

UK_countries_cases <- readxl::read_xlsx(path = "data/original/PHE_main_data.xlsx", 
                                         sheet = 4, 
                                         skip = 7)

england_region <- readxl::read_xlsx(path = "data/original/PHE_main_data.xlsx", 
                                    sheet = 5, 
                                    skip = 7)

england_countyUA <- readxl::read_xlsx(path = "data/original/PHE_main_data.xlsx", 
                                    sheet = 6, 
                                    skip = 7)
UK_total_original <- read.csv("data/original/UK_total.csv")
UK_total_original$date <- as.Date(UK_total_original$date, "%d/%m/%Y")

# DATA PROCESSING --------------------------------------------------------------

# UK_total
UK_total <- UK_total %>%
  rename(date = Date, total_cases = `Cumulative Cases`)
dat <- data.frame(date=UK_total$date[UK_total$date>"2020-03-09"], total_cases = UK_total$total_cases[UK_total$date>"2020-03-09"],
                  total_deaths=UK_countries_deaths$UK[UK_countries_deaths$Date>"2020-03-09"])

UK_total <- rbind(UK_total_original,dat)

# UK_countries_deaths
UK_countries_deaths <- UK_countries_deaths %>%
  select(-Deaths)

# UK_countries_cases
UK_countries_cases <- UK_countries_cases %>%
  select(-`Area Code`, country = `Area Name`) %>%
  filter(country!= "UK") %>%
  tidyr::gather(key="date", value="cum_cases", -country) %>%
  mutate(date=as.Date(as.numeric(date), origin="1899-12-30"))
 
# # Convert to long format and 
# cases_long <- cases %>% 
#   filter(area_name != "England") %>% 
#   tidyr::gather(key = "date", value = "cum_cases", -area_code, -area_name)
# 
# # Covert date from excel format to standard format
# cases_long$date <- as.Date(as.numeric(cases_long$date), origin = "1899-12-30")
# 
# # Convert gain to wide format
# cases_wide <- cases_long %>% 
#   tidyr::spread(key = date, value = cum_cases)
# 
# # SAVE OUTPUT ------------------------------------------------------------------
# 
# # last_day <- names(cases_wide)[ncol(cases_wide)]
# 
# readr::write_csv(cases_long, 
#                  paste0("data/processed/cases_long.csv"))
# 
# readr::write_csv(cases_wide, 
#                  paste0("data/processed/cases_wide.csv"))

# DELETE OLD VERSIOSN ----------------------------------------------------------
# case_data_files <- list.files("data/processed/", 
# 															pattern = ".csv",
# 															full.names = T)
# to_delete <- case_data_files[!grepl(last_day, case_data_files)]
# file.remove(to_delete)