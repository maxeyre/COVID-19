# LOAD REQUIRED PACKAGES AND FUNCTIONS -----------------------------------------
if (!require("pacman")) install.packages("pacman")
pkgs = c("dplyr") # package names
pacman::p_load(pkgs, character.only = T)

# HARVEST DATA FROM PHE --------------------------------------------------------

# Link to historical cumalative daily cases for UTLA
url <- "https://fingertips.phe.org.uk/documents/Historic%20COVID-19%20Dashboard%20Data.xlsx"

# Download file and load it in R
download.file(url, destfile = "data/original/PHE_main_data.xlsx")
cases <- readxl::read_xlsx(path = "data/original/daily_cum_cases.xlsx", 
                           sheet = 6, 
                           skip = 7) 

# DATA PROCESSING --------------------------------------------------------------

# Give robust column names
# names(cases)[1:2] <- c("area_code", "area_name")
# 
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