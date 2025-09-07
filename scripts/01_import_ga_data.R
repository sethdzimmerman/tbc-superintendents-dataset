# Close all connections and clear environment
closeAllConnections()
rm(list = ls())

source(here::here("scripts/00_setup.R"))

# Define Georgia data directory
ga_dir_path <- here("data", "raw", "ga")

# Import Georgia raw data OLD
ga_raw1 <- read_delim(
  file.path(ga_dir_path, "SalaryTravelDataExportAllYears_old.txt"), 
  delim = ",")

# Import Georgia raw data NEW
ga_raw2 <- read_delim(
  file.path(ga_dir_path, "SalaryTravelDataExportAllYears.txt"), 
  delim = ",",
  quote = "'"  
)

# Append the data frames and remove duplicates
ga_raw <- bind_rows(ga_raw1, ga_raw2) %>%
  distinct()

all_ga_stf <- ga_raw %>% 
  select(name_raw = NAME, position_raw = TITLE, salary = SALARY, 
         year = FISCAL_YEAR, organization = ORGANIZATION) %>% 
  mutate(state = "GA")

# From: https://gbpi.org/georgia-revenue-primer-for-state-fiscal-year-2022/
# "Georgia's 2022 fiscal year runs from July 1, 2021 through June 30, 2022."
# So, fiscal years in my data (e.g. FY 2013) correspond to school year of the prior year (e.g. school year 2012)
all_ga_stf$year <- all_ga_stf$year - 1

all_ga_stf <- all_ga_stf %>% select(state, year, organization, name_raw, position_raw, salary)

# Restrict to superintendents
ga_supers <- all_ga_stf %>% filter(position_raw == "SUPERINTENDENT")

#In cases with >1 superintendent per district per year, take the observation with the highest salary
ga_supers <- ga_supers %>% group_by(year, organization) %>% 
  mutate(rank_sal = rank(-salary)) %>% 
  filter(rank_sal == 1) %>% 
  select(-rank_sal)

# Map district IDs to LEAIDs
# Initialize an empty data frame
ga_distids <- data.frame()
years <- 2009:2023

# Loop through years to load and process data
for(y in years){
  print(y)
  
  # Load Rda file
  load(file.path(dist_chars_path, paste0("chars_", y, ".Rda")))
  df <- get(paste0("chars_", y))
  
  # Process the data
  temp <- df %>% 
    filter(fips == "Georgia") %>% 
    select(year, leaid, state_leaid, nces_lea_name = lea_name, agency_charter_indicator, enrollment) %>% 
    mutate(leaid = parse_number(leaid))
  
  ga_distids <- bind_rows(ga_distids, temp)
  
  # Remove the loaded object
  rm(list = paste0("chars_", y))
}

ga_distids$state_leaid_clean <- str_remove_all(str_remove_all(ga_distids$state_leaid, "GA"),"-")
ga_distids$state_leaid_clean <- as.numeric(ga_distids$state_leaid_clean)

ga_distids$dist_name <- tolower(ga_distids$nces_lea_name)
ga_distids$dist_name<- str_remove_all(ga_distids$dist_name, "state charter schools- ")
ga_distids$dist_name<- str_remove_all(ga_distids$dist_name, "commission charter schools- ")
ga_distids$dist_name <- ifelse(ga_distids$dist_name=="dalton public schools",
                               "dalton city",ga_distids$dist_name)

ga_supers$city <- ifelse(str_sub(ga_supers$organization, 1, 4)=="CITY",1,0)
ga_supers$dist_name <- str_remove_all(ga_supers$organization, " BOARD OF EDUCATION")
ga_supers$dist_name <- str_remove_all(ga_supers$dist_name, " SCHOOL DISTRICT")
ga_supers$dist_name <- str_remove_all(ga_supers$dist_name, "CITY OF ")
ga_supers$dist_name <- tolower(ga_supers$dist_name)
ga_supers$dist_name <- ifelse(ga_supers$city, 
                              paste0(ga_supers$dist_name, " city"), 
                              ga_supers$dist_name)
ga_supers$dist_name <- ifelse(ga_supers$dist_name=="atlanta independent school system", 
                              "atlanta public schools", ga_supers$dist_name)
ga_supers$dist_name <- ifelse(ga_supers$dist_name=="polk", 
                              "polk county", ga_supers$dist_name)
ga_supers$dist_name <- ifelse(ga_supers$dist_name=="thomaston - upson county", 
                              "thomaston-upson county", ga_supers$dist_name)

ga_supers_lea <- left_join(ga_supers, ga_distids, by = c("dist_name","year"))

ga_supers_lea$leaid <- ifelse(ga_supers_lea$dist_name=="atlanta heights charter school",1300221,ga_supers_lea$leaid)
ga_supers_lea$leaid <- ifelse(ga_supers_lea$dist_name=="decatur city",1301680,ga_supers_lea$leaid)
ga_supers_lea$leaid <- ifelse(ga_supers_lea$dist_name=="griffin - spalding county",1302520,ga_supers_lea$leaid)
ga_supers_lea$leaid <- ifelse(ga_supers_lea$dist_name=="ivy prep academy at kirkwood for girls school",1300226,ga_supers_lea$leaid)
ga_supers_lea$leaid <- ifelse(ga_supers_lea$dist_name=="ivy preparatory young men's leadership academy school",1300229,ga_supers_lea$leaid)
ga_supers_lea$leaid <- ifelse(ga_supers_lea$dist_name=="mountain education center",1300214,ga_supers_lea$leaid)
ga_supers_lea$leaid <- ifelse(ga_supers_lea$dist_name=="provost academy of georgia",1300231,ga_supers_lea$leaid)
ga_supers_lea$leaid <- ifelse(ga_supers_lea$dist_name=="savannah-chatham county",1301020,ga_supers_lea$leaid)

# Clean names
ga_supers_lea$name_clean <- clean_names(ga_supers_lea$name_raw)

# Create IDs
ga_supers_lea$id <- paste0("ga", 1:nrow(ga_supers_lea))

# Create table with names, district IDs, and years
all_supers <- ga_supers_lea %>% 
  filter(!is.na(leaid)) %>% 
  ungroup() %>% select(id, state, leaid, name_raw, name_clean, year, salary)

save(all_supers, file = file.path(clean_path, "all_supers_ga.Rda"))

# data checks 
data_checks(all_supers)
