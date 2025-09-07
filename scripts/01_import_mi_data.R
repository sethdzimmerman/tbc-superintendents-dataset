# Close all connections and clear environment
closeAllConnections()
rm(list = ls())

source(here::here("scripts/00_setup.R"))

library(foreign)

# Define Michigan data directory
mi_dir_path <- here("data", "raw", "mi")

files <- list.files(mi_dir_path, pattern = "*.DBF", full.names = T)

all <- data.frame()
for(f in files){
  raw <- read.dbf(f) %>% select(NCES, FNAME, LNAME) %>% 
    filter(is.na(NCES)==0) %>% 
    mutate(file = f)
  all <- bind_rows(raw, all)
  
}

#Map each file to a school year
filemap <- data.frame(file = files, 
                      short = basename(files))
filemap$date <- str_sub(filemap$short, 6, 15)
filemap$date <- gsub('[[:punct:] ]+','/',filemap$date) %>% as.Date()
filemap$year <- ifelse(lubridate::month(filemap$date) > 7,  
                       lubridate::year(filemap$date), 
                       lubridate::year(filemap$date) - 1)

all <- left_join(all, filemap)

#Take earliest file for each district and school year
all <- all %>% arrange(NCES, date) %>% 
  group_by(NCES, year) %>% mutate(n = row_number()) %>% 
  filter(n==1) %>% select(-n)

all$state <- "mi"
all$id <- paste0("mi",1:nrow(all))

all$name_raw <- paste(all$FNAME, all$LNAME)
all$name_clean <- clean_names(all$name_raw)

all$leaid <- as.numeric(as.character(all$NCES))

#Create table with names, district IDs, and years
all_supers <- all %>% ungroup() %>% 
  select(id, state, leaid, name_raw, name_clean, year, leaid)

# Save the processed data
save(all_supers, file = file.path(clean_path, "all_supers_mi.Rda"))

# data checks
data_checks(all_supers)
