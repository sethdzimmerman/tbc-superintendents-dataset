# Close all connections and clear environment
closeAllConnections()
rm(list = ls())

source(here::here("scripts/00_setup.R"))


# Define Connecticut data directory
ct_dir_path <- here("data", "raw", "ct")

#Loop through CT PDFs
files <- list.files(ct_dir_path, pattern = "*.pdf", full.names = T, recursive = T)

names_files <- data.frame()
for(f in files){
  print(f)
  alltext <- pdf_text(f) %>% tolower()
  
  #Find first instance of ", superintendent"
  super_pos <- str_locate_all(alltext, ", superintendent")[[1]]
  alltext_start <- str_sub(alltext[1], 1,super_pos[1]-1)
  
  #Take last "line" of this text string
  lines_pos <- str_locate_all(alltext_start, "\n")[[1]]
  lines_pos <- lines_pos[nrow(lines_pos),]
  name <- str_sub(alltext_start, lines_pos[1] + 1, 999) %>% trimws(.)
  
  #Find name of district
  dist_pos <- str_locate_all(alltext, "school district")[[1]]
  dist_start <- str_sub(alltext[1],1,dist_pos[1]+nchar("school district"))

  # Remove trailing newlines first
  dist_start <- str_replace_all(dist_start, "\n+$", "")

  lines_pos <- str_locate_all(dist_start, "\n")[[1]]

  if(nrow(lines_pos) > 0){
    lines_pos <- lines_pos[nrow(lines_pos),]
    dist <- str_sub(dist_start, lines_pos[2] + 1, -1) %>% trimws()
  } else {
    dist <- gsub("\\s+", " ", trimws(dist_start)) %>% trimws()
  }

  temp <- data.frame(file = paste(basename(dirname(f)), basename(f), sep = "/"),
                     dist = dist,
                     name = name)

  names_files <- bind_rows(names_files, temp)

}

summary(is.na(names_files$dist))

# assign year
names_files$year <- str_sub(sub(".*PDFs/([0-9_]+)/.*", "\\1", names_files$file),1,4) %>% as.numeric()

names_files$year[is.na(names_files$year)] <- sapply(
  names_files$file[is.na(names_files$year)], 
  function(file_path) {
    filename <- basename(file_path)
    as.numeric(str_sub(filename, 1, 4))
  }
)


# Load CT LEAID to file map
# These are manually mapped using the filenames
dist_map <- read_excel(file.path(ct_dir_path, "CT_District_Map.xlsx"))

all_ct_lea <- left_join(names_files, dist_map, by = "file")


all_ct_lea$state <- "CT"
all_ct_lea$id <- paste0("ct",1:nrow(all_ct_lea))
summary(is.na(all_ct_lea$leaid))

# Extend LEAID to observations with the same district name 
# First, create a lookup table of district names to their unique non-NA LEAIDs
all_ct_lea <- all_ct_lea %>%
  mutate(leaid = as.character(leaid))

# Create a lookup table of district names to their unique non-NA LEAIDs
district_leaid_lookup <- all_ct_lea %>%
  filter(!is.na(leaid)) %>%           # Keep only rows with non-missing LEAIDs
  group_by(dist) %>%                  # Group by district name
  summarize(
    unique_leaids = n_distinct(leaid), # Count number of unique LEAIDs per district
    consistent_leaid = if(unique_leaids == 1) first(leaid) else NA_character_  # Use if-else instead of ifelse
  ) %>%
  filter(!is.na(consistent_leaid))    # Keep only districts with consistent LEAIDs

# Now fill in the missing LEAIDs
all_ct_lea <- all_ct_lea %>%
  left_join(district_leaid_lookup %>% select(dist, consistent_leaid), by = "dist") %>%
  mutate(
    leaid = ifelse(is.na(leaid) & !is.na(consistent_leaid), consistent_leaid, leaid)
  ) %>%
  select(-consistent_leaid)  # Remove the temporary column

# Check how many missing LEAIDs remain
summary(is.na(all_ct_lea$leaid))

sum(!is.na(all_ct_lea$name) & is.na(all_ct_lea$leaid))

## export missing leaids to excel to hand match 
# missing_leaid_data <- all_ct_lea %>%
#   filter(is.na(leaid) & !is.na(name))
# write_xlsx(missing_leaid_data, "missing_leaid_observations.xlsx")

# Clean names
all_ct_lea$name_raw <- all_ct_lea$name
all_ct_lea$name_clean <- clean_names(all_ct_lea$name_raw)

# Create table with names, district IDs, and years
all_supers <- all_ct_lea %>% 
  filter(is.na(name_raw)==0) %>% 
  select(id, state, leaid, name_raw, name_clean, year, leaid)

save(all_supers, file = file.path(clean_path, "all_supers_ct.Rda"))

# data checks 
data_checks(all_supers)