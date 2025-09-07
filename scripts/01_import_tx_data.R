# Close all connections and clear environment
closeAllConnections()
rm(list = ls())
source(here::here("scripts/00_setup.R"))
library(pdftools)
library(stringr)
library(dplyr)

# Define Texas data directory
tx_dir_path <- here("data", "raw", "tx")
files <- paste0("tx_",2007:2022,".pdf")

# Updated extract function 
extract_table_from_text <- function(text_lines, area = NULL) {
  # Remove empty lines
  text_lines <- text_lines[nchar(trimws(text_lines)) > 0]
  
  # Split each line into columns based on multiple spaces or tabs
  table_data <- lapply(text_lines, function(line) {
    # Split on multiple spaces (2 or more) or tabs
    cols <- str_split(line, "\\s{2,}|\\t")[[1]]
    # Remove empty strings and trim whitespace
    cols <- trimws(cols[nchar(trimws(cols)) > 0])
    return(cols)
  })
  
  # Filter out lines that don't have at least 5 columns (changed from 6)
  # This will capture both 5-column (2018-2019) and 6-column (other years) formats
  table_data <- table_data[sapply(table_data, length) >= 5]
  
  # Convert to matrix, handling both formats
  if(length(table_data) > 0) {
    # If we have 5-column format, we need to split the phone/county column
    table_data <- lapply(table_data, function(row) {
      if(length(row) == 5) {
        # Split the phone/county column (column 2)
        phone_county <- row[2]
        
        # Extract phone number (in parentheses) and county-district number
        phone_match <- str_extract(phone_county, "\\([0-9]{3}\\) [0-9]{3}-[0-9]{4}")
        
        # Remove phone number first, then extract county-district
        if(!is.na(phone_match)) {
          # Remove phone number and any leading/trailing spaces
          remaining <- str_replace(phone_county, "\\([0-9]{3}\\) [0-9]{3}-[0-9]{4}", "")
          remaining <- trimws(remaining)
          # Extract county-district number: exactly 3 digits, dash, exactly 3 digits at the start
          # This ensures we don't match zip codes like 79431-0120 (which have 5 digits before dash)
          county_match <- str_extract(remaining, "^[0-9]{3}-[0-9]{3}(?=\\s|$)")
        } else {
          county_match <- NA
        }
        
        if(!is.na(phone_match) && !is.na(county_match)) {
          # Reconstruct as 6 columns
          new_row <- c(row[1], phone_match, county_match, row[3], row[4], row[5])
          return(new_row)
        }
      }
      return(row)
    })
    
    # Now standardize to 6 columns
    max_cols <- 6
    table_matrix <- do.call(rbind, lapply(table_data, function(row) {
      if(length(row) < max_cols) {
        c(row, rep("", max_cols - length(row)))
      } else {
        row[1:max_cols]
      }
    }))
    return(list(table_matrix))
  } else {
    return(list())
  }
}

###########################################################################
# Extract tables and save them 
for(f in files){
  print(f)
  file_path <- file.path(tx_dir_path, f)
  
  # Check if file exists
  if(!file.exists(file_path)) {
    print(paste("File does not exist:", file_path))
    next
  }
  
  pages <- pdf_info(file_path)$pages
  
  for(p in 1:pages){
    # Extract text from the specific page
    page_text <- pdf_text(file_path)[p]
    
    # Split text into lines
    text_lines <- str_split(page_text, "\n")[[1]]
    
    # Extract table data (you may need to adjust this based on your PDF structure)
    table_list <- extract_table_from_text(text_lines)
    
    # Create Tables directory if it doesn't exist
    tables_dir <- file.path(tx_dir_path, "Tables")
    if(!dir.exists(tables_dir)) {
      dir.create(tables_dir, recursive = TRUE)
    }
    
    # Save the extracted table
    save(table_list, file = file.path(tables_dir, paste0(f,"_",p,"_tables.Rda")))
  }
}

# Process the extracted tables (equivalent to your second loop)
tx_out <- data.frame()
for(f in files){
  print(f)
  file_path <- file.path(tx_dir_path, f)
  
  # Check if file exists
  if(!file.exists(file_path)) {
    print(paste("File does not exist:", file_path))
    next
  }
  
  pages <- pdf_info(file_path)$pages
  
  for(p in 1:pages){
    table_file <- file.path(tx_dir_path, "Tables", paste0(f,"_",p,"_tables.Rda"))
    
    if(file.exists(table_file)) {
      load(table_file)
      
      if(length(table_list) > 0 && nrow(table_list[[1]]) > 0){
        df <- data.frame(table_list[[1]])
        
        # Ensure we have the expected number of columns
        expected_cols <- 6
        if(ncol(df) < expected_cols) {
          # Add missing columns
          for(i in (ncol(df) + 1):expected_cols) {
            df[paste0("X", i)] <- ""
          }
        }
        
        # Rename columns to match your original format
        if(ncol(df) >= expected_cols) {
          names(df)[1:expected_cols] <- paste0("X", 1:expected_cols)
        }
        
        df$file <- f
        df$page <- p
        tx_out <- bind_rows(tx_out, df)
      } else{
        print(paste0("No table on page ", p, " of ", pages))
      }
    } else {
      print(paste0("Table file not found for page ", p, " of ", f))
    }
  }
}

# Clean file 
tx_out$emptyrow <- ifelse(rowSums(tx_out=="")==6,1,0)
tx_out$headerrow <- ifelse(tx_out$X1=="School district" | 
                             tx_out$X2=="Phone",1,0)
tx_clean_old <- tx_out %>% filter(emptyrow==0, headerrow==0) %>% select(-emptyrow, -headerrow)

# Only rename columns if we have the expected number
if(ncol(tx_clean_old) >= 8) {
  colnames(tx_clean_old) <- c("dist_name","dist_phone","cty_distno",
                          "address","zip","name_raw","file","page")
}

# Add years
# Convert spring years to fall by subtracting 1
tx_clean_old$year <- parse_number(str_sub(tx_clean_old$file, 4, 7)) - 1


# fix zip + name column mix 
tx_clean_old <- tx_clean_old %>%
  mutate(
    # Extract the numeric part (zip code) - keep numbers, dashes, and spaces only
    zip_fixed = str_extract(zip, "^[0-9\\-\\s]+"),
    
    # Extract the non-numeric part (name that got mixed in)
    name_from_zip = str_extract(zip, "(?<=[0-9\\-\\s])[A-Za-z].*$"),
    
    # Update zip column to only contain the numeric part
    zip = str_trim(zip_fixed),
    
    # Update name_raw: if name_raw is empty/NA and we found a name in zip, use that
    # Otherwise, if name_raw has content and we found additional name in zip, combine them
    name_raw = case_when(
      # If name_raw is empty or NA, use the name from zip
      (is.na(name_raw) | name_raw == "") & !is.na(name_from_zip) ~ str_trim(name_from_zip),
      # If both have content, combine them
      !is.na(name_raw) & name_raw != "" & !is.na(name_from_zip) ~ str_trim(paste(name_raw, name_from_zip)),
      # Otherwise keep original name_raw
      TRUE ~ name_raw
    )
  ) %>%
  # Remove the temporary columns
  select(-zip_fixed, -name_from_zip)

summary(as.factor(tx_clean_old$file))

###########################################################################
# read in 2022-2024 from excel sheets 

# Initialize empty dataframe
tx_clean_new <- data.frame()

# Loop through years 2023-2025
for(year in 2023:2025) {
  # Construct filename
  filename <- paste0("TSD-", year, "-final.xlsx")
  filepath <- file.path(tx_dir_path, filename)
  
  # Print progress
  print(paste("Processing:", filename))
  
  # Check if file exists before trying to read
  if(file.exists(filepath)) {
    # Read excel file
    df <- read_excel(filepath, sheet = "Index of Districts and Charters")
    
    # Keep only columns 1, 3, 6 and rename them
    df_clean <- df %>%
      select(1, 3, 6) %>%
      rename(dist_name = 1, cty_distno = 2, name_raw = 3)
    
    # Add file and year columns
    df_clean$file <- filename
    df_clean$year <- year - 1
    df_clean <- df_clean %>% filter(dist_name != "School district")
    
    # Append to main df
    tx_clean_new <- bind_rows(tx_clean_new, df_clean)
    
  } else {
    print(paste("File not found:", filename))
  }
}

#Bind all years 
tx_clean <- bind_rows(tx_clean_old, tx_clean_new)

summary(as.factor(tx_clean$year))
summary(as.factor(tx_clean_old$year))


################################################################################
# Map district IDs to LEAIDs
# Initialize an empty data frame
tx_distids <- data.frame()
years <- 2006:2024

# Loop through years to load and process data
for(y in years){
  print(y)
  
  # Load Rda file
  load(file.path(dist_chars_path, paste0("chars_", y, ".Rda")))
  df <- get(paste0("chars_", y))
  
  # Process the data
  temp <- df %>% 
    filter(fips == "Texas") %>% 
    select(year, leaid, state_leaid, nces_lea_name = lea_name, agency_charter_indicator, enrollment) %>% 
    mutate(leaid = as.character(leaid))
  
  tx_distids <- bind_rows(tx_distids, temp)
  
  # Remove the loaded object
  rm(list = paste0("chars_", y))
}

# Clean state_ids
tx_distids$len <- nchar(tx_distids$state_leaid)
tx_distids$cty_distno <- paste0(str_sub(tx_distids$state_leaid,tx_distids$len-5,tx_distids$len-3), 
                                "-",
                                str_sub(tx_distids$state_leaid,tx_distids$len-2,tx_distids$len))

tx_clean_lea <- inner_join(tx_clean, tx_distids, by = c("cty_distno", "year"))

# Inspect unmatched
unmatched <- anti_join(tx_clean, tx_distids, by = c("cty_distno", "year")) %>% 
  # Remove non-named superintendents
  filter(name_raw!="")
sort(table(unmatched$dist_name))

# Clean names
tx_clean_lea$name_clean <- clean_names(tx_clean_lea$name_raw)


tx_clean_lea$state <- "TX"
tx_clean_lea$id <- paste0("tx",1:nrow(tx_clean_lea))

#Create table with names, district IDs, and years
all_supers <- tx_clean_lea %>% select(id, state, leaid, name_raw, name_clean, year, leaid)

summary(as.factor(all_supers$year))
summary(as.factor(unmatched$year))

# drop missing 
all_supers <- all_supers %>% filter(!is.na(name_clean)) %>%
                             filter(name_clean!="")

# Save the processed data
save(all_supers, file = file.path(clean_path, "all_supers_tx.Rda"))

# data checks
data_checks(all_supers)