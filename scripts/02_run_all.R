# Close all connections and clear environment
closeAllConnections()
rm(list = ls())

source(here::here("scripts/00_setup.R"))

# Updated States
  states <- c("ar", "ca", "ct", "ga", "il", "in", "mo", "ne",
              "oh", "ok", "or", "pa", "va", "ia", "nj", "wi",
              "ks", "ny", "tx", "mi")


# Run all import scripts
for (state in states) {
  script_path <- file.path("scripts", paste0("01_import_", state, "_data.R"))
  if (file.exists(script_path)) {
    message("Running: ", script_path)
    env <- new.env()
    # Quietly run the script in an isolated environment
    sys.source(script_path, envir = env)
  } else {
    warning("Script not found: ", script_path)
  }
}

# Load processed .Rda files and bind
supers_list <- list()

for (state in states) {
  rda_path <- file.path("data", "processed", paste0("all_supers_", state, ".Rda"))
  if (file.exists(rda_path)) {
    message("Loading: ", rda_path)
    load(rda_path)
    supers_list[[state]] <- get("all_supers")
  } else {
    warning("Processed file not found: ", rda_path)
  }
}

# Convert all leaid columns to character before binding
supers_list <- map(supers_list, ~ .x %>% mutate(leaid = as.character(leaid)))
all_supers <- bind_rows(supers_list, .id = "state")


summary(as.factor(all_supers$year))
summary(as.factor(all_supers$state))
summary(is.na(all_supers$salary))

# unique superintendent ID
all_supers <- all_supers %>%
  group_by(state, name_clean) %>%
  mutate(super_id = paste0(first(state), "_", cur_group_id())) %>%
  ungroup()

all_supers %>%
  summarise(unique_super_ids = n_distinct(super_id))

# padding 0s in LEAIDs
all_supers <- all_supers %>%
  mutate(leaid = ifelse(nchar(leaid) == 6, paste0("0", leaid), leaid))

# Save combined data
save(all_supers, file = file.path("data", "processed", "combined_superintendents.Rda"))
write.csv(all_supers, file = file.path("data", "processed", "combined_superintendents.csv"), row.names = FALSE)
