# ===============================================
# Load Libraries and Data
# ===============================================
library(tidyverse)
library(readxl)

# Read cleaned datasets
veh   <- read_excel("vehicle_clean.xlsx")
crash <- read_excel("vic_road_crash_data_clean.xlsx")

# Ensure consistent join key type
veh   <- veh   %>% mutate(ACCIDENT_NO = as.character(ACCIDENT_NO))
crash <- crash %>% mutate(ACCIDENT_NO = as.character(ACCIDENT_NO))

# Join to attach severity
veh_sev <- veh %>%
  inner_join(crash %>% select(ACCIDENT_NO, SEVERITY), by = "ACCIDENT_NO") %>%
  filter(!is.na(SEVERITY))

# Clean up severity labels
veh_sev <- veh_sev %>%
  mutate(
    SEVERITY = case_when(
      str_detect(SEVERITY, "Fatal") ~ "Fatal",
      str_detect(SEVERITY, "Serious") ~ "Serious injury",
      str_detect(SEVERITY, "Other") ~ "Other injury",
      str_detect(SEVERITY, "Non") ~ "Non injury",
      TRUE ~ SEVERITY
    ),
    SEVERITY = factor(SEVERITY,
                      levels = c("Fatal", "Serious injury", "Other injury", "Non injury"),
                      ordered = TRUE)
  )
