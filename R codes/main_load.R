# Packages
install.packages(c("tidyverse","readxl","lubridate"), dependencies = TRUE)

library(tidyverse)
library(readxl)
library(lubridate)

# === Load cleaned Excel files ===
# If you saved as separate files:
crash <- read_excel("vic_road_crash_data_clean.xlsx")
veh   <- read_excel("vehicle_clean.xlsx")
pers  <- read_excel("person_clean.xlsx")

# Safety: parse date + derive year-month
crash <- crash %>%
  mutate(
    accident_date = dmy(ACCIDENT_DATE),
    year  = year(accident_date),
    month = floor_date(accident_date, "month")
  )

# A helper “severity_ordered” for consistent plotting
crash <- crash %>%
  mutate(
    severity_ordered = factor(
      SEVERITY,
      levels = c("Fatal", "Serious injury", "Other injury", "Non injury"),
      ordered = TRUE
    )
  )

