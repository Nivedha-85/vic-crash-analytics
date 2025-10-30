# ===================================================
# 1️⃣  Setup
# ===================================================
library(tidyverse)
library(readxl)
library(lubridate)

# Load cleaned datasets
crash  <- read_excel("vic_road_crash_data_clean.xlsx")
person <- read_excel("person_clean.xlsx")

# Prepare crash dates & join with person demographics
crash <- crash %>%
  mutate(
    ACCIDENT_DATE = dmy(ACCIDENT_DATE),
    YEAR  = year(ACCIDENT_DATE),
    MONTH = month(ACCIDENT_DATE, label = TRUE)
  )

df <- person %>%
  select(ACCIDENT_NO, SEX, AGE_GROUP, ROAD_USER_TYPE_DESC) %>%
  inner_join(crash %>% select(ACCIDENT_NO, SEVERITY, DAY_OF_WEEK, YEAR, MONTH, DTP_REGION, DEG_URBAN_NAME),
             by = "ACCIDENT_NO")

df %>%
  count(MONTH, SEVERITY) %>%
  ggplot(aes(x = MONTH, y = n, fill = SEVERITY)) +
  geom_col(position = "stack") +
  scale_fill_manual(values = c("red","orange","yellow","green")) +
  labs(title = "Crash Severity by Month",
       x = "Month", y = "Crash Count", fill = "Severity") +
  theme_minimal(base_size = 13)

