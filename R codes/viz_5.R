library(tidyverse)
library(readxl)
library(gtools)   # <-- add this line

crash <- read_excel("vic_road_crash_data_clean.xlsx")

crash_speed <- crash %>%
  filter(!is.na(SPEED_ZONE)) %>%
  mutate(
    SPEED_ZONE = as.character(SPEED_ZONE),
    SPEED_ZONE = str_remove_all(SPEED_ZONE, "km/hr|km/h|KM/HR"),
    SPEED_ZONE = trimws(SPEED_ZONE),
    SPEED_ZONE = if_else(SPEED_ZONE == "", "Unknown", SPEED_ZONE)
  ) %>%
  count(SPEED_ZONE, SEVERITY) %>%
  group_by(SPEED_ZONE) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup()

crash_speed$SPEED_ZONE <- factor(
  crash_speed$SPEED_ZONE,
  levels = mixedsort(unique(crash_speed$SPEED_ZONE))
)

ggplot(crash_speed, aes(x = SPEED_ZONE, y = prop, fill = SEVERITY)) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("red", "orange", "gold", "green")) +
  labs(
    title = "Crash Severity by Posted Speed Zone — Victoria",
    subtitle = "Higher-speed roads show a higher proportion of serious and fatal crashes",
    x = "Speed Zone (km/h)",
    y = "Proportion of Crashes",
    fill = "Severity"
  ) +
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

