library(tidyverse)

ggplot(
  veh_sev %>%
    filter(!is.na(CUBIC_CAPACITY), CUBIC_CAPACITY < quantile(CUBIC_CAPACITY, 0.99, na.rm = TRUE)),  # trim outliers
  aes(x = CUBIC_CAPACITY, fill = SEVERITY)
) +
  geom_histogram(position = "fill", bins = 30) +
  scale_fill_manual(values = c("red", "orange", "gold", "green")) +
  labs(
    title = "Crash Severity by Engine Capacity",
    subtitle = "Higher engine displacement vehicles show different severity patterns",
    x = "Engine Capacity (cc)",
    y = "Proportion of Crashes",
    fill = "Severity"
  ) +
  theme_minimal(base_size = 13)


ggplot(
  veh_sev %>%
    filter(!is.na(TARE_WEIGHT), TARE_WEIGHT < quantile(TARE_WEIGHT, 0.99, na.rm = TRUE)),
  aes(x = TARE_WEIGHT, fill = SEVERITY)
) +
  geom_histogram(position = "fill", bins = 30) +
  scale_fill_manual(values = c("red", "orange", "gold", "green")) +
  labs(
    title = "Crash Severity by Vehicle Weight",
    x = "Vehicle Weight (kg)",
    y = "Proportion of Crashes",
    fill = "Severity"
  ) +
  theme_minimal(base_size = 13)
