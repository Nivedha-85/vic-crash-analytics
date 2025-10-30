veh_make <- veh_sev %>%
  count(VEHICLE_MAKE, SEVERITY) %>%
  group_by(VEHICLE_MAKE) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup()

top_makes <- veh_make %>%
  count(VEHICLE_MAKE, wt = n, sort = TRUE) %>%
  slice_head(n = 10) %>%
  pull(VEHICLE_MAKE)

ggplot(veh_make %>% filter(VEHICLE_MAKE %in% top_makes),
       aes(x = reorder(VEHICLE_MAKE, -prop), y = prop, fill = SEVERITY)) +
  geom_col(position = "fill") +
  coord_flip() +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("red", "orange", "gold", "green")) +
  labs(
    title = "Crash Severity by Vehicle Make (Top 10)",
    x = "Make",
    y = "Proportion of Crashes",
    fill = "Severity"
  ) +
  theme_minimal(base_size = 13)

