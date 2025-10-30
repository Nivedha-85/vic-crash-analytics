veh_body <- veh_sev %>%
  filter(!is.na(VEHICLE_BODY_STYLE)) %>%
  count(VEHICLE_BODY_STYLE, SEVERITY) %>%
  group_by(VEHICLE_BODY_STYLE) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup()

top_body <- veh_body %>%
  count(VEHICLE_BODY_STYLE, wt = n, sort = TRUE) %>%
  slice_head(n = 10) %>%
  pull(VEHICLE_BODY_STYLE)

ggplot(veh_body %>% filter(VEHICLE_BODY_STYLE %in% top_body),
       aes(x = reorder(VEHICLE_BODY_STYLE, -prop), y = prop, fill = SEVERITY)) +
  geom_col(position = "fill") +
  coord_flip() +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("red", "orange", "gold", "green")) +
  labs(
    title = "Crash Severity by Vehicle Body Style (Top 10)",
    x = "Body Style",
    y = "Proportion of Crashes",
    fill = "Severity"
  ) +
  theme_minimal(base_size = 13)
