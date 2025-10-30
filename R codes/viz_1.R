ts_month <- crash %>%
  count(month, name = "n") %>%
  arrange(month) %>%
  mutate(roll_mean_3m = zoo::rollmean(n, k = 3, fill = NA, align = "right"))

ggplot(ts_month, aes(month, n)) +
  geom_line(linewidth = 0.7) +
  geom_line(aes(y = roll_mean_3m), linewidth = 1, linetype = 2) +
  labs(
    title = "Monthly Road Crashes in Victoria",
    x = "Month", y = "Crashes",
    caption = "Dashed = 3-month rolling average"
  ) +
  theme_minimal(base_size = 12)

