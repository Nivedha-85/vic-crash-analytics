df %>%
  count(DAY_OF_WEEK, SEVERITY) %>%
  ggplot(aes(x = DAY_OF_WEEK, y = n, fill = SEVERITY)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("red","orange","yellow","green")) +
  labs(title = "Relative Severity by Day of Week",
       x = "Day of Week", y = "Percentage of Crashes") +
  theme_minimal(base_size = 13)



df %>%
  group_by(AGE_GROUP, SEX, SEVERITY) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(prop = n / sum(n)) %>%
  ggplot(aes(x = AGE_GROUP, y = prop, fill = SEVERITY)) +
  geom_col(position = "fill") +
  facet_wrap(~ SEX) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Severity Distribution by Age & Gender",
       x = "Age Group", y = "Percentage of Crashes", fill = "Severity") +
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

