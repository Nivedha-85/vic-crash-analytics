# ───────────────────────────────────────────────
#  Crash Severity Composition by Vehicle Type
# ───────────────────────────────────────────────
suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
  library(forcats)
  library(ggplot2)
  library(scales)
})

# 1️⃣ Normalise severity text
veh_sev <- veh_sev %>%
  mutate(SEVERITY = str_to_lower(str_trim(SEVERITY))) %>%
  mutate(SEVERITY = case_when(
    str_detect(SEVERITY, "fatal") ~ "Fatal accident",
    str_detect(SEVERITY, "serious") ~ "Serious injury accident",
    str_detect(SEVERITY, "other") ~ "Other injury accident",
    str_detect(SEVERITY, "non") ~ "Non injury accident",
    TRUE ~ "Other injury accident"
  ))

# 2️⃣ Order & colour palette
severity_levels <- c("Non injury accident",
                     "Other injury accident",
                     "Serious injury accident",
                     "Fatal accident")

severity_palette <- c(
  "Non injury accident"     = "#81C784",  # green
  "Other injury accident"   = "#FFF176",  # yellow
  "Serious injury accident" = "#FB8C00",  # orange
  "Fatal accident"          = "#E53935"   # red
)

# 3️⃣ Prepare data
veh_comp <- veh_sev %>%
  mutate(VehicleType = str_squish(VEHICLE_TYPE_DESC)) %>%
  filter(!is.na(VehicleType),
         VehicleType != "",
         !str_detect(VehicleType, regex("unknown|not applicable|not known", TRUE))) %>%
  count(VehicleType, SEVERITY, name = "n") %>%
  group_by(VehicleType) %>%
  mutate(
    total = sum(n),
    prop = n / total
  ) %>%
  ungroup() %>%
  filter(total >= 100) %>%
  mutate(SEVERITY = factor(SEVERITY, levels = severity_levels)) %>%
  group_by(VehicleType) %>%
  mutate(FSI = sum(prop[SEVERITY %in% c("Fatal accident", "Serious injury accident")])) %>%
  ungroup() %>%
  mutate(VehicleType = fct_reorder(str_wrap(VehicleType, 25), FSI))

# 4️⃣ Visualise
p <- ggplot(veh_comp, aes(y = VehicleType, x = prop, fill = SEVERITY)) +
  geom_col(width = 0.7, colour = "white", linewidth = 0.2) +
  scale_fill_manual(values = severity_palette, name = "Severity") +
  scale_x_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "Crash Severity Composition by Vehicle Type",
    subtitle = "Each bar shows crash severity distribution per vehicle type (n ≥ 100)\nOrdered by Fatal + Serious proportion",
    x = "Proportion of Crashes (%)",
    y = NULL,
    caption = "Source: Victorian Crash Data (2012–2024)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.text.y = element_text(size = 10, colour = "#222"),
    legend.position = "top",
    legend.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(size = 11, colour = "grey30"),
    plot.caption = element_text(size = 9, colour = "grey40")
  )

print(p)

ggsave("Crash_Severity_by_VehicleType_FINAL.png", p,
       width = 10, height = 9, dpi = 300, bg = "white")
