# ====================================
# 1️⃣ Install & load required libraries
# ====================================
install.packages(c("ggmap", "tidyverse", "readxl"), dependencies = TRUE)

library(ggmap)
library(tidyverse)
library(readxl)

# ====================================
# 2️⃣ Register your Google Maps API key
# ====================================
register_google(key = "AIzaSyDBuORmhhdjrDXmmjDb4igdHYvFFqEjkCw")  # replace with your actual key

# ====================================
# 3️⃣ Load cleaned crash dataset
# ====================================
crash <- read_excel("vic_road_crash_data_clean.xlsx") %>%
  mutate(
    LATITUDE = as.numeric(LATITUDE),
    LONGITUDE = as.numeric(LONGITUDE)
  ) %>%
  filter(
    !is.na(LATITUDE), !is.na(LONGITUDE),
    between(LATITUDE, -39.8, -34.0),
    between(LONGITUDE, 141.0, 150.5)
  )

# ====================================
# 4️⃣ Fetch Google base map of Victoria
# ====================================
# you can replace "Melbourne, Victoria" with "Victoria, Australia" for full state
vic_map <- get_map(
  location = "Melbourne, Victoria",
  zoom = 7, 
  maptype = "roadmap"   # other options: "terrain", "satellite", "hybrid"
)

# Display the map alone
ggmap(vic_map)

# ====================================
# 5️⃣ Plot crash points on the Google Map
# ====================================
ggmap(vic_map) +
  geom_point(
    data = crash,
    aes(x = LONGITUDE, y = LATITUDE),
    color = "red", alpha = 0.3, size = 1
  ) +
  labs(
    title = "Road Crashes across Victoria",
    subtitle = "Google Maps API — plotted from cleaned VIC crash data",
    x = "Longitude", y = "Latitude"
  ) +
  theme_minimal(base_size = 13)

