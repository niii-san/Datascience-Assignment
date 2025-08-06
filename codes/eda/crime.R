library(tidyverse)
library(fmsb)

# Read data
crime_data <- read.csv("cleaned_crime_combined_population.csv") %>%
  filter(Year != 2025) %>%
  filter(!(County == "South" & District == "Leeds")) %>%
  filter(!(County == "West" & District == "Barnsley")) %>%
  distinct()

# ---- 1. Boxplot for Drug Offense Rate by District (2 graphs) ----
# Calculate drug rate per 100 total crimes per District-Year
drug_data <- crime_data %>%
  filter(Crime.type == "Drugs") %>%
  group_by(County, District, Year) %>%
  summarise(drug_count = n(), .groups = "drop")

total_data <- crime_data %>%
  group_by(County, District, Year) %>%
  summarise(total_crimes = n(), .groups = "drop")
drug_rate <- drug_data %>%
  left_join(total_data, by = c("County", "District", "Year")) %>%
  mutate(rate_percent = (drug_count / total_crimes) * 100)

# Separate data
drug_south <- drug_rate %>% filter(County == "South")
drug_west  <- drug_rate %>% filter(County == "West")

# Plot 1 - South
ggplot(drug_south, aes(x = District, y = rate_percent)) +
  geom_boxplot(fill = "#2E86C1") +
  labs(title = "Drug Offense Rate in South Yorkshire Districts",
       y = "Drug Rate (%)", x = "District") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  coord_flip() + theme_minimal()

# Plot 2 - West
ggplot(drug_west, aes(x = District, y = rate_percent)) +
  geom_boxplot(fill = "#C0392B") +
  labs(title = "Drug Offense Rate in West Yorkshire Districts",
       y = "Drug Rate (%)", x = "District") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_flip() + theme_minimal()

# ---- 2. Radar Chart for Vehicle Crime Rate (West, May 2022) ----
radar_data <- crime_data %>%
  filter(County == "West", Year == 2022, Month == 5, Crime.type == "Vehicle crime") %>%
  group_by(District) %>%
  summarise(vehicle_crime = n(), .groups = "drop")
# Prepare for radar
radar_plot_data <- radar_data %>%
  column_to_rownames("District") %>%
  t() %>%
  as.data.frame()

# Add max-min rows required by fmsb
radar_plot_data <- rbind(rep(max(radar_plot_data), ncol(radar_plot_data)),
                         rep(0, ncol(radar_plot_data)),
                         radar_plot_data)
# Plot radar
radarchart(radar_plot_data, axistype = 1,
           pcol = "#34495E", pfcol = scales::alpha("#3498DB", 0.6), plwd = 2,
           cglcol = "grey", axislabcol = "black",
           title = "Vehicle Crime Rate by District\n(West Yorkshire, May 2022)")

# ---- 3. Pie Chart for Robbery (South, June 2022) ----
robbery_data <- crime_data %>%
  filter(County == "West", Year == 2022, Month == 6, Crime.type == "Robbery") %>%
  group_by(District) %>%
  summarise(robbery_count = n(), .groups = "drop")
# Plot pie chart
ggplot(robbery_data, aes(x = "", y = robbery_count, fill = District)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  labs(title = "Robbery Distribution by District\n(West Yorkshire, June 2022)") +
  theme_void()

# ---- 4. Line Chart for Drug Offense Rate per 10,000 People ----
# Filter for drug crimes only
drug_data <- crime_data %>%
  filter(Crime.type == "Drugs")
# Ensure Year is numeric if not already
drug_data$Year <- as.numeric(drug_data$Year)
# Add population per year
drug_data <- drug_data %>%
  mutate(Population = case_when(
    Year == 2020 ~ Population2020,
    Year == 2021 ~ Population2021,
    Year == 2022 ~ Population2022,
    Year == 2023 ~ Population2023,
    Year == 2024 ~ Population2024,
    TRUE ~ NA_real_
  ))

# Group by County, Year, and Month
district_rate_yearly <- drug_data %>%
  group_by(County, Year, Month) %>%
  summarise(
    drug_offense_count = n(),
    Population = sum(Population, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    drug_rate_per_10k = (drug_offense_count / Population) * 10000,
    date = as.Date(paste(Year, Month, "01", sep = "-"))
  )

# Plot the monthly drug offense rate with proper date formatting
ggplot(district_rate_yearly, aes(x = date, y = drug_rate_per_10k, color = County)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_x_date(
    date_breaks = "3 months",
    date_labels = "%Y-%m"
  ) +
  labs(
    title = "Drug Offense Trend of South & West Yorkshire (2022–2024)",
    x = "Month",
    y = "Drug Offense Rate (/10,000)",
    color = "County"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom"  )
