library(tidyverse)

house_prices = read_csv("cleaned_house_prices.csv")
colnames(house_prices)

# Extract year
house_prices <- house_prices %>%
  mutate(year = year(as.Date(date_of_transfer)))
# Filter years and counties
filtered_hp <- house_prices %>%
  filter(year %in% 2021:2024,
         county %in% c("WEST YORKSHIRE", "SOUTH YORKSHIRE"))
# Group and summarize
avg_prices_by_year_county <- filtered_hp %>%
  group_by(year, county) %>%
  summarise(avg_price = mean(price, na.rm = TRUE), .groups = "drop")
# Plot line graph
ggplot(avg_prices_by_year_county, aes(x = year, y = avg_price, color = county)) +
  geom_line(size = 1.2) +
  geom_point() +
  labs(
    title = "Average House Prices (2021–2024)",
    x = "Year",
    y = "Average Price",
    color = "County"
  ) + theme_minimal()

# bar chart
# Filter for 2023 only
avg_2023 <- filtered_hp %>%
  filter(year == 2023) %>%
  group_by(county) %>%
  summarise(avg_price = mean(price, na.rm = TRUE), .groups = "drop")

# Plot bar chart
ggplot(avg_2023, aes(x = county, y = avg_price, fill = county)) +
  geom_col(width = 0.6) +
  labs(
    title = "Average House Prices in 2023",
    x = "County",
    y = "Average Price"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# Boxplot for average house prices for both counties in separate diagrams (take
# variables Price and District) 
# Calculate IQR and filter out outliers
filtered_hp_clean <- filtered_hp %>%
  group_by(county) %>%
  mutate(
    Q1 = quantile(price, 0.25, na.rm = TRUE),
    Q3 = quantile(price, 0.75, na.rm = TRUE),
    IQR = Q3 - Q1,
    lower_bound = Q1 - 1.5 * IQR,
    upper_bound = Q3 + 1.5 * IQR
  ) %>%
  filter(price >= lower_bound & price <= upper_bound) %>%
  ungroup()

# Split datasets
west_yorkshire <- filtered_hp_clean %>% filter(county == "WEST YORKSHIRE")
south_yorkshire <- filtered_hp_clean %>% filter(county == "SOUTH YORKSHIRE")

# Plot for West Yorkshire
ggplot(west_yorkshire, aes(x = district, y = price, fill = district)) +
  geom_boxplot() +
  labs(
    title = "House Prices by District – West Yorkshire",
    x = "District",
    y = "Price"
  ) +
  theme_minimal() +
  coord_flip() +
  theme(
     legend.position = "none"
  )

# Plot for South Yorkshire
ggplot(south_yorkshire, aes(x = district, y = price, fill = district)) +
  geom_boxplot() +
  labs(
    title = "House Prices by District – South Yorkshire",
    x = "District",
    y = "Price"
  ) +
  theme_minimal() + 
  coord_flip() +
  theme(
    legend.position = "none"
  ) 
  

        