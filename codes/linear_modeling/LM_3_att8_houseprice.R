library(tidyverse)

school_dataset = read_csv("cleaned_filtered_school_dataset.csv")
house_dataset = read_csv("cleaned_house_prices.csv")
colnames(school_dataset)
colnames(house_dataset)


# Clean postcodes (remove spaces for consistent joining)
house_clean <- house_dataset %>%
  select(postcode, price, county) %>%
  mutate(postcode = str_remove_all(postcode, " "))  # e.g. "HX28SN"

school_clean <- school_dataset %>%
  select(PCODE, ATT8SCR, County) %>%
  rename(postcode = PCODE, attainment = ATT8SCR) %>%
  mutate(postcode = str_remove_all(postcode, " "))  # e.g. "S639EW"

# Aggregate House Prices by Postcode
house_avg <- house_clean %>%
  group_by(postcode, county) %>%
  summarise(median_price = median(price, na.rm = TRUE), .groups = "drop")


# Merge School & House Datasets
merged_data <- inner_join(school_clean, house_avg, by = "postcode") %>%
  filter(!is.na(attainment) & !is.na(median_price))

# Merge School & House Datasets
merged_data <- inner_join(school_clean, house_avg, by = "postcode") %>%
  filter(!is.na(attainment) & !is.na(median_price))

# Plot (House Price vs Attainment Score)
ggplot(merged_data, aes(x = median_price, y = attainment, color = County)) +
  geom_point(alpha = 0.6, size = 2.5) +
  geom_smooth(method = "lm", se = TRUE, size = 1.2) +
  labs(
    title = "Attainment 8 Score vs House Price by County",
    x = "Median House Price (£)",
    y = "Attainment 8 Score",
    color = "County"
  ) +
  theme_minimal(base_size = 14)

# Linear Model Summary (Per County)
# West Yorkshire model
west <- merged_data %>% filter(County == "West Yorkshire")
model_west <- lm(attainment ~ median_price, data = west)
summary(model_west)

# South Yorkshire model
south <- merged_data %>% filter(County == "South Yorkshire")
model_south <- lm(attainment ~ median_price, data = south)
summary(model_south)

# Correlation Coefficients
cor_west <- cor(west$median_price, west$attainment, use = "complete.obs")
cor_south <- cor(south$median_price, south$attainment, use = "complete.obs")

cat("Correlation (West Yorkshire):", cor_west, "\n")
cat("Correlation (South Yorkshire):", cor_south, "\n")

