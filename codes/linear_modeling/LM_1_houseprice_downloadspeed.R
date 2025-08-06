library(tidyverse)

house_data = read_csv("cleaned_house_prices.csv")
broadband_data = read_csv("broadband_filtered_yorkshire.csv")

colnames(house_data)
colnames(broadband_data)

# Preprocess Data
house_trimmed <- house_data %>%
  select(postcode, price, county = county) %>%
  mutate(postcode = str_remove_all(postcode, " "))  # Remove spaces

broadband_trimmed <- broadband_data %>%
  select(postcode = `postcode.x`, download_speed = `Average download speed (Mbit/s)`, County) %>%
  mutate(postcode = str_remove_all(postcode, " "))

# Merge Both Datasets by Postcode
merged_data <- inner_join(house_trimmed, broadband_trimmed, by = "postcode") %>%
  filter(!is.na(download_speed) & !is.na(price))  # Remove missing values

# Visualize Scatter Plot + Linear Models
ggplot(merged_data, aes(x = price, y = download_speed, color = County)) +
  geom_point(alpha = 0.5, size = 2) +
  geom_smooth(method = "lm", se = TRUE, size = 1.2) +
  labs(
    title = "Download Speed vs House Price by County",
    subtitle = "With Linear Regression Lines for West and South Yorkshire",
    x = "House Price (£)",
    y = "Average Download Speed (Mbit/s)",
    color = "County"
  ) +
  theme_minimal(base_size = 14)

# Linear Model Summary per County
# i. West Yorkshire
west <- merged_data %>% filter(County == "West Yorkshire")
model_west <- lm(download_speed ~ price, data = west)
summary(model_west)

# ii. South Yorkshire
south <- merged_data %>% filter(County == "South Yorkshire")
model_south <- lm(download_speed ~ price, data = south)
summary(model_south)

# Correlation Calculation
cor_west <- cor(west$download_speed, west$price, use = "complete.obs")
cor_south <- cor(south$download_speed, south$price, use = "complete.obs")

cat("Correlation (West Yorkshire):", cor_west, "\n")
cat("Correlation (South Yorkshire):", cor_south, "\n")
