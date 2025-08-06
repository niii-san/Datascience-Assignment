library(tidyverse)

house_data = read_csv("cleaned_house_prices.csv")
drug_data = read_csv("cleaned_crime_combined_population.csv") %>% 
  filter(Crime.type == "Drugs")

colnames(house_data)
colnames(drug_data)

# Fix postcode formatting: remove spaces and uppercase for joining
house_data <- house_data %>%
  mutate(Postcode = gsub(" ", "", toupper(postcode)))

# Compute average house price per postcode
avg_house_price <- house_data %>%
  group_by(Postcode, county) %>%
  summarise(AveragePrice = mean(price, na.rm = TRUE), .groups = "drop")

#Preprocess Drug Data
# Filter only 2023 drug crimes
drug_2023 <- drug_data %>%
  filter(Crime.type == "Drugs", Year == 2023)

# Remove spaces and uppercase for joining
drug_2023 <- drug_2023 %>%
  mutate(Postcode = gsub(" ", "", toupper(Postcode)))

# Calculate drug offense rate per 10,000 people
drug_rates_2023 <- drug_2023 %>%
  group_by(Postcode, County) %>%
  summarise(
    DrugOffenseCount = n(),
    Population = sum(Population2023, na.rm = TRUE),
    RatePer10k = (DrugOffenseCount / Population) * 10000,
    .groups = "drop"
  )

# Join Both Datasets
# Inner join on postcode
joined_data <- inner_join(drug_rates_2023, avg_house_price, by = "Postcode")

# Plotting with Linear Model and Correlation
# Correlation
correlation <- cor(joined_data$RatePer10k, joined_data$AveragePrice, use = "complete.obs")
cat("Correlation between Drug Rate and House Price:", correlation, "\n")

# Linear model
lm_model <- lm(AveragePrice ~ RatePer10k, data = joined_data)
summary(lm_model)

# Plot
ggplot(joined_data, aes(x = RatePer10k, y = AveragePrice, color = County)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "House Price vs Drug Offense Rate per 10,000 People (2023)",
    x = "Drug Offense Rate per 10,000",
    y = "Average House Price (GBP)"
  ) +
  theme_minimal()
