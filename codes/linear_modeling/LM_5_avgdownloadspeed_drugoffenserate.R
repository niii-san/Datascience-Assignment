library(tidyverse)

# 4. Average Download speed vs Drug Offense Rate per 10000 people for both counties
# in single diagram (include linear model summary report and correlation)

broadband_data = read_csv("broadband_filtered_yorkshire.csv")
drugs_data = read_csv("cleaned_crime_combined_population.csv") 


# STEP 1: Filter drug data for only "Drugs" crime
drug_data_filtered <- drugs_data %>%
  filter(Crime.type == "Drugs")

# STEP 2: Aggregate drug offenses per postcode and count per 10,000
drug_rates <- drug_data_filtered %>%
  group_by(Postcode, County) %>%
  summarise(
    TotalCrimes = n(),
    AvgPopulation = sum(Population2024, na.rm = TRUE)
  ) %>%
  mutate(RatePer10k = (TotalCrimes / AvgPopulation) * 10000)

# STEP 3: Select relevant broadband columns
broadband_clean <- broadband_data %>%
  select(postcode = "postcode.x", County, `Average download speed (Mbit/s)`)

# STEP 4: Join the datasets by pcds
joined_data <- inner_join(drug_rates, broadband_clean, by = c("Postcode" = "postcode"), suffix = c("_drug", "_broadband"))

# STEP 5: Clean NA values
final_data <- joined_data %>%
  filter(!is.na(`Average download speed (Mbit/s)`), !is.na(RatePer10k))

# STEP 6: Correlation
correlation <- cor(final_data$`Average download speed (Mbit/s)`, final_data$RatePer10k)
cat("Correlation:", round(correlation, 3), "\n")


# STEP 7: Linear model
lm_model <- lm(`Average download speed (Mbit/s)` ~ RatePer10k, data = final_data)
summary(lm_model)

# STEP 8: Plot
ggplot(final_data, aes(x = RatePer10k, y = `Average download speed (Mbit/s)`, color = County_broadband)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "Drug Offense Rate vs Average Download Speed",
    x = "Drug Offense Rate per 10,000",
    y = "Average Download Speed (Mbit/s)",
    color = "County"
  ) +
  theme_minimal()

