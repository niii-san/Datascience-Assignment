library(tidyverse)

school_dataset = read_csv("cleaned_filtered_school_dataset.csv")
crime_data = read_csv("cleaned_crime_combined_population.csv")

# Filter for Drugs only and year 2023
drug_data <- crime_data %>%
  filter(Crime.type == "Drugs", Year == 2023) %>% 
  distinct(LSOA.code, .keep_all = TRUE)


# Calculate Drug Offense Rate per 10,000 people
drug_rates <- drug_data %>%
  group_by(Postcode, Year, Month) %>%
  summarise(
    drug_crimes = n(),
    population = sum(Population2023, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(rate_per_10k = (drug_crimes / population) * 10000)

# Clean & Prepare School Dataset
school_clean <- school_dataset %>%
  select(PCODE, ATT8SCR, County) %>%
  rename(attainment = ATT8SCR) %>%
  mutate(PCODE = str_remove_all(PCODE, " "))

colnames(school_clean)
colnames(drug_rates)


# Merge Drug Offense Rates with School Attainment
merged_data <- inner_join(school_clean, drug_rates, by = c("PCODE" = "Postcode")) %>%
  filter(!is.na(attainment) & !is.na(rate_per_10k))

# Plot: Drug Crime Rate vs Attainment Score
ggplot(merged_data, aes(x = rate_per_10k, y = attainment, color = County)) +
  geom_point(alpha = 0.6, size = 2.5) +
  geom_smooth(method = "lm", se = TRUE, size = 1.2) +
  labs(
    title = "Attainment 8 Score vs Drug Offense Rate (per 10,000 people) in 2023",
    x = "Drug Offense Rate per 10,000 People",
    y = "Attainment 8 Score",
    color = "County"
  ) +
  theme_minimal(base_size = 14)


# Linear Model Summary (Per County)
west <- merged_data %>% filter(County == "West Yorkshire")
south <- merged_data %>% filter(County == "South Yorkshire")

model_west <- lm(attainment ~ rate_per_10k, data = west)
model_south <- lm(attainment ~ rate_per_10k, data = south)

summary(model_west)
summary(model_south)

# Correlation
cor_west <- cor(west$rate_per_10k, west$attainment, use = "complete.obs")
cor_south <- cor(south$rate_per_10k, south$attainment, use = "complete.obs")

cat("Correlation (West Yorkshire):", cor_west, "\n")
cat("Correlation (South Yorkshire):", cor_south, "\n")
