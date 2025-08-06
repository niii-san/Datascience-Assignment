library(tidyverse)

# 6. Average download speed vs Attainment 8 score for both counties in single
# diagram (include linear model summary report and correlation)

broadband_data = read_csv("broadband_filtered_yorkshire.csv")
school_data = read_csv("cleaned_filtered_school_dataset.csv")

colnames(broadband_data)
colnames(school_data)

# 1. Clean column names if needed
colnames(broadband_data) <- make.names(colnames(broadband_data))
colnames(school_data) <- make.names(colnames(school_data))

# 2. Merge broadband and school data using postcode (remove space to match formats)
broadband_data <- broadband_data %>%
  mutate(postcode_nospace = gsub(" ", "", postcode_space))

school_data <- school_data %>%
  mutate(postcode_nospace = gsub(" ", "", PCODE))

merged_data <- inner_join(school_data, broadband_data, by = "postcode_nospace")

# 3. Filter for West Yorkshire and South Yorkshire
filtered_data <- merged_data %>%
  filter(County.x %in% c("West Yorkshire", "South Yorkshire")) %>%
  select(SCHNAME, ATT8SCR, `Average.download.speed..Mbit.s.`, County = County.x)

# 4. Remove rows with missing values
filtered_data <- filtered_data %>%
  filter(!is.na(ATT8SCR), !is.na(Average.download.speed..Mbit.s.))

# 5. Correlation
correlation <- cor(filtered_data$Average.download.speed..Mbit.s., filtered_data$ATT8SCR, use = "complete.obs")
cat("Correlation: ", correlation, "\n")


# 6. Linear model
model <- lm(ATT8SCR ~ Average.download.speed..Mbit.s., data = filtered_data)
summary(model)

# 7. Plot
ggplot(filtered_data, aes(x = Average.download.speed..Mbit.s., y = ATT8SCR, color = County)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "Average Download Speed vs Attainment 8 Score",
    subtitle = paste("Correlation:", round(correlation, 3)),
    x = "Average Download Speed (Mbit/s)",
    y = "Attainment 8 Score"
  ) +
  theme_minimal()
