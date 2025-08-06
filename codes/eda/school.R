library(tidyverse)

school_data = read.csv("cleaned_filtered_school_dataset.csv")
View(school_data)
colnames(school_data)

#  Boxplot: South Yorkshire – Attainment 8 Score by District (TOWN)
# South Yorkshire boxplot
school_data %>%
  filter(County == "South Yorkshire", Year == 2022, !is.na(ladnm)) %>%
  ggplot(aes(x = fct_reorder(ladnm, ATT8SCR), y = ATT8SCR)) +
  geom_boxplot(fill = "steelblue") +
  labs(
    title = "Attainment 8 Score by District (2022) - South Yorkshire",
    x = "Town", y = "Attainment 8 Score"
  ) +
  theme_minimal() +
  coord_flip()

# Boxplot for West Yorkshire schools (2022 only)
school_data %>%
  filter(County == "West Yorkshire", Year == 2022, !is.na(ladnm)) %>%
  ggplot(aes(x = fct_reorder(ladnm, ATT8SCR), y = ATT8SCR)) +
  geom_boxplot(fill = "darkseagreen") +
  labs(
    title = "Attainment 8 Score by District (2022) - West Yorkshire",
    x = "Town", y = "Attainment 8 Score"
  ) +
  theme_minimal() +
  coord_flip()

# Line Graph to show the relationship between attainment 8 score and years over
# multiple districts in South Yorkshire and west Yorkshire
# West Yorkshire Line Graph
school_data %>%
  filter(County %in% c("South Yorkshire", "West Yorkshire"), !is.na(ladnm)) %>%
  group_by(County, ladnm, Year) %>%
  summarise(avg_attainment = mean(ATT8SCR, na.rm = TRUE), .groups = "drop") %>%
  mutate(ladnm_full = paste0(ladnm, " (", County, ")")) %>%
  ggplot(aes(x = ladnm_full, y = avg_attainment, group = Year, color = as.factor(Year))) +
  geom_line(aes(linetype = as.factor(Year)), size = 1) +
  geom_point(size = 2) +
  labs(
    title = "Attainment 8 Score by District (South & West Yorkshire)",
    x = "Town (County)",
    y = "Average Attainment 8 Score",
    color = "Year",
    linetype = "Year"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


