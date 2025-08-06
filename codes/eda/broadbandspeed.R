library(tidyverse)

broadband = read.csv("broadband_filtered_yorkshire.csv")
colnames(broadband)

# Remove rows with missing or invalid average download speed
broadband <- broadband %>%
  filter(!is.na(Average.download.speed..Mbit.s.))


# Boxplot of Avg Download Speed by District
# West Yorkshire boxplot
broadband %>%
  filter(County == "West Yorkshire") %>%
  ggplot(aes(x = fct_reorder(ladnm.x, Average.download.speed..Mbit.s., .fun = median), 
             y = Average.download.speed..Mbit.s.)) +
  geom_boxplot(fill = "skyblue") +
  coord_flip() +
  labs(title = "West Yorkshire - Avg Download Speed by District",
       x = "District", y = "Avg Download Speed (Mbps)") +
  theme_minimal()

# South Yorkshire boxplot
broadband %>%
  filter(County == "South Yorkshire") %>%
  ggplot(aes(x = fct_reorder(ladnm.x, Average.download.speed..Mbit.s., .fun = median), 
             y = Average.download.speed..Mbit.s.)) +
  geom_boxplot(fill = "salmon") +
  coord_flip() +
  labs(title = "South Yorkshire - Avg Download Speed by District",
       x = "District", y = "Avg Download Speed (Mbps)") +
  theme_minimal()

#  Bar Chart of Avg Download Speed by Town (from lsoa11nm)
# West Yorkshire
broadband %>%
  filter(County == "West Yorkshire") %>%
  group_by(ladnm.x) %>%
  summarise(avg_speed = mean(Average.download.speed..Mbit.s., na.rm = TRUE)) %>%
  arrange(desc(avg_speed)) %>%
  slice_head(n = 15) %>%
  ggplot(aes(x = fct_reorder(ladnm.x, avg_speed), y = avg_speed)) +
  geom_col(fill = "steelblue") +
  labs(title = "West Yorkshire Towns by Avg Download Speed",
       x = "Town", y = "Avg Download Speed (Mbps)") +
  theme_minimal()

# South Yorkshire
broadband %>%
  filter(County == "South Yorkshire") %>%
  group_by(ladnm.x) %>%
  summarise(avg_speed = mean(Average.download.speed..Mbit.s., na.rm = TRUE)) %>%
  arrange(desc(avg_speed)) %>%
  slice_head(n = 15) %>%
  ggplot(aes(x = fct_reorder(ladnm.x, avg_speed), y = avg_speed)) +
  geom_col(fill = "coral") +
  labs(title = "South Yorkshire Towns by Avg Download Speed",
       x = "Town", y = "Avg Download Speed (Mbps)") +
  theme_minimal()

