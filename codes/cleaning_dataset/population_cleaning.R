library(tidyverse)

# Read datasets
population <- read_csv("Population2011_1656567141570.csv")

# Clean population dataset
population_clean <- population %>%
  mutate(Postcode = str_replace_all(str_trim(Postcode), " +", " "),
         Postcode = toupper(Postcode))

# Save the cleaned dataset
write_csv(population_clean, "cleaned_population.csv")

PopulationData = population_clean

View(PopulationData)
PopulationData$Postcode <- gsub("\\s.*", "", PopulationData$Postcode)

PopulationData = PopulationData %>% 
  distinct(Postcode, .keep_all = TRUE)


PopulationData$Population <- as.numeric(PopulationData$Population)


CombinedPopulationData = PopulationData %>%  
  group_by(Postcode) %>%
  summarise_at(vars(Population), list(Population2011 = ~sum(., na.rm = TRUE))) %>%
  mutate(Population2012 = 1.00695353132322269 * Population2011) %>%
  mutate(Population2013 = 1.00669740535540783 * Population2012) %>%
  mutate(Population2014 = 1.00736463978721671 * Population2013) %>%
  mutate(Population2015 = 1.00792367505802859 * Population2014) %>%
  mutate(Population2016 = 1.00757874492811929 * Population2015) %>%
  mutate(Population2017 = 1.00679374473924223 * Population2016) %>%
  mutate(Population2018 = 1.00605929132212552 * Population2017) %>%
  mutate(Population2019 = 1.00561255390388033 * Population2018) %>%
  mutate(Population2020 = 1.00561255390388033 * Population2019) %>%
  mutate(Population2021 = 1.005425 * Population2020) %>%
  mutate(Population2022 = 1.004920 * Population2021) %>%
  mutate(Population2023 = 1.004510 * Population2022) %>%
  mutate(Population2024 = 1.004220 * Population2023) %>%
  select(Postcode, Population2020, Population2021, Population2022, Population2023, Population2024)



View(CombinedPopulationData)

write_csv(CombinedPopulationData,"population_combined_distinct.csv")