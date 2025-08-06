library(tidyverse)

# Reading Datasets
broadband_perf <- read_csv("201805_fixed_pc_performance_r03.csv")
broadband_cov <- read_csv("201809_fixed_pc_coverage_r01.csv")

# Standardizing and cleaning postcodes in broadband data sets
broadband_perf <- broadband_perf %>%
  mutate(postcode = toupper(str_replace_all(postcode, " ", ""))) %>%
  filter(!is.na(postcode) & postcode != "")
broadband_cov <- broadband_cov %>%
  mutate(postcode = toupper(str_replace_all(postcode, " ", ""))) %>%
  filter(!is.na(postcode) & postcode != "")

# Joining broadband performance and coverage data on postcode
broadband_combined <- left_join(broadband_perf, broadband_cov, by = "postcode")

# Reading postcode to LSOA data set
postcode_to_lsoa <- read_csv("PostcodeToLSOA.csv")
View(postcode_to_lsoa)

postcode_to_lsoa_clean <- postcode_to_lsoa %>%
  mutate(pcds = toupper(str_replace_all(pcds, " ", ""))) %>%
  filter(!is.na(pcds) & pcds != "")

broadband_with_lsoa <- broadband_combined %>%
  left_join(postcode_to_lsoa_clean, by = c("postcode" = "pcds"))

broadband_final <- broadband_with_lsoa %>%
  filter(!is.na(lsoa11cd))

write_csv(broadband_final, "cleaned_broadband_speed_with_lsoa.csv")
View(broadband_final)

colnames(broadband_final)

postcode_lsoa_county = read.csv("postcode_lsoa_county.csv")
View(postcode_lsoa_county)

broadband_final <- broadband_final %>%
  mutate(pcds = str_remove_all(postcode, "\\s+"))

postcode_lsoa_county <- postcode_lsoa_county %>%
  mutate(pcds = str_remove_all(pcds, "\\s+"))

broadband_with_county <- broadband_final %>%
  inner_join(
    postcode_lsoa_county %>% select(pcds, County),
    by = "pcds"
  )
colnames(broadband_with_county)

# Filtering only South Yorkshire, West Yorkshire and needed columns
broadband_filtered <- broadband_with_county %>%
  filter(County %in% c("South Yorkshire", "West Yorkshire")) %>% 
  select(-"% of premises unable to receive 2Mbit/s", -"% of premises unable to receive 5Mbit/s",
         -"% of premises unable to receive 10Mbit/s", -"% of premises unable to receive 30Mbit/s",              
         -"% of premises unable meet USO", -"% of premises able to receive decent broadband from FWA",
         -"% of premises able to receive SFBB from FWA", -"% of premises able to receive NGA",
         -"Average data usage (GB) for lines < 10Mbit/s",-"UFBB availability (% premises)", -"SFBB availability (% premises)",
         -"FTTP availability (% premises)",-"Number of connections < 2 Mbit/s (number of lines)",
         -"Number of connections 2<5 Mbit/s (number of lines)",-"Number of connections 5<10 Mbit/s (number of lines)",
         -"Number of connections 10<30 Mbit/s (number of lines)",-"Number of connections 30<300 Mbit/s (number of lines)",
         -"Number of connections >= 300 Mbit/s (number of lines)",-"Number of connections >= 30 Mbit/s (number of lines)",
         -"Average data usage (GB) for lines < 10Mbit/s",-"Average data usage (GB) for Basic BB lines",
         -"Average data usage (GB) for SFBB lines",-"Average data usage (GB) for UFBB lines",
         ,-"All Premises",-"All Matched Premises",-"doterm")

View(broadband_filtered)
write_csv(broadband_filtered, "broadband_filtered_yorkshire.csv")