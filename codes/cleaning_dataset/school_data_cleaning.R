library(tidyverse)

ks4final_2021_2022 = read_csv("england_ks4final_2021_2022.csv")
ks4final_2022_2023 = read_csv("england_ks4final_2022_2023.csv")
ks4final_2023_2024 = read_csv("england_ks4final_2023_2024.csv")

postcode_lsoa_county = read_csv("postcode_lsoa_county_filtered.csv")

test = ks4final_2021_2022 %>% 
  distinct(TOWN , .keep_all =  TRUE)

View(ks4final_2021_2022)
View(ks4final_2022_2023)
View(ks4final_2023_2024)

# Adding year and selecting relevant columns
ks4_21_22 <- ks4final_2021_2022 %>%
  select(URN, SCHNAME, PCODE, TOWN, ATT8SCR) %>%
  mutate(Year = "2022")
ks4_22_23 <- ks4final_2022_2023 %>%
  select(URN, SCHNAME, PCODE, TOWN, ATT8SCR) %>%
  mutate(Year = "2023")
ks4_23_24 <- ks4final_2023_2024 %>%
  select(URN, SCHNAME, PCODE, TOWN, ATT8SCR) %>%
  mutate(Year = "2024")

# Combining all three years ks4 dataset
ks4_combined <- bind_rows(ks4_21_22, ks4_22_23, ks4_23_24)

# Cleaning ATT8SCR by removing 'NE', 'SUPP', and non-numeric values, then remove NAs
ks4_clean <- ks4_combined %>%
  filter(!ATT8SCR %in% c("NE", "SUPP")) %>%
  filter(!is.na(ATT8SCR)) %>%
  filter(str_detect(ATT8SCR, "^[0-9.]+$"))

# Converting ATT8SCR to numeric for analysis/visualization
ks4_clean <- ks4_clean %>%
  mutate(ATT8SCR = as.numeric(ATT8SCR))

postcode_lsoa_county = read_csv("postcode_lsoa_county.csv")

# Triming whitespace in postcode columns
ks4_clean <- ks4_clean %>%
  mutate(PCODE = str_trim(PCODE))
postcode_lsoa_county <- postcode_lsoa_county %>%
  mutate(pcds = str_trim(pcds))

# Joining to add 'County'
ks4_with_county <- ks4_clean %>%
  left_join(postcode_lsoa_county %>% select(pcds, County), by = c("PCODE" = "pcds"))

View(ks4_with_county)

# Filtering only West Yorkshire and South Yorkshire
ks4_filtered <- ks4_with_county %>%
  filter(County %in% c("West Yorkshire", "South Yorkshire"))

View(ks4_filtered)

# Joining ladnm to school data set
school_data <- inner_join(
  school_data,
  postcode_lsoa_county %>% select(pcd7, ladnm),
  by = c("PCODE" = "pcd7")
)

write.csv(ks4_filtered, "cleaned_filtered_school_dataset.csv", row.names = FALSE)
