library(tidyverse)

base_path <- "~/ds_assignment/obtained_data/crime_dataset/all_together"

# Getting all CSV files inside sub folders
csv_files <- list.files(base_path, pattern = "*.csv", full.names = TRUE, recursive = TRUE)

# Cleaning function
clean_crime_file <- function(file_path) {
  df <- read_csv(file_path, show_col_types = FALSE)
  
  filename <- basename(file_path)
  split_name <- str_split(filename, "-", simplify = TRUE)
  year_month <- paste0(split_name[1], "-", split_name[2])
  county <- str_to_title(split_name[3])  # "South" or "West"
  
  # Adding metadata and select useful columns only
  df <- df %>%
    mutate(
      County = county,
      YearMonth = year_month,
      District = word(`LSOA name`, 1)
    ) %>%
    select(
      YearMonth,
      Month,
      County,
      District,
      `LSOA code`,
      `Crime type`,
      Longitude,
      Latitude
    )
  
  return(df)
}

# Combining all cleaned crime files
crime_df <- map_dfr(csv_files, clean_crime_file)

# Save to CSV if needed
write_csv(crime_df, "cleaned_crime_dataset.csv")

cleaned_crime_dataset.csv = read_csv("cleaned_crime_dataset.csv")
View(cleaned_crime_dataset.csv)

########################### Crime dataset combining with population  ##########################
crime_dataset = read.csv("cleaned_crime_dataset.csv")
postcode_lsoa_county_filtered = read.csv("postcode_lsoa_county_filtered.csv")
View(crime_dataset)
View(postcode_lsoa_county_filtered)

# Triming all whitespace in 'pcds'
postcode_lsoa_county_filtered$pcds <- gsub("\\s.*", "", postcode_lsoa_county_filtered$pcd8)

# Keep only one postcode per LSOA (choose the first for now)
postcode_unique <- postcode_lsoa_county_filtered %>%
  select(lsoa11cd, pcds) %>%
  distinct(lsoa11cd, .keep_all = TRUE)

# Join using deduplicated postcode dataset
combined_dataset <- crime_dataset %>%
  left_join(postcode_unique, by = c("LSOA.code" = "lsoa11cd"))

# combining crime dataset with population ####################
population = read_csv("population_combined_distinct.csv")

# Triming numbers after white space in Postcode column
population <- population %>%
  mutate(Postcode = gsub("\\s.*", "", Postcode)) %>%     # Keep only part before space
  distinct(Postcode, .keep_all = TRUE)   

# combining population with crime dataset
combined_with_population <- combined_dataset %>%
  left_join(population, by = c("pcds" = "Postcode"))

View(combined_with_population)

write_csv(combined_with_population,"cleaned_crime_combined_population.csv")
