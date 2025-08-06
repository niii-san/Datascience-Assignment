library(tidyverse)
library(lubridate)

house_data = read_csv("cleaned_house_prices.csv")
crime = read_csv("cleaned_crime_combined_population.csv")
broadband_data = read_csv("broadband_filtered_yorkshire.csv")
schoolData = read_csv("cleaned_filtered_school_dataset.csv")


# Check original data dimensions
cat("Original data dimensions:\n")
cat(sprintf("house_data: %d rows\n", nrow(house_data)))
cat(sprintf("crime: %d rows\n", nrow(crime)))
cat(sprintf("schoolData: %d rows\n", nrow(schoolData)))
cat(sprintf("broadband_data: %d rows\n", nrow(broadband_data)))

# Check unique towns in each dataset
cat("\nUnique towns count:\n")
cat(sprintf("house_data towns: %d\n", length(unique(house_data$town_city))))
cat(sprintf("crime towns: %d\n", length(unique(crime$town_city))))
cat(sprintf("schoolData towns: %d\n", length(unique(schoolData$TOWN))))
cat(sprintf("broadband_data towns: %d\n", length(unique(broadband_data$town_city))))


# Housing Data Processing
selected_house <- house_data %>%
  mutate(TOWN = str_trim(toupper(town_city))) %>%
  group_by(TOWN) %>%
  summarise(avgPrice = mean(price, na.rm = TRUE)) %>%
  select(avgPrice, TOWN) %>%
  na.omit() %>%
  distinct()

# School Data Processing
selected_attainment8 <- schoolData %>%
  mutate(
    TOWN = str_trim(toupper(TOWN)),
    ATT8SCR_clean = suppressWarnings(as.numeric(as.character(ATT8SCR)))
  ) %>%
  filter(!is.na(ATT8SCR_clean), !TOWN == "SOUTH YORKSHIRE", !TOWN == "WEST YORKSHIRE") %>% 
  group_by(TOWN) %>% 
  summarize(avgAtt8 = mean(ATT8SCR_clean, na.rm = TRUE)) %>%
  ungroup() 


# Broadband Data Processing
selected_broadband <- broadband_data %>%
  group_by(town_city) %>%
  mutate(
    AvgUpSpeed = `Average upload speed (Mbit/s)`,
    AvgDownSpeed = `Average download speed (Mbit/s)`
  ) %>% 
  summarise(
    avg_down_speed = mean(AvgDownSpeed, na.rm = TRUE)
  ) %>%
  mutate(TOWN = str_trim(toupper(town_city))) %>%
  select(TOWN, avg_down_speed)

# Crime Data processsing
selected_crime <- crime %>%
  group_by(postcode) %>% 
  summarize(
    crimeno = n(),
    TOWN = first(town_city)  # or use unique(Town_City)[1] for safety
  ) %>% 
  select(postcode, crimeno, TOWN) %>%  
  arrange(desc(crimeno))

cat(sprintf("crime towns: %d\n", length(unique(selected_crime$TOWN))))
colnames(selected_crime)



house_towns <- unique(selected_house$TOWN)
school_towns <- unique(selected_attainment8$TOWN)
broadband_towns <- unique(selected_broadband$TOWN)
crime_towns <- unique(selected_crime$TOWN)

cat(sprintf("House towns: %d\n", length(house_towns)))
cat(sprintf("School towns: %d\n", length(school_towns)))
cat(sprintf("Broadband towns: %d\n", length(broadband_towns)))
cat(sprintf("Crime towns: %d\n", length(crime_towns)))


#Combine All Data (using left_join to keep as many as possible)
ranking <- selected_house %>%
  left_join(selected_attainment8, by = "TOWN") %>%  
  left_join(selected_broadband, by = "TOWN") %>%  
  left_join(selected_crime, by = "TOWN") %>%
  na.omit()


Extremes <- ranking %>%
  summarise(
    minDownSpeed = min(avg_down_speed, na.rm = TRUE),
    maxDownSpeed = max(avg_down_speed, na.rm = TRUE),
    minAtt8 = min(avgAtt8, na.rm = TRUE),
    maxAtt8 = max(avgAtt8, na.rm = TRUE),
    minHousingPrice = min(avgPrice, na.rm = TRUE),
    maxHousingPrice = max(avgPrice, na.rm = TRUE),
    minCrimeRate = min(crimeno, na.rm = TRUE),
    maxCrimeRate = max(crimeno, na.rm = TRUE)
  )

print(Extremes)



finalRanking <- ranking %>%
  mutate(
    normDownSpeed = 10 * (avg_down_speed - Extremes$minDownSpeed) / (Extremes$maxDownSpeed - Extremes$minDownSpeed),
    normAtt8 = 10 * (avgAtt8 - Extremes$minAtt8) / (Extremes$maxAtt8 - Extremes$minAtt8),
    normHousingPrice = 10 * (1 - (avgPrice - Extremes$minHousingPrice) / (Extremes$maxHousingPrice - Extremes$minHousingPrice)),
    normCrimeRate = 10 * (1 - (crimeno - Extremes$minCrimeRate) / (Extremes$maxCrimeRate - Extremes$minCrimeRate)),
    finalPoints = normDownSpeed  + normAtt8 + normHousingPrice + normCrimeRate
  )

colnames(finalRanking)


houserank <- finalRanking %>%
  select(TOWN, avgPrice, normHousingPrice) %>%
  arrange(desc(normHousingPrice))  %>% 
  distinct(TOWN,.keep_all = TRUE) %>% 
  slice_head(n=10) 

crimerank <- finalRanking %>%
  select(TOWN, normCrimeRate, crimeno) %>%
  arrange(desc(normCrimeRate))  %>% 
  distinct(TOWN,.keep_all = TRUE) %>% 
  slice_head(n=10) 

schoolrank <- finalRanking %>%
  select(TOWN, avgAtt8, normAtt8) %>%
  arrange(desc(normAtt8))  %>% 
  distinct(TOWN,.keep_all = TRUE) %>% 
  slice_head(n=10) 1

broadbandrank <- finalRanking %>%
  select(TOWN, avg_down_speed, normDownSpeed) %>%
  arrange(desc(normDownSpeed)) %>% 
  distinct(TOWN,.keep_all = TRUE) %>% 
  slice_head(n=10) 


final_rank <- finalRanking %>%
  select(TOWN, avgPrice, crimeno, avgAtt8, avg_down_speed, finalPoints) %>%
  mutate(finalPoints = finalPoints / 4) %>% 
  arrange(desc(finalPoints)) %>% 
  distinct(TOWN,.keep_all = TRUE) %>% 
  slice_head(n=10)
