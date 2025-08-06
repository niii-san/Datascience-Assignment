library(tidyverse)

# joining postcode_lsoa with county
postcode_to_lsoa = read.csv("PostcodetoLSOA.csv")
View(postcode_to_lsoa)
lad_counties = read.csv("lad_counties.csv") # source https://geoportal.statistics.gov.uk/
View(lad_counties)

postcode_with_county <- postcode_to_lsoa %>%
  inner_join(lad_counties, by = c("ladcd" = "LAD24CD")) %>%
  rename(County = CTY24NM)

View(postcode_with_county)

postcode_with_county <- postcode_with_county %>% select(-LAD24NM) 

postcode_lsoa_county <- postcode_with_county

View(postcode_lsoa_county)

postcode_lsoa_county <- postcode_lsoa_county %>%
  mutate(pcds = str_replace_all(pcds, "\\s+", "")) %>%  # Remove all spaces in pcds column
  filter(County %in% c("South Yorkshire", "West Yorkshire"))

View(postcode_yorkshire)

# saving filtered data to CSV
write.csv(postcode_lsoa_county, "postcode_lsoa_county_filtered.csv", row.names = FALSE)
