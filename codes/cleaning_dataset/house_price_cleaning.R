library(tidyverse)


# Defining the column names based on UK Price Paid Data structure
col_names <- c(
  "transaction_id", "price", "date_of_transfer", "postcode",
  "property_type", "new_build_flag", "tenure_type", "primary_address",
  "secondary_address", "street", "locality", "town_city",
  "district", "county", "ppd_category_type", "record_status"
)

# Loading the datasets without headers
pp2021 <- read_csv("pp-2021.csv", col_names = FALSE)
pp2022 <- read_csv("pp-2022.csv", col_names = FALSE)
pp2023 <- read_csv("pp-2023.csv", col_names = FALSE)
pp2024 <- read_csv("pp-2024.csv", col_names = FALSE)

# Assigning column names to each dataset
colnames(pp2021) <- col_names
colnames(pp2022) <- col_names
colnames(pp2023) <- col_names
colnames(pp2024) <- col_names

View(pp2022)

# Combining all datasets into one
house_data <- bind_rows(pp2021, pp2022, pp2023, pp2024)

# Cleaning the postcode column: trim white space and convert to uppercase
house_data <- house_data %>%
  mutate(postcode = str_trim(str_to_upper(postcode)))

# only filtering south yorkshire and west yorkshire
filtered_data <- house_data %>%
  filter(county %in% c("WEST YORKSHIRE", "SOUTH YORKSHIRE"))
View(filtered_data)

write_csv(filtered_data, "cleaned_house_prices.csv")