# _________________________________________________
# Purpose:
# import vaccination date data extracted by cohort extractor
# organise vaccination date data to "vax X type", "vax X date" (rather than "pfizer X date", "az X date", ...)
# _________________________________________________

# Preliminaries ----

# Import libraries
library("tidyverse")
library("lubridate")
library("arrow")
library("here")
library("glue")

# Import custom functions
source(here("analysis", "utility.R"))

# create output directory
output_dir <- here("output", "process")
fs::dir_create(output_dir)

# define start and end of observation period
start_date <- as.Date("2020-06-01")
end_date <- as.Date("2023-12-31")

# Import and process fixed dataset ----

# Import fixed dataset
data_extract_fixed <-
  import_extract(
    here("lib", "dummydata", "dummyinput_fixed.arrow"),
    here("output", "extracts", "extract_fixed.arrow")
  )

# Process snapshot dataset
data_processed_fixed <- data_extract_fixed %>%
  mutate(
    sex = case_when(
      sex == "female" ~ "Female",
      sex == "male" ~ "Male",
      # sex == "intersex" ~ "Inter-sex",
      # sex == "unknown" ~ "Unknown",
      TRUE ~ NA_character_
    ) %>% factor(),
    # ethnicity....
  )

# save processed fixed dataset
data_processed_fixed %>%
  select(
    patient_id,
    sex,
    #ethnicity...
    death_date
  ) %>%
  write_rds(fs::path(output_dir, "data_fixed.rds"), compress = "gz")


## delete in-memory objects to save space
rm(data_processed_fixed)
rm(data_extract_fixed)



# Import and process fixed dataset ----

# import
data_extract_varying <-
  import_extract(
    here("lib", "dummydata", "dummyinput_varying.arrow"),
    here("output", "extracts", "extract_varying.arrow")
  )

# function to standardise characteristics for each vaccination event
# doing this on wide format not long format as it reduces dataset size prior to reshaping
standardise_characteristics <- function(i) {
  rlang::quos(
    "ageband_{i}" := cut(
      .data[[glue("age_{i}")]],
      breaks = c(-Inf, 18, 40, 55, 65, 75, Inf),
      labels = c("under 18", "18-39", "40-54", "55-64", "65-74", "75+"),
      right = FALSE
    ),
    "region_{i}" := fct_collapse(
      .data[[glue("region_{i}")]],
      `East of England` = "East",
      `London` = "London",
      `Midlands` = c("West Midlands", "East Midlands"),
      `North East and Yorkshire` = c("Yorkshire and The Humber", "North East"),
      `North West` = "North West",
      `South East` = "South East",
      `South West` = "South West"
    )
  )
}

# apply standardisation across all events
data_processed_varying <-
  data_extract_varying %>%
  mutate(
    !!!standardise_characteristics(1),
    !!!standardise_characteristics(2),
    !!!standardise_characteristics(3),
    !!!standardise_characteristics(4),
    !!!standardise_characteristics(5),
    !!!standardise_characteristics(6),
    !!!standardise_characteristics(7),
    !!!standardise_characteristics(8),
    !!!standardise_characteristics(9),
    !!!standardise_characteristics(10),
    !!!standardise_characteristics(11),
    !!!standardise_characteristics(12),
    !!!standardise_characteristics(13),
    !!!standardise_characteristics(14),
    !!!standardise_characteristics(15),
    !!!standardise_characteristics(16),
  )


# Reshape vaccination data
data_vax <-
  data_processed_varying %>%
  select(
    patient_id,
    matches("covid_vax\\_\\d+\\_date"),
    matches("covid_vax_type_\\d+"),
    matches("registered_\\d+"),
    matches("deregistration_\\d+"),
    matches("age_\\d+"),
    matches("ageband_\\d+"),
    matches("region_\\d+"),
    matches("stp_\\d+"),
    #... more clinical characteristics here
  ) %>%
  pivot_longer(
    cols = -patient_id,
    names_to = c(".value", "vax_index"),
    names_pattern = "^(.*)_(\\d+)",
    values_drop_na = TRUE,
    names_transform = list(vax_index = as.integer)
  ) %>%
  rename(
    vax_date = covid_vax,
    vax_type = covid_vax_type,
  ) %>%
  arrange(patient_id, vax_date) %>%
  mutate(
    vax_type = fct_recode(factor(vax_type, vax_product_lookup), !!!vax_product_lookup) %>% fct_explicit_na("other")
  ) %>%
  group_by(patient_id) %>%
  mutate(
    vax_interval = as.integer(vax_date - lag(vax_date, 1))
  ) %>%
  ungroup()

# save dataset with all vaccines
write_rds(data_vax, fs::path(output_dir, "data_vax.rds"), compress = "gz")

# remove vaccinations occurring within 14 days of a previous vaccination
data_vax_clean <-
  # remove vaccine events occurring within 14 days of a previous vaccine event
  data_vax %>%
  filter(
    !is.na(vax_date),
    is.na(vax_interval) | vax_interval >= 14,
    vax_date >= start_date,
    vax_date <= end_date
  ) %>%
  group_by(patient_id) %>%
  mutate(
    vax_index = row_number()
  ) %>%
  ungroup()

# save ataset with <14-day vaccines removed
write_rds(data_vax_clean, fs::path(output_dir, "data_vax_clean.rds"), compress = "gz")
