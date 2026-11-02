# _________________________________________________
# Purpose:
# import vaccination date data extracted by ehrql
# organise vaccination date data to "vax X product", "vax X date" (rather than "pfizer X date", "az X date", ...)
# _________________________________________________

# Preliminaries ----

# Import libraries
library("tidyverse")
library("dtplyr")
library("lubridate")
library("arrow")
library("here")
library("glue")

# Import custom functions
source(here("analysis", "0-lib", "design.R"))

# create output directory
output_dir <- here("output", "2-prepare", "prepare")
fs::dir_create(output_dir)
options(width = 200) # set output width for capture.output

# Import and process fixed dataset ----

# Import fixed dataset
data_extract_fixed <- read_feather(here("output", "1-extract", "extract_fixed.arrow"))

stopifnot(
  "inconsistency between ethnicity5 and ethnicity 16" = identical(data_extract_fixed$ethnicity5, ethnicity_16_to_5(data_extract_fixed$ethnicity16))
)

# print details about dataset
capture.output(
  skimr::skim_without_charts(data_extract_fixed),
  file = fs::path(output_dir, "data_extract_fixed_skim.txt"),
  split = FALSE
)

# Process snapshot dataset
data_processed_fixed <-
  data_extract_fixed |>
  lazy_dt() |>
  mutate(
    sex = fct_case_when(
      sex == "female" ~ "Female",
      sex == "male" ~ "Male",
      # sex == "intersex" ~ "Inter-sex",
      # sex == "unknown" ~ "Unknown",
      TRUE ~ NA_character_
    ),
    ethnicity5 = factor(ethnicity5, levels = factor_levels$ethnicity5, ordered = FALSE),
    ethnicity16 = factor(ethnicity16, levels = factor_levels$ethnicity16, ordered = FALSE) |>
      fct_relabel(~ str_extract(.x, "(?<= - )(.*)")), # pick up everything after " - "
  ) |>
  as_tibble()

# print details about dataset
capture.output(
  skimr::skim_without_charts(data_processed_fixed),
  file = fs::path(output_dir, "data_processed_fixed_skim.txt"),
  split = FALSE
)

# save processed fixed dataset
data_processed_fixed |>
  select(
    patient_id,
    sex,
    ethnicity5,
    ethnicity16,
    death_date,
    covid_death_date
  ) |>
  write_feather(fs::path(output_dir, "data_fixed.arrow"))


## delete in-memory objects to save space
rm(data_processed_fixed)
rm(data_extract_fixed)



# extract event level data for vaccines ----

data_vax_ELD0 <- read_feather(here(
  "output",
  "1-extract",
  "extract_varying",
  "vaccinations.arrow"
))

# when running locally, create some dummy data to make outputs more sensible
# this should NOT EVER be run on the real data, as it will overwrite real data
if (localrun) {
  data_vax_ELD0 <-
    data_vax_ELD0 |>
    mutate(
      vax_date = as.Date(runif(
        n(),
        as.Date("2019-01-12"),
        max(campaign_info$early_milestone_date)
      )),
      vax_product = sample(product_info$product_tpp_name, n(), replace = TRUE)
    )
}


# - remove rows where vaccination date is missing
# - attach info about the campaign during which the vaccination was given
# - collapse exact duplicates (where patient id, date, and product all match)
data_vax_ELD <-
  data_vax_ELD0 |>
  lazy_dt() |>
  arrange(patient_id, vax_date) |>
  # undated vaccines are a pain, but as v low frequency the easiest thing is to remove them
  filter(!is.na(vax_date)) |>
  # remove exact duplicates, as these are extremely likely to be duplicates rather than multiple same-product vaccines on the same day
  distinct(.keep_all = TRUE) |>
  as_tibble() |>
  mutate(
    # convert long product name to short version for ease
    vax_product_raw = vax_product,
    vax_product = factor(
      vax_product,
      levels = product_info$product_tpp_name,
      labels = product_info$product_short_name
    ) |>
      fct_na_value_to_level("UNMAPPED"),
    # in which campaign did the vaccine occur
    campaign = cut(
      vax_date,
      breaks = c(campaign_info$campaign_start_date, as.Date(Inf)),
      labels = campaign_info$campaign_label
    ),
    # what is the campaign start date
    campaign_start = recode_values(
      campaign,
      from = campaign_info$campaign_label,
      to = campaign_info$campaign_start_date
    )
  ) 

# print details about dataset
capture.output(
  skimr::skim_without_charts(data_vax_ELD),
  file = fs::path(output_dir, "data_vax_ELD_skim.txt"),
  split = FALSE
)
write_feather(data_vax_ELD, fs::path(output_dir, "data_vax_ELD.arrow"))


data_vax_ELD_cleaned <-
  data_vax_ELD |>
  lazy_dt() |>
  mutate(
    uk_approval_date = recode_values(
      vax_product,
      from = product_info$product_short_name,
      to = product_info$uk_approval_date
    ),
    first_campaign_date = recode_values(
      vax_product,
      from = product_info$product_short_name,
      to = product_info$first_campaign_date
    )
  ) |>
  # Clean vaccine data for analysis where we care about uptake of vaccines in UK vaccine porgrammes
  # - not interested in product-specific uptake, but want to exclude vaccines definitely given outside of UK
  # - exlcude vaccines that are almost certainly duplicates (eg if recorded as given on the same day or near the same day)
  filter(
    # remove vaccines not approved for use in the UK
    # for now, ignore if approved vaccine is dated before first use (or after last use). This could mean [correct product wrong date] or [wrong date correct product]
    !is.na(uk_approval_date)
  ) |>
  mutate(
    vax_interval = as.integer(vax_date - lag(vax_date, 1)),
    .by = patient_id
  ) |>
  filter(
    # remove vaccines occurring with 14 days of previous vaccine
    is.na(vax_interval) | vax_interval >= 14,
  )

  # save to disk and rename to original vaccination table names
  data_vax_ELD_cleaned |>
    as_tibble() |>
    transmute(
      patient_id,
      date = vax_date,
      product_name = vax_product
    ) |>
    write_feather(fs::path(output_dir, "data_vax_ELD_cleaned.arrow"))

