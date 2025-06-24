# _________________________________________________
# Purpose:
# import vaccination date data extracted by cohort extractor
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
options(width=200) # set output width for capture.output

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
    ethnicity5 = factor(ethnicity5, levels = factor_levels$ethnicity5),
    ethnicity16 = factor(ethnicity16, levels = factor_levels$ethnicity16) |>
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
    death_date
  ) |>
  write_feather(fs::path(output_dir, "data_fixed.arrow"))



## delete in-memory objects to save space
rm(data_processed_fixed)
rm(data_extract_fixed)


# Import and process fixed dataset ----

# import
#data_extract_varying <-
#  import_extract(
#    here("lib", "dummydata", "dummyinput_varying.arrow"),
#    here("output", "extracts", "extract_varying.arrow")
#  )

data_extract_varying <- read_feather(here("output", "1-extract", "extract_varying", "dataset.arrow"))

# Reshape vaccination data
data_vax <-
  data_extract_varying |>
  #lazy_dt() |>
  select(
    patient_id,
    matches("covid_vax\\_\\d+\\_date"),
    matches("covid_vax_product_\\d+"),
    matches("registered_\\d+"),
    matches("deregistered_\\d+"),
    matches("age_\\d+"),
    matches("ageband_\\d+"),
    matches("region_\\d+"),
    matches("stp_\\d+"),
    matches("imd_\\d+"),
    matches("imd_quintile_\\d+"),

    # PRIMIS variables
#    matches("primis_atrisk_\\d+") #, # Clinically vulnerable
#    matches("crd_\\d+"), # chronic respiratory disease
#    matches("chd_\\d+"), # chronic heart disease
#    matches("ckd_\\d+"), # chronic kidney disease
#    matches("cld_\\d+"), # chronic liver disease
#    matches("cns_\\d+"), # chronic neurological disease
#    matches("learndis_\\d+"), # learning Disability
#    matches("diabetes_\\d+"), # diabetes
#    matches("immunosuppressed_\\d+"), # immunosuppressed
#    matches("asplenia_\\d+"), # asplenia or dysfunction of the spleen
#    matches("severe_obesity_\\d+"), # obesity
#    matches("smi_\\d+"), #severe mental illness
  ) |>
  pivot_longer(
    cols = -patient_id,
    names_to = c(".value", "vax_index"),
    names_pattern = "^(.*)_(\\d+)",
    # values_drop_na = TRUE, # this causes an error in dtplyr - replace with filter(!is_na(covid_vax))
    # names_transform = list(vax_index = as.integer) # not supported by dtplyr - use vax_index = as.integer(vax_index) in a mutate step
  ) |>
  filter(!is.na(covid_vax)) |>
  mutate(
    vax_index = as.integer(vax_index)
  ) |>
  rename(
    vax_date = covid_vax,
    vax_product = covid_vax_product,
  ) |>
  #as_tibble() |> # insert this here to revert to standard dplyr as `cut` function doesn't work with dtplyr
  mutate(
    !!!standardise_characteristics,
    vax_campaign = cut(
      vax_date,
      breaks = c(campaign_dates$start, end_date),
      labels = campaign_dates$campaign,
      include.lowest = TRUE, right = TRUE
    )
  ) |>
  arrange(patient_id, vax_date) |>
  mutate(
    vax_product_raw = vax_product,
    vax_product = fct_recode(factor(vax_product, vax_product_lookup), !!!vax_product_lookup) |> fct_na_value_to_level("other")
  ) |>
  group_by(patient_id) |>
  mutate(
    vax_interval = as.integer(vax_date - lag(vax_date, 1))
  ) |>
  ungroup()

capture.output(
  skimr::skim_without_charts(data_vax),
  file = fs::path(output_dir, "data_vax_skim.txt"),
  split = FALSE
)

# save dataset with all vaccines
write_feather(data_vax, fs::path(output_dir, "data_vax.arrow"))

data_vax |>
  mutate(
    vax_product_raw = if_else(is.na(vax_product_raw), "NULL", vax_product_raw),
  ) |>
  group_by(vax_product_raw) |>
  summarise(
    n = ceiling_any(n(), 100),
  ) |>
  write_csv(fs::path(output_dir, "vax_product_count.csv"))


# remove vaccinations occurring within 14 days of a previous vaccination
data_vax_clean <-
  # remove vaccine events occurring within 14 days of a previous vaccine event
  data_vax |>
  filter(
    !is.na(vax_date),
    is.na(vax_interval) | vax_interval >= 14,
    vax_date >= start_date,
    vax_date <= end_date
  ) |>
  group_by(patient_id) |>
  mutate(
    vax_index = row_number()
  ) |>
  ungroup()

capture.output(
  skimr::skim_without_charts(data_vax_clean),
  file = fs::path(output_dir, "data_vax_clean_skim.txt"),
  split = FALSE
)

# save dataset with <14-day vaccines removed
write_feather(data_vax_clean, fs::path(output_dir, "data_vax_clean.arrow"))


# Test equivalence of ELD extract ----

data_vax_ELD0 <- read_feather(here("output", "1-extract", "extract_varying", "vaccinations.arrow"))

data_vax_ELD <-
  data_vax_ELD0 |>
  filter(!is.na(vax_date)) |>
  group_by(patient_id) |>
  filter((vax_date!=lag(vax_date)) | row_number()==1) |>
  mutate(vax_index = row_number()) |>
  filter(row_number()<=16) |>
  ungroup()

data_vax_PLD <-
  data_vax |>
  select(patient_id, vax_date, vax_product = vax_product_raw, age, vax_index)


# check equality of datasets
cat(
  "are datasets from ELD versus PLD identical after some standardisation? \n",
  all.equal(data_vax_ELD, data_vax_PLD)
)


# report multiple vaccinations on the same day
cat(
  "number of occassions where a person is vaccinated more than once in a day:\n",
  data_vax_ELD0 |>
  group_by(patient_id, vax_date) |>
  summarise(n=n()) |>
  filter(n>1) |>
  nrow()
)

# report no vax date
cat(
  "number of occassions where a person is vaccinated with a null date:\n",
  data_vax_ELD0 |>
  filter(is.na(vax_date)) |>
  summarise(n=n()) |>
  pull(n)
)
