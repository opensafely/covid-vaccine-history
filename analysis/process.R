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


# Import and process fixed dataset ----

# Import fixed dataset
#data_extract_fixed <-
#  import_extract(
#    here("lib", "dummydata", "dummyinput_fixed.arrow"),
#    here("output", "extracts", "extract_fixed.arrow")
#  )
 data_extract_fixed <- read_feather(here("output", "extracts", "extract_fixed.arrow"))

stopifnot(
  "inconsistency between ethnicity5 and ethnicity 16" = identical(data_extract_fixed$ethnicity5, ethnicity_16_to_5(data_extract_fixed$ethnicity16))
)


# Process snapshot dataset
data_processed_fixed <- data_extract_fixed %>%
  mutate(
    sex = fct_case_when(
      sex == "female" ~ "Female",
      sex == "male" ~ "Male",
      # sex == "intersex" ~ "Inter-sex",
      # sex == "unknown" ~ "Unknown",
      TRUE ~ NA_character_
    ),
    ethnicity5 = factor(ethnicity5, levels = factor_levels$ethnicity5),
    ethnicity16 = factor(ethnicity16, levels = factor_levels$ethnicity16) %>%
      fct_relabel(~ str_extract(.x, "(?<= - )(.*)")), # pick up everything after " - "
  )

# save processed fixed dataset
data_processed_fixed %>%
  select(
    patient_id,
    sex,
    ethnicity5,
    ethnicity16,
    death_date
  ) %>%
  write_rds(fs::path(output_dir, "data_fixed.rds"), compress = "gz")


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

data_extract_varying <- read_feather(here("output", "extracts", "extract_varying.arrow"))


# Reshape vaccination data
data_vax <-
  data_extract_varying %>%
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
    matches("imd_\\d+"),
    matches("imd_quintile_\\d+"),
    # ... more clinical (PRIMIS) characteristics here
    matches("primis_atrisk_\\d+"), # Clinically vulnerable
    matches("crd_\\d+"), #chronic respiratory disease
    matches("chd_\\d+"), #chronic heart disease
    matches("ckd_\\d+"), #chronic kidney disease
    matches("cld_\\d+"), # chronic liver disease
    matches("cns_\\d+"), # chronic neurological disease 
    matches("learndis_\\d+"), # learning Disability
    matches("diabetes_\\d+"), #diabetes
    matches("immunosuppressed_\\d+"), #immunosuppress grouped
    matches("asplenia_\\d+"), # asplenia or dysfunction of the spleen
    matches("severe_obesity_\\d+"), #immunosuppress grouped
    matches("smi_\\d+"), #severe mental illness
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
  mutate(
    !!!standardise_characteristics,
    vax_campaign = cut(
      vax_date,
      breaks = c(campaign_dates$start, end_date),
      labels = campaign_dates$campaign,
      include.lowest = TRUE, right = TRUE
    )
  )  %>%
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
