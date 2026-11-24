# _______________________________________________________________________________________
# Purpose:
# Report the distribution of vaccines schedules in different population subgroups as at a given date
# This is the orchestration script. Functions for analyses are in the report_snapshot_functions.R script
# _______________________________________________________________________________________


# Preliminaries ----

# Import libraries
library("tidyverse")
library("dtplyr")
library("lubridate")
library("glue")
library("here")
library("arrow")
library("survival")
library("splines")
library("fastglm")

# Import design functions
source(here("analysis", "0-lib", "design.R"))

# Import report-specific functions
source(here("analysis", "4-snapshot", "report_snapshot_functions.R"))

args <- commandArgs(trailingOnly = TRUE)

if (length(args) == 0) {
  # use for interactive testing
  # removeobjects <- FALSE
  snapshot_date <- as.Date("20220321", format = "%Y%m%d")
} else {
  # removeobjects <- TRUE
  snapshot_date <- as.Date(args[[1]], format = "%Y%m%d")
}

# how wide are the temporal bins for frequencies over time? in days
# use fix width, rather than months or bi-months or quarters, so that temporal denominator is even and the ratio of weekends to weekdays is fixed
temporal_resolution_history <- 28L

# how wide are the temporal bins for frequencies over time for Kaplan-Meier plots? in days
temporal_resolution_km <- 7L


# pull out campaign start dates for prior campaigns to help calculate which campaign each person's prior vaccine occurred in
prior_campaigns <- 
  campaign_info |>
  filter(campaign_start_date < snapshot_date & campaign_start_date >= study_dates$firstpossiblevax_date) |>
  transmute(
    campaign_start_date, 
    campaign_n_reverse = (n() - row_number() + 1),
    campaign_label = case_when(
      campaign_n_reverse == 1 & campaign_start_date >= study_dates$start_date ~ "1 campaign ago",
      campaign_n_reverse == 2 & campaign_start_date >= study_dates$start_date ~ "2 campaigns ago",
      campaign_n_reverse == 3 & campaign_start_date >= study_dates$start_date ~ "3 campaigns ago",
      campaign_n_reverse >= 4 & campaign_start_date >= study_dates$firstpossiblevax_date ~ "4+ campaigns ago",
      .default = campaign_label
    )
  )

# select info for selected campaign
selected_campaign_info <- campaign_info |> filter(campaign_start_date == snapshot_date)

# overwrite primary milestone to match rounded kaplan meier curve
selected_campaign_info$primary_milestone_days <- ceiling_any(
  selected_campaign_info$primary_milestone_days,
  temporal_resolution_km
) |>
  as.integer()

selected_campaign_milestone_days <-
  selected_campaign_info[, c(
    "early_milestone_days",
    "primary_milestone_days",
    "final_milestone_days"
  )] |>
  t() |>
  as.integer()

selected_campaign_milestone_dates <-
  selected_campaign_info[, c(
    "early_milestone_date",
    "primary_milestone_date",
    "final_milestone_date"
  )] |>
  t() |>
  as.Date()

# dates to round down to for period-specific vax dates
floor_dates <- seq(
  as.Date("2020-06-01"), # monday
  as.Date("2029-12-31"),  # to monday!
  by = temporal_resolution_history
)

# create string representation of date in compact format YYYMMDD
snapshot_date_compact <- format(snapshot_date, "%Y%m%d")

## Create output directory
output_dir <- here("output", "4-snapshot", glue("report_snapshot_{snapshot_date_compact}"))
fs::dir_create(output_dir)
options(width = 200) # set output width for capture.output

# Import and process cleaned vaccination data ----

# ideally we would do this step in ehrQL, but table_from_file doesn't work for eventFrames (ie, event level data), so we'll do it here instead
data_vax <- read_feather(here(
  "output",
  "2-prepare",
  "prepare",
  "data_vax_ELD_cleaned.arrow"
))

# rank vax date order
signed_index <- function(x) {
  rnk <- rank(x, ties.method = "first")
  xnonneg <- x >= 0
  anyxnonneg <- any(xnonneg)
  if (anyxnonneg) {
    index <- min(which(xnonneg))
  }
  if (!anyxnonneg) {
    index <- length(x) + 1
  }

  rnk - index
}

# select previous 3 and next 2 vaccines
data_vax_at_snapshot <-
  data_vax |>
  arrange(patient_id, vax_date) |>
  mutate(
    vax_index_relative = signed_index(vax_date - snapshot_date),
    .by = patient_id
  ) |>
  filter(
    between(vax_index_relative, -3, 1)
  ) |>
  mutate(
    name = if_else(
      vax_index_relative < 0,
      glue("covid_vax_prior_{-vax_index_relative}"),
      glue("covid_vax_{vax_index_relative+1}"),
    )
  )

# tranform to wide format
data_vax_wide <-
  data_vax_at_snapshot |>
  rename(
    product = vax_product,
    date = vax_date
  ) |>
  pivot_wider(
    id_cols = c(patient_id),
    names_from = name,
    names_glue = "{name}_{.value}",
    values_from = c(date, product),
    names_sort = TRUE,
    names_vary = "slowest"
  )

# count total number of documented prior vaccines
data_vax_prior_count <-
  data_vax |>
  filter(vax_date < snapshot_date) |>
  summarise(
    covid_vax_prior_count = n(),
    .by = patient_id
  )

# remove to release memory
rm(data_vax_at_snapshot)

# Import processed data ----
data_snapshot <- read_feather(here("output", "1-extract", glue("extract_snapshot_{snapshot_date_compact}.arrow")))
data_fixed <- read_feather(here("output", "2-prepare", "prepare", "data_fixed.arrow"))

capture.output(
  skimr::skim_without_charts(data_snapshot),
  file = fs::path(output_dir, "data_snapshot_skim.txt"),
  split = FALSE
)

## tests ----
if (any(coalesce((snapshot_date - data_vax_wide$covid_vax_prior_1_date)<=0, FALSE))) {
  stop("'covid_vax_prior_1_date' equals or exceeds 'snapshot_date'")
}

cat(
  glue(
    "To be excluded: number of people with vaccine dates prior to {study_dates$firstpossiblevax_date} = 
    {data_vax_wide |> filter(coalesce(covid_vax_prior_1_date<study_dates$firstpossiblevax_date, FALSE)) |> nrow() |> round_sdc()}
    "
  )
)

# merge fixed data and vaccine data onto snapshot data
# note that in dummy data this doesn't work very well because patient IDs might not be matched across all datasets
data_combined <-
  data_snapshot |>
  lazy_dt() |>
  left_join(
    lazy_dt(data_fixed) |>
      select(
        patient_id,
        sex,
        ethnicity5,
        ethnicity16,
        death_date,
        covid_death_date
      ),
    by = "patient_id"
  ) |>
  left_join(
    lazy_dt(data_vax_wide),
    by = "patient_id"
  ) |>
  left_join(
    lazy_dt(data_vax_prior_count),
    by = "patient_id"
  ) |>
  # remove anyone with a vaccination date prior to pandemic, since this indicates a vaccination probably occurred but we don't know when
  filter(coalesce(covid_vax_prior_1_date >= study_dates$firstpossiblevax_date, TRUE)) |>
  # remove currently unused variables
  select(
    -covid_vax_prior_2_date,
    -covid_vax_prior_2_product,
    -covid_vax_prior_3_date,
    -covid_vax_prior_3_product,
    -stp,
    -age # remove snapshot age and replace (below) with age_eligibility
  ) |>
  rename(
    # overwrite age to be that of a date later on in the campaign (rather than as at the start date) to allow "operational flexibility"
    age = eligibility_age,

    # previous vaccine summary
    # add more variables here based on covid_vax_prior_1_date, covid_vax_prior_2_date,... etc if needed
    vax_count = covid_vax_prior_count,
    last_vax_date = covid_vax_prior_1_date,
    last_vax_product = covid_vax_prior_1_product,

    # info on first vaccine(s) received after snapshot date
    next_vax_date = covid_vax_1_date,
    next_vax_product = covid_vax_1_product,
    # next2_vax_date = covid_vax_2_date,
  ) |>
  mutate(
    all = "All",

    !!!standardise_demographic_characteristics,
    !!!standardise_primis_and_extended_characteristics,

    age_above_eligibility_threshold = (age >= selected_campaign_info$age_threshold),

    # used to chose if the at risk group is all clinical risk variables or just immunosuppressed people
    clinical_priority = .data[[selected_campaign_info$clinical_priority]],

    clinical_priority_only = clinical_priority &
      !age_above_eligibility_threshold,

    any_eligibility = age_above_eligibility_threshold |
      clinical_priority |
      carehome_status,

    last_vax_product = fct_na_value_to_level(last_vax_product, "Unvaccinated"),
    last_vax_date = if_else(
      vax_count == 0,
      study_dates$firstpossiblevax_date + as.integer(runif(n(), 0, 10)),
      last_vax_date
    ),
    # last_vax_week = floor_date(last_vax_date, unit = "week", week_start = 1), # starting on a monday
    last_vax_period = floor_date(last_vax_date, unit = floor_dates), # round dates to a period, defined by "temporal_resolution_history" above
    # number of days since prior vaccination
    last_vax_time_since = as.numeric(snapshot_date - last_vax_date),

    # how many campaigns ago did prior vaccine occur
    last_vax_campaign_fct = last_vax_date |>
      floor_date(unit = prior_campaigns$campaign_start_date) |>
      recode_values(
        from = c(prior_campaigns$campaign_start_date, as.Date(NA)),
        to = c(prior_campaigns$campaign_label, "Unvaccinated")
      ),

    # last_vax_time_since_fct = cut(
    #   last_vax_time_since,
    #   breaks = c(0, 26*7, 52*7, Inf),
    #   labels = c("0-6 months", "9-14 months", "15+ months"),
    #   right = TRUE
    # ) |> fct_na_value_to_level(level = "none"),

    censor_date = pmin(
      deregistered_date,
      selected_campaign_info$final_milestone_date,
      study_dates$end_date,
      na.rm = TRUE
    ),

    # time from snapshot date until next vaccination
    vax_time = as.integer(
      pmin(next_vax_date, death_date, censor_date, na.rm = TRUE) - snapshot_date
    ) +
      1L, # +1 because vaccination on snapshot date is allowed, but events at time zero are not
    vax_indicator = coalesce(next_vax_date <= pmin(censor_date, death_date, na.rm = TRUE), FALSE),

    # time from snapshot date until covid hospital admission
    covid_admitted_time = as.integer(
      pmin(covid_admitted_date, death_date, censor_date, na.rm = TRUE) -
        snapshot_date
    ) +
      1L,
    covid_admitted_indicator = coalesce(
      covid_admitted_date <= pmin(censor_date, death_date, na.rm = TRUE),
      FALSE
    ),

    # time from snapshot date until covid (primary position only) hospital admission
    covid_admitted_primary_time = as.integer(
      pmin(covid_admitted_primary_date, death_date, censor_date, na.rm = TRUE) -
        snapshot_date
    ) +
      1L,
    covid_admitted_primary_indicator = coalesce(
      covid_admitted_primary_date <= pmin(censor_date, death_date, na.rm = TRUE),
      FALSE
    ),

    # time from snapshot date until covid critical care admission
    covid_critcare_time = as.integer(
      pmin(covid_critcare_date, death_date, censor_date, na.rm = TRUE) -
        snapshot_date
    ) +
      1L,
    covid_critcare_indicator = coalesce(
      covid_critcare_date <= pmin(censor_date, death_date, na.rm = TRUE),
      FALSE
    ),

    # time from snapshot date until covid death
    covid_death_time = as.integer(
      pmin(covid_death_date, death_date, censor_date, na.rm = TRUE) -
        snapshot_date
    ) +
      1L,
    covid_death_indicator = coalesce(
      covid_death_date <= pmin(censor_date, death_date, na.rm = TRUE),
      FALSE
    ),

    # time from snapshot date until death
    death_time = as.integer(
      pmin(death_date, censor_date, na.rm = TRUE) - snapshot_date
    ) +
      1L,
    death_indicator = coalesce(
      death_date <= pmin(censor_date, death_date, na.rm = TRUE),
      FALSE
    ),

    # time from snapshot date until deregistration
    deregistration_time = as.integer(
      pmin(deregistered_date, censor_date, na.rm = TRUE) - snapshot_date
    ) +
      1L,
    deregistration_indicator = coalesce(
      deregistered_date <= pmin(censor_date, na.rm = TRUE),
      FALSE
    ),

    # time from snapshot date until covid admission OR covid death
    covid_admitted_death_date = pmin(
      covid_admitted_date,
      covid_death_date,
      na.rm = TRUE
    ),
    covid_admitted_death_time = as.integer(
      pmin(covid_admitted_death_date, death_date, censor_date, na.rm = TRUE) - snapshot_date + 1
    ),
    covid_admitted_death_indicator = coalesce(
      covid_admitted_death_date <= pmin(censor_date, death_date, na.rm = TRUE),
      FALSE
    ),

    # time from snapshot date until covid critical care admission OR covid death
    covid_critcare_death_date = pmin(
      covid_critcare_date,
      covid_death_date,
      na.rm = TRUE
    ),
    covid_critcare_death_time = as.integer(
      pmin(covid_critcare_death_date, death_date, censor_date, na.rm = TRUE) - snapshot_date + 1
    ),
    covid_critcare_death_indicator = coalesce(
      covid_critcare_death_date <= pmin(censor_date, death_date, na.rm = TRUE),
      FALSE
    ),

    # indicator for if patient is alive and registered at the end of the campaign (for comparison with UKHSA reporting)
    alive_and_registered = (!death_indicator) & (!deregistration_indicator)
  ) |>
  as_tibble() |>
  mutate(
    vax_status = case_when(
      vax_indicator ~ "vaccinated",
      (death_date <= censor_date) & !is.na(death_date) ~ "died",
      (censor_date < death_date) | is.na(death_date) ~ "censored",
      .default = NA_character_
    ) |>
      factor(levels = c("censored", "vaccinated", "death")),
    vax_status_product = if_else(vax_indicator, next_vax_product, vax_status),
    next_vax_product_uncensored = if_else(
      vax_indicator,
      next_vax_product,
      NA_character_
    ),

    # setting missing values to explicit missing
    across(
      where(is.factor) | where(is.character),
      ~ fct_drop(fct_na_value_to_level(.x, level = "(Missing)"))
    )
  )

# remove snapshot and fixed datasets to release memory, as these are all captured in the data_combined dataset.
rm(data_snapshot, data_fixed, data_vax_wide, data_vax_prior_count)
# recover memory
gc()


capture.output(
  skimr::skim_without_charts(data_combined),
  file = fs::path(output_dir, "data_combined_skim.txt"),
  split = FALSE
)


# ________________________________________________________________________________________
## Data quality / validation tests ----
# ________________________________________________________________________________________

# check that vaccination event times are positive

times_count <- table(
  cut(
    data_combined$vax_time,
    c(-Inf, 0, 1, Inf),
    right = FALSE,
    labels = c("<0", "0", ">0")
  ),
  useNA = "ifany"
)

if (!identical(as.integer(times_count), c(0L, 0L, nrow(data_combined)))) {
  print(times_count)
  stop("all event times must be strictly positive")
}



# ________________________________________________________________________________________
# Summarise cohort ----
# ________________________________________________________________________________________

## quicker way to obtain cohort summaries rather than using the "plot_vax_count" function below
# consider this in future when vax_count is obsolete...

  # table1_summary(
  #   data_combined,
  #   .by = all,
  #   variables = level1_group,
  #   rounding.function = round_sdc
  # )

# ________________________________________________________________________________________
# Pre-snapshot date vaccine history, stratified by characteristic recorded on the snapshot_date ----
# ________________________________________________________________________________________


## _______________________________________________________________________________________
cat("## Report info on date of last vaccination", "\n")
## _______________________________________________________________________________________

for (group in level1_group) {
  plot_date_of_last_dose(data = data_combined, group)
}

## _______________________________________________________________________________________
cat("## Report info on prior dose count and product type", "\n")
## _______________________________________________________________________________________

for (group in level1_group) {
  plot_vax_count(data = data_combined, group)
}


## _______________________________________________________________________________________
cat("## Report info in a standardised table", "\n")
## _______________________________________________________________________________________

# loop over all group1 and group2 variable combinations and combine into one big dataset
prior_vax_summary_table_all <-
  level_combos |>
  mutate(
    prior_vax_summary = map2(
      group1,
      group2,
      .f = function(x, y) {
        if (is.na(y)) {
          y <- NULL
        }
        lookup <- c(group1_value = x, group2_value = y)

        table_prior_vax_summary(data = data_combined, x, y) |>
          mutate(across(c(all_of(c(x, y))), as.character)) |>
          rename(any_of(lookup))
      }
    )
  ) |>
  unnest(prior_vax_summary) |>
  select(group1, group1_value, group2, group2_value, everything())

# Write table to a CSV file
write_csv(prior_vax_summary_table_all, fs::path(output_dir, glue("prior_vax_table.csv")))


# ________________________________________________________________________________________
# Post-snapshot vaccine coverage, stratified by characteristic recorded on the snapshot_date ----
# ________________________________________________________________________________________

# This code borrows heavily from the KM reusable action https://github.com/opensafely-actions/kaplan-meier-function/blob/main/analysis/km.R
# The reason not to use the KM reusable action directly is that we need to reuse it multiple times across many different stratification variables
# This would create a large number of project.yaml actions, so it's easier to do the repeats within a single script
# The reusable action could be modified to enable multiple stratified analyses to be run, but it's a bit faffy and a bit scope-creepy

## Function to get km estimates over all subgroup combinations ----

get_all_km_estimates <- function(data, event_name, event_time, event_indicator, resolution) {
  # loop over all group1 and group2 variable combinations and combine into one big dataset
  km_estimates_table <-
    level_combos |>
    mutate(
      km_summary = map2(
        group1,
        group2,
        .f = \(x, y) {
          if (is.na(y)) {
            y <- NULL
          }
          lookup <- c(group1_value = x, group2_value = y)

          km_estimates(
            data = data,
            group_name1 = x,
            group_name2 = y,
            event_name = event_name,
            event_time = event_time,
            event_indicator = event_indicator,
            resolution = temporal_resolution_km
          ) |>
            mutate(across(c(all_of(c(x, y))), as.character)) |>
            rename(all_of(c(group1_value = x, group2_value = y)))
        }
      )
    ) |>
    unnest(km_summary) |>
    select(group1, group1_value, group2, group2_value, everything())

  # Write table to CSV files
  # split up by level1 grouping variables, so as not to exceed 5,000 row limit
  iwalk(
    split(km_estimates_table, km_estimates_table$group1),
    ~ write_csv(.x, fs::path(output_dir, glue("km_estimates_{event_name}_table_{.y}.csv")))
  )

  # Write table to a CSV file containing _only_ reporting milestones
  km_estimates_milestones <-
    km_estimates_table |>
    filter(
      time %in%
        (c(
          selected_campaign_info$early_milestone_days,
          selected_campaign_info$primary_milestone_days,
          selected_campaign_info$final_milestone_days
        ) *
          1L)
    ) |>
    mutate(
      milestone_date = recode_values(
        time,
        from = selected_campaign_milestone_days,
        to = selected_campaign_milestone_dates
      ),
      milestone = recode_values(
        time,
        from = selected_campaign_milestone_days,
        to = c("Early", "Primary", "Final")
      )
    ) |>
    rename(days_since_campaign_start = time)

  # extremely irritatingly we have to split this output up because it's too big to be shown (5000 row limit)
  # so split by milestone
  iwalk(
    split(km_estimates_milestones, km_estimates_milestones$milestone),
    ~ write_csv(.x, fs::path(output_dir, glue("km_estimates_{event_name}_milestones_{.y}.csv")))
  )

  return(km_estimates_table)
}

## _______________________________________________________________________________________
cat("## Report KM cumulative incidence of vaccination in a standardised table", "\n")
## _______________________________________________________________________________________

km_estimates_vax_table <-
  get_all_km_estimates(
    data = data_combined,
    event_name = "vax",
    event_time = "vax_time",
    event_indicator = "vax_indicator"
  )
km_estimates_vax_alive_table <-
  get_all_km_estimates(
    filter(data_combined, alive_and_registered),
    event_name = "vax_alive",
    event_time = "vax_time",
    event_indicator = "vax_indicator"
  )

# consider raw KM plots for disease burden too
# km_estimates_covid_admitted_table <- 
#   get_all_km_estimates(
#     data = data_combined, 
#     event_name = "covid_admitted", 
#     event_time = "covid_admitted_time", 
#     event_indicator = "covid_admitted_indicator"
#   )



## _______________________________________________________________________________________
cat("## Plot KM cumulative incidences of for inspection the server", "\n")
## _______________________________________________________________________________________

# plot km estimates for inspection the server
walk2(
  level_combos$group1,
  level_combos$group2,
  .f = \(x, y) {
    km_plot(
      km_data = km_estimates_vax_table |> filter(group1 == x, group2 == y),
      event_name = "vax",
      group1 = x,
      group2 = y
    )
  }
)


# ________________________________________________________________________________________
# Post-snapshot Covid-19 vaccination and disease burden expressed as a IRR up to final milestone, stratified by characteristics recorded on the snapshot_date ----
# ________________________________________________________________________________________


## function to loop over all group combinations and report IRR of level0 versus levelX ----
# for a given outcome, loop over all groups combinations, obtaining contrasts for each using adjusted_estimates function, and combining into one file
# specifically, compare level2 groups amongst each other, for all people meeting level1 group criteria
get_all_estimates <- function(data, event_name, event_time, event_indicator) {

  l1ticker <<- ""

  estimates_list <-
    level_combos |>
    mutate(
      estimates = map2(
        group1,
        group2,
        function(group1, group2) {
          # just to keep track of how far along we are
          if (l1ticker != group1) {
            print(group1)
            l1ticker <<- group1
          }

          summary_data <-
            data |>
            mutate(
              group1_value = data[[group1]],
            ) |>
            nest(.by = c(group1_value), .key = "group1_subset") |>
            mutate(
              estimates = map(group1_subset, \(group1_subset) {
                adjusted_estimates(group1_subset, group2, event_time, event_indicator)
              })
            ) |>
            select(-group1_subset) |>
            unnest(estimates) |>
            select(-variable) |>
            rename(group2_value = label) |>
            mutate(
              across(c(group1_value, group2_value), as.character) # to ensure the unnest() works later
            )

          return(summary_data)
          rm(summary_data)
          gc()
        }
      )
    ) |>
    unnest(estimates) |>
    select(group1, group1_value, group2, group2_value, everything()) # reorder columns

  write_csv(estimates_list, fs::path(output_dir, glue("contrasts_{event_name}.csv")))

  # return(estimates_list)

}


## _______________________________________________________________________________________
cat("## Get all IRR contrasts for covid vaccination and burden", "\n")
## _______________________________________________________________________________________

# IRR for vaccination, in the usual way
get_all_estimates(
  data_combined,
  "vax",
  "vax_time",
  "vax_indicator"
)

# IRR for vaccination, only looking at those who survived or stayed registered to the end of the season (collider bias! but matches UKHSA reports)
get_all_estimates(
  filter(data_combined, alive_and_registered),
  "vax_alive",
  "vax_time",
  "vax_indicator"
)

# IRR for COVID burden, in the usual way
get_all_estimates(data_combined, "covid_admitted", "covid_admitted_time", "covid_admitted_indicator")
get_all_estimates(data_combined, "covid_admitted_primary", "covid_admitted_primary_time", "covid_admitted_primary_indicator")
get_all_estimates(data_combined, "covid_critcare", "covid_critcare_time", "covid_critcare_indicator")
get_all_estimates(data_combined, "covid_death", "covid_death_time", "covid_death_indicator")
get_all_estimates(data_combined, "covid_admitted_death", "covid_admitted_death_time", "covid_admitted_death_indicator")
get_all_estimates(data_combined, "covid_critcare_death", "covid_critcare_death_time", "covid_critcare_death_indicator")


# ________________________________________________________________________________________
# Length of Covid-19 hospital stay, stratified by characteristics recorded on the snapshot_date ----
# ________________________________________________________________________________________


## function to get LoS across all group combinations ----
# for a given los outcome, loop over all groups combinations, obtaining los summaries for each using los_estimates function, and combining into one file
get_all_los_estimates <- function(data, event_name, event_los) {

  estimates_list <-
    level_combos |>
    mutate(
      estimates = map2(
        group1, group2,
        \(group1, group2) {

          data |>
            mutate(
              group1_value = data[[group1]],
            ) |>
            nest(.by = c(group1_value), .key = "group1_subset") |>
            mutate(
              estimates = map(group1_subset, \(group1_subset) {
                los_estimates(group1_subset, group2, event_los)
              })
            ) |>
            select(-group1_subset) |>
            unnest(estimates) |>
            select(-variable) |>
            rename(group2_value = label) |>
            mutate(
              across(c(group1_value, group2_value), as.character)
            )
        }
      )
    ) |>
    unnest(estimates) |>
    select(group1, group1_value, group2, group2_value, everything()) # reorder columns

  write_csv(estimates_list, fs::path(output_dir, glue("los_{event_name}.csv")))

}

## _______________________________________________________________________________________
cat("## Get all LOS values for burden", "\n")
## _______________________________________________________________________________________

get_all_los_estimates(data_combined, "covid_admitted", "covid_admitted_los")
