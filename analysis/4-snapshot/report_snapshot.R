# _______________________________________________________________________________________
# Purpose:
# Report the distribution of vaccines schedules in different population subgroups as at a given date
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
library("parglm")

# Import custom functions
source(here("analysis", "0-lib", "design.R"))

args <- commandArgs(trailingOnly = TRUE)

if (length(args) == 0) {
  # use for interactive testing
  # removeobjects <- FALSE
  snapshot_date <- as.Date("20210906", format = "%Y%m%d")
} else {
  # removeobjects <- TRUE
  snapshot_date <- as.Date(args[[1]], format = "%Y%m%d")
}

# how wide are the temporal bins for frequencies over time? in days
# use fix width, rather than months or bi-months or quarters, so that temporal denominator is even and the ratio of weekends to weekdays is fixed
temporal_resolution_history <- 28L

# how wide are the temporal bins for frequencies over time for Kaplan-Meier plots? in days
temporal_resolution_km <- 7L

campaign_info <- campaign_info |> filter(campaign_start_date == snapshot_date)

# list2env(campaign_info, globalenv())

# overwrite primary milestone to match rounded kaplan meier curve
campaign_info$primary_milestone_days <- ceiling_any(campaign_info$primary_milestone_days, temporal_resolution_km)


# dates to round down to
# use this with `findInterval` until lubridate package is updated in the opensafely R image
# (then use `floor_date(date, unit=floor_dates`)
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


# Import processed data ----
data_snapshot <- read_feather(here("output", "1-extract", glue("extract_snapshot_{snapshot_date_compact}.arrow")))
data_fixed <- read_feather(here("output", "2-prepare", "prepare", "data_fixed.arrow"))

capture.output(
  skimr::skim_without_charts(data_snapshot),
  file = fs::path(output_dir, "data_snapshot_skim.txt"),
  split = FALSE
)

# merge fixed data and vaccine data onto snapshot data
# note that in dummy data this doesn't work very well because patient IDs might not be matched across all datasets
data_combined <-
  data_snapshot |>
  lazy_dt() |>
  left_join(
    lazy_dt(data_fixed) |> select(patient_id, sex, ethnicity5, ethnicity16, death_date, covid_death_date),
    by = "patient_id"
  ) |>
  mutate(
    all = "All",
    !!!standardise_demographic_characteristics,
    # !!!ckd_rrt_classification,
    cns_learndis = (cns | learndis),
    immunosuppressed_asplenia = (immunosuppressed | asplenia),

    # should be the same as primis_atrisk
    # cv = (crd | chd |  ckd | cld | cns_learndis | diabetes | immunosuppressed_asplenia | severe_obesity | smi),

    age_above_eligiblity_threshold = (age >= campaign_info$age_threshold),
    primis_atrisk_only = primis_atrisk & !age_above_eligiblity_threshold,

    any_eligibility = age_above_eligiblity_threshold | primis_atrisk | carehome_status,

    # previous vaccine summary
    # add more variables here based on covid_vax_prior_1_date, covid_vax_prior_2_date,... etc if needed
    vax_count = covid_vax_prior_count,
    vax_count_group = cut(vax_count, c(-Inf, 0, 2, 4, Inf), labels = c("0", "1-2", "3-4", "5+")),
    last_vax_date = covid_vax_prior_1_date,
    last_vax_product = covid_vax_prior_1_product,
    days_since_vax = snapshot_date - last_vax_date,

    last_vax_product = fct_na_value_to_level(last_vax_product, "Unvaccinated"),
    last_vax_date = if_else(vax_count == 0, study_dates$firstpossiblevax_date + as.integer(runif(n(), 0, 10)), last_vax_date),
    last_vax_week = floor_date(last_vax_date, unit = "week", week_start = 1), # starting on a monday
    last_vax_period = floor_date(last_vax_date, unit = floor_dates), # use floor_dates[findInterval(last_vax_date, floor_dates)] if lubridate isn't working

    # info on first vaccine(s) received after snapshot date
    next_vax_date = covid_vax_1_date,
    next_vax_product = covid_vax_1_product,
    next2_vax_date = covid_vax_2_date,

    censor_date = pmin(
      deregistered_date,
      campaign_info$final_milestone_date,
      na.rm = TRUE
    ),

    # time from snapshot date until next vaccination
    vax_time = as.integer(pmin(next_vax_date, death_date, censor_date, na.rm = TRUE) - snapshot_date) + 1L, # +1 because vaccination on snapshot date is allowed, but events at time zero are not
    vax_indicator = (next_vax_date <= pmin(censor_date, death_date, na.rm = TRUE)) & !is.na(next_vax_date),

    # time from snapshot date until covid hospital admission
    covid_admitted_time = as.integer(pmin(covid_admitted_date, death_date, censor_date, na.rm = TRUE) - snapshot_date) + 1L,
    covid_admitted_indicator = (covid_admitted_date <= pmin(censor_date, death_date, na.rm = TRUE)) & !is.na(covid_admitted_date),

    # time from snapshot date until covid critical care admission
    covid_critcare_time = as.integer(pmin(covid_critcare_date, death_date, censor_date, na.rm = TRUE) - snapshot_date) + 1L,
    covid_critcare_indicator = (covid_critcare_date <= pmin(censor_date, death_date, na.rm = TRUE)) & !is.na(covid_critcare_date),

    # time from snapshot date until covid death
    covid_death_time = as.integer(pmin(covid_death_date, death_date, censor_date, na.rm = TRUE) - snapshot_date) + 1L,
    covid_death_indicator = (covid_death_date <= pmin(censor_date, death_date, na.rm = TRUE)) & !is.na(covid_death_date),
  ) |>
  filter(
    # only consider people with documented eligibility
    any_eligibility
  ) |>
  as_tibble() |>
  mutate(
    vax_status = case_when(
      vax_indicator ~ "vaccinated",
      (death_date <= censor_date) & !is.na(death_date) ~ "died",
      (censor_date < death_date) | is.na(death_date) ~ "censored",
      .default = NA_character_
    ) |> factor(levels = c("censored", "vaccinated", "death")),
    vax_status_product = if_else(vax_indicator, next_vax_product, vax_status),
    next_vax_product_uncensored = if_else(vax_indicator, next_vax_product, NA_character_),

    # setting missing values to explicit missing
    across(where(is.factor) | where(is.character), ~ fct_drop(fct_na_value_to_level(.x, level = "(Missing)")))
  )

capture.output(
  skimr::skim_without_charts(data_combined),
  file = fs::path(output_dir, "data_combined_skim.txt"),
  split = FALSE
)

# ________________________________________________________________________________________
# Pre-snapshot date vaccine history, stratified by characteristic recorded on the snapshot_date ----
# ________________________________________________________________________________________


## _______________________________________________________________________________________
## Report info on date of last vaccination
## _______________________________________________________________________________________

# This produces data and a plot showing the distribution of most recent prior vaccination date (to the nearest 4 weeks)
# if no documented prior vaccination, then values are stacked on the LHS of the chart

plot_date_of_last_dose <- function(subgroup) {

  over2years_dummy_date <- (snapshot_date - ceiling_any(365 * 2, 7))

  summary_by <-
    data_combined |>
    lazy_dt() |>
    group_by({{ subgroup }}, last_vax_product, last_vax_period) |>
    summarise(
      n = round_any(n(), sdc_threshold)
    ) |>
    ungroup() |>
    mutate(
      # if last vaccination date was over 2 years ago, replace with dummy date
      last_vax_period = if_else(
        (last_vax_period < over2years_dummy_date)  | is.na(last_vax_period),
        over2years_dummy_date - 42,
        last_vax_period
      )
    ) |>
    as_tibble() |>
    complete(
      {{ subgroup }}, last_vax_product, last_vax_period,
      fill = list(n = 0)
    )

  breaks <- seq(as.Date("2021-01-01"),  as.Date("2028-06-01"), by = "6 month")

  temp_plot <-
    summary_by |>
    ggplot() +
    geom_col(
      aes(x = last_vax_period, y = n, fill = last_vax_product, group = last_vax_product),
      alpha = 0.5,
      position = position_stack(reverse = TRUE),
      # position=position_identity(),
      width = temporal_resolution_history
    ) +
    facet_grid(
      rows = vars({{ subgroup }}),
      switch = "y",
      space = "free_x",
      scales = "free_x"
    ) +
    labs(
      x = "Date of most recent COVID-19 vaccine",
      y = NULL,
      fill = NULL
    ) +
    scale_fill_brewer(
      palette = "Set2",
      na.value = "grey50",
      labels = function(breaks) {
        breaks[is.na(breaks)] <- "Other"
        breaks
      }
    ) +
    scale_x_date(
      breaks = c(over2years_dummy_date - 42, breaks),
      date_minor_breaks = "month",
      # labels = ~{c("Unvaccinated", scales::label_date("%Y")(.x[-1]))},
      labels = c("+2 years prior)", scales::label_date("%Y-%b")(breaks)),
    ) +
    theme_minimal() +
    theme(
      axis.text.x.top = element_text(hjust = 0),
      axis.text.x.bottom = element_text(hjust = 0),
      strip.text.y.left = element_text(angle = 0, hjust = 1),
      axis.ticks.x = element_line(),
      strip.placement = "outside",
      axis.text.y = element_blank(),
      legend.position = "bottom"
    )

  # print(temp_plot)

  subgroup_name <- deparse(substitute(subgroup))
  # col_name = deparse(substitute(cols))

  ggsave(fs::path(output_dir, glue("last_vax_date_{subgroup_name}.png")), plot = temp_plot)

  # write tables that capture underlying plotting data
  # write_csv(summary_by, fs::path(output_dir, glue("last_vax_date_{subgroup_name}.csv")))
}


## --VARIABLES--

# Demographic
plot_date_of_last_dose(all)
plot_date_of_last_dose(sex)
plot_date_of_last_dose(ageband4)
plot_date_of_last_dose(ethnicity5)
plot_date_of_last_dose(region)
plot_date_of_last_dose(imd_quintile)
plot_date_of_last_dose(carehome_status)

# PRIMIS
plot_date_of_last_dose(crd) # chronic respiratory disease
plot_date_of_last_dose(chd) # chronic heart disease
plot_date_of_last_dose(ckd) # chronic kidney disease
plot_date_of_last_dose(cld) # chronic liver disease
plot_date_of_last_dose(cns) # chronic neurological
plot_date_of_last_dose(cns_learndis) # chronic neurological or learning disability
plot_date_of_last_dose(diabetes) # diabetes
plot_date_of_last_dose(immunosuppressed_asplenia) # immunosuppressed or asplenia
plot_date_of_last_dose(severe_obesity) # obesity
plot_date_of_last_dose(smi) # severe mental illness
plot_date_of_last_dose(primis_atrisk) # clinically vulnerable

# Extended subgroups
plot_date_of_last_dose(ckd_rrt) # Chronic kidney disease classification
plot_date_of_last_dose(copd) # Chronic obstructive pulmonary disease
plot_date_of_last_dose(down_sydrome)
plot_date_of_last_dose(sickle_cell)

## _______________________________________________________________________________________
## Report info on prior dose count and product type
## _______________________________________________________________________________________

plot_vax_count <- function(subgroup) {
  summary_by <-
    data_combined |>
    lazy_dt() |>
    group_by({{ subgroup }}, vax_count_group) |>
    summarise(
      n = round_any(n(), sdc_threshold),
    ) |>
    ungroup() |>
    as_tibble() |>
    complete(
      {{ subgroup }}, vax_count_group,
      fill = list(n = 0)
    ) |>
    group_by({{ subgroup }}) |>
    mutate(
      row_total = sum(n),
      prop = ifelse(row_total > 0, n / row_total * 100, 0),
    ) |>
    ungroup()

  temp_plot <-
    ggplot(summary_by) +
    geom_bar(
      aes(x = prop, y = {{ subgroup }}, width = row_total, fill = vax_count_group),
      stat = "identity", # position = "fill",
      position = position_stack(reverse = TRUE),
    ) +
    facet_grid(
      rows = vars({{ subgroup }}),
      scales = "free_y",
      space = "free_y") +
    labs(
      x = "%",
      y = NULL,
      fill = "Vaccine count"
    ) +
    scale_fill_brewer(
      palette = "Set2",
      na.value = "grey50",
      labels = function(breaks) {
        breaks[is.na(breaks)] <- "(Missing)"
        breaks
      }
    ) +
    theme_minimal() +
    theme(
      axis.text.x.top = element_text(hjust = 0),
      axis.text.x.bottom = element_text(hjust = 0),
      strip.text.y.left = element_text(angle = 0, hjust = 1),
      axis.ticks.x = element_line(),
      strip.text = element_blank(),
      legend.position = "bottom"
    ) +
    NULL

  # print(temp_plot)

  subgroup_name <- deparse(substitute(subgroup))
  # col_name = deparse(substitute(cols))

  ggsave(fs::path(output_dir, glue("vax_count_{subgroup_name}.png")), plot = temp_plot)

  # write tables that capture underlying plotting data
  write_csv(summary_by, fs::path(output_dir, glue("vax_count_{subgroup_name}.csv")))
}


## --VARIABLES--

# Demographic
plot_vax_count(all)
plot_vax_count(sex)
plot_vax_count(ageband4)
plot_vax_count(ethnicity5)
plot_vax_count(region)
plot_vax_count(imd_quintile)
plot_vax_count(carehome_status)


# PRIMIS
plot_vax_count(crd) # chronic respiratory disease
plot_vax_count(chd) # chronic heart disease
plot_vax_count(ckd) # chronic kidney disease
plot_vax_count(cld) # chronic liver disease
plot_vax_count(cns_learndis) # chronic neurological disease or learning disability
plot_vax_count(diabetes) # diabetes
plot_vax_count(immunosuppressed_asplenia) # immunosuppressed or asplenia
plot_vax_count(severe_obesity) # obesity
plot_vax_count(smi) # severe mental illness
plot_vax_count(primis_atrisk) # clinically vulnerable

# Extended subgroups

plot_vax_count(ckd_rrt) # Chronic kidney disease classification
plot_vax_count(copd) # Chronic obstructive pulmonary disease
plot_vax_count(down_sydrome)
plot_vax_count(sickle_cell)

## _______________________________________________________________________________________
## Report info in a standardised table
## _______________________________________________________________________________________

# function to print table for an abritrary number of grouping variables

# groups are passed as strings via dots (...)
table_prior_vax_summary <- function(...) {

  group_names <- c(...)

  summary_table <-
    data_combined |>
    group_by(across(all_of(group_names))) |>
    lazy_dt() |>
    summarise(
      # Dose counts
      total = round_any(n(), sdc_threshold),
      count_n0 = round_any(sum(vax_count == 0, na.rm = TRUE), sdc_threshold),
      count_n1 = round_any(sum(vax_count == 1, na.rm = TRUE), sdc_threshold),
      count_n2 = round_any(sum(vax_count == 2, na.rm = TRUE), sdc_threshold),
      count_n3 = round_any(sum(vax_count == 3, na.rm = TRUE), sdc_threshold),
      count_n4 = round_any(sum(vax_count == 4, na.rm = TRUE), sdc_threshold),
      count_n5plus = round_any(sum(vax_count >= 5, na.rm = TRUE), sdc_threshold),
      # Dose summary
      count_median = quantile(vax_count, probs = 0.5, na.rm = TRUE),
      count_p10 = quantile(vax_count, probs = 0.10, na.rm = TRUE),
      count_p25 = quantile(vax_count, probs = 0.25, na.rm = TRUE),
      count_p75 = quantile(vax_count, probs = 0.75, na.rm = TRUE),
      count_p90 = quantile(vax_count, probs = 0.90, na.rm = TRUE),
      # Vaccination in past 12 and 24 months
      days_since_n12m = round_any(sum(days_since_vax <= 365, na.rm = TRUE), sdc_threshold),
      days_since_n24m = round_any(sum(days_since_vax <= 365 * 2, na.rm = TRUE), sdc_threshold),
      # Time since last dose
      days_since_median = quantile(days_since_vax, probs = 0.5, na.rm = TRUE),
      days_since_p10 = quantile(days_since_vax, probs = 0.10, na.rm = TRUE),
      days_since_p25 = quantile(days_since_vax, probs = 0.25, na.rm = TRUE),
      days_since_p75 = quantile(days_since_vax, probs = 0.75, na.rm = TRUE),
      days_since_p90 = quantile(days_since_vax, probs = 0.90, na.rm = TRUE),

      .groups = "drop"
    ) |>
    mutate(
      # Dose percentages - put this here and not in earlier summarise step so that it works with dtplyr
      count_pct0 = round(count_n0 * 100 / total, 1),
      count_pct1 = round(count_n1 * 100 / total, 1),
      count_pct2 = round(count_n2 * 100 / total, 1),
      count_pct3 = round(count_n3 * 100 / total, 1),
      count_pct4 = round(count_n4 * 100 / total, 1),
      count_pct5plus = round(count_n5plus * 100 / total, 1),
      # Vaccination % in past 12 and 24 months
      days_since_pct12m = round(days_since_n12m * 100 / total, 1),
      days_since_pct24m = round(days_since_n24m * 100 / total, 1),
    ) |>
    as_tibble()

  # subgroup_name <- map_chr(rlang::quos(...), rlang::as_name) |> paste0(collapse = "_")

  # Write table to a CSV file
  # write_csv(summary_table, fs::path(output_dir, glue("prior_vax_table_{subgroup_name}.csv")))

  return(summary_table)
}


# for testing function interactively
# table_prior_vax_summary("all", "all")
# table_prior_vax_summary("all", "ageband4")
# table_prior_vax_summary("ageband4", "all")
# table_prior_vax_summary("ageband4", "sex")

# loop over all group1 and group2 variable combinations and combine into one big dataset
prior_vax_summary_table_all <-
  level_combos |>
  mutate(
    prior_vax_summary = map2(
      group1, group2,
      .f = function(x, y) {

        if (is.na(y)) y <- NULL
        lookup <- c(group1_value = x, group2_value = y)

        table_prior_vax_summary(x, y) |>
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


## tests ----

times_count <- table(cut(data_combined$vax_time, c(-Inf, 0, 1, Inf), right = FALSE, labels = c("<0", "0", ">0")), useNA = "ifany")

if (!identical(as.integer(times_count), c(0L, 0L, nrow(data_combined)))) {
  print(times_count)
  stop("all event times must be strictly positive")
}

## Function to calculate KM estimates for a given stratification variable ----

# group variables are provided as characters via dots (...)
# resolution argument is the precision used for the time dimension. If zero, then original resolution is used.
km_estimates <- function(group_name1, group_name2, event_name, event_time, event_indicator, resolution = 0) {


  group_names <- c(group_name1, group_name2)

  # if (is.na(group_name2)) {
  #   group_name2 <- "all"
  # }

  data_outcome <-
    data_combined |>
    select(
      patient_id,
      all_of(group_names),
      event_time = {{ event_time }},
      event_indicator = {{ event_indicator }}
    ) |>
    mutate(event_time = ceiling_any(event_time, resolution))

  data_km <-
    data_outcome |>
    group_by(across(all_of(group_names))) |>
    nest() |>
    mutate(
      surv_obj_tidy = map(
        data, ~ {
          survfit(
            Surv(event_time, event_indicator) ~ 1,
            data = .x,
            conf.type = "log-log"
          ) |>
            broom::tidy() |>
            add_row(
              time = 0, # assumes time origin is zero
              n.risk = 0,
              n.event = 0,
              n.censor = 0,
              estimate = 1,
              conf.low = 1,
              conf.high = 1,
              .before = 1L
            ) |>
            complete(
              time = seq(0L, campaign_info$final_milestone_days, resolution), # fill in 1 row for each period (defined by resolution) of follow up
              fill = list(n.event = 0L, n.censor = 0L) # fill in zero events on those days
            ) |>
            fill(
              n.risk,
              .direction = "up"
            ) |>
            fill(
              estimate, conf.low, conf.high,
              .direction = "down"
            )
        }
      ),
    ) |>
    select(-data) |>
    unnest(surv_obj_tidy) |>
    mutate(

      # disclosure control
      n.risk = ceiling_any(n.risk, sdc_threshold),
      estimate = plyr::round_any(estimate, sdc_threshold / nth(n.risk, 2)), # use 2nd value as this skips the t=0 row where n.risk=0
      conf.low = plyr::round_any(conf.low, sdc_threshold / nth(n.risk, 2)),
      conf.high = plyr::round_any(conf.high, sdc_threshold / nth(n.risk, 2)),

      # cumulative incidence
      cmlinc = 1 - estimate,
      cmlinc.low = 1 - conf.high,
      cmlinc.high = 1 - conf.low,
    )


  # plot km curves locally for checking (but probs not for release as these can be reconstructed from released data)
  coverage_plot <-
    data_km |>
    mutate(
      lagtime = lag(time, 1, 0), # assumes the time-origin is zero
    ) |>
    ggplot() +
    geom_step(aes(x = time, y = cmlinc, group = .data[[group_name2]], colour = .data[[group_name2]]), direction = "vh") +
    # geom_step(aes(x = time, y = cmlinc, group = {{ subgroup }}, colour = {{ subgroup }}), direction = "vh", linetype = "dashed", alpha = 0.5) +
    geom_rect(aes(xmin = lagtime, xmax = time, ymin = cmlinc.low, ymax = cmlinc.high, group = .data[[group_name2]], fill = .data[[group_name2]]), alpha = 0.1, colour = "transparent") +
    facet_grid(rows = group_name1) +
    scale_color_brewer(type = "qual", palette = "Set1", na.value = "grey") +
    scale_fill_brewer(type = "qual", palette = "Set1", guide = "none", na.value = "grey") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.01))) +
    coord_cartesian(xlim = c(0, NA)) +
    labs(
      x = "Days since start of campaign",
      y = "Cumulative Incidence",
      colour = NULL,
      title = NULL
    ) +
    theme_minimal() +
    theme(
      axis.line.x = element_line(colour = "black"),
      panel.grid.minor.x = element_blank(),
      legend.position = "inside",
      legend.position.inside = c(.05, .95),
      legend.justification = c(0, 1),
    )

  ggsave(fs::path(output_dir, glue("km_{event_name}_{paste0(group_names, collapse='_')}.png")), plot = coverage_plot)

  # print(data_km)

  # write tables that capture underlying plotting data
  data_km_nozero <-
    data_km |>
    arrange(across(all_of(group_names))) |>
    ungroup() |>
    filter(time != 0) |>
    select(
      all_of(group_names),
      time,
      cmlinc,
      cmlinc.low,
      cmlinc.high,
    )

  # write_csv(data_km_nozero, fs::path(output_dir, glue("km_{event_name}_{paste0(group_names, collapse='_')}.csv")))

  return(data_km_nozero)

}

# create specific function for vax outcomes
km_estimates_vax <- partial(
  km_estimates,
  event_name = "vax", event_time = vax_time, event_indicator = vax_indicator, resolution = temporal_resolution_km
)

# for testing function interactively
# km_estimates_vax("all", "all")
# km_estimates_vax("all", "ageband4")
# km_estimates_vax("ageband4", "all")
# km_estimates_vax("ageband4", "sex")


# loop over all group1 and group2 variable combinations and combine into one big dataset
km_estimates_all <-
  level_combos |>
  mutate(
    km_summary = map2(
      group1, group2,
      .f = \(x, y) {

        if (is.na(y)) y <- NULL
        lookup <- c(group1_value = x, group2_value = y)

        km_estimates_vax(x, y) |>
          mutate(across(c(all_of(c(x, y))), as.character)) |>
          rename(all_of(c(group1_value = x, group2_value = y)))
      }
    )
  ) |>
  unnest(km_summary) |>
  select(group1, group1_value, group2, group2_value, everything()) |>
  mutate(
    early_milestone = (time == campaign_info$early_milestone_days) * 1L,
    primary_milestone = (time == campaign_info$primary_milestone_days) * 1L, # TODO this doesn't work as we're reporting weekly but we the primary milestone is not always the same day of the week - to fix
    final_milestone = (time == campaign_info$final_milestone_days) * 1L,
  )


# km_estimates_all |>
#   group_by(group1) |>
#   summarise(n = n())

# Write table to a CSV file
# split up by level1 grouping variables, so as not to exceed 5,000 row limit
iwalk(
  split(km_estimates_all, km_estimates_all$group1),
  ~ write_csv(.x, fs::path(output_dir, glue("km_estimates_table_{.y}.csv")))
)

# write_csv(km_estimates_all, fs::path(output_dir, glue("km_estimates_table.csv")))

# consider raw KM plots for disease burden too
# km_estimates(all, covid_admitted_time, covid_admitted_indicator, temporal_resolution_km)


# ________________________________________________________________________________________
# Post-snapshot Covid-19 disease burden, stratified by characteristics recorded on the snapshot_date ----
# ________________________________________________________________________________________

# Function to output HRs and IRRs for disease  burden comparing different subgroups
adjusted_estimates <- function(subgroup, event_name, event_time, event_indicator) {


  cox_formula <- as.formula(glue("Surv(event_time, event_indicator) ~ {subgroup} + sex + ns(age, 3)"))
  if (subgroup == "ageband4") cox_formula <- as.formula(glue("Surv(event_time, event_indicator) ~ ageband4 + sex"))
  if (subgroup == "ageband13") cox_formula <- as.formula(glue("Surv(event_time, event_indicator) ~ ageband13 + sex"))

  poisson_formula <- as.formula(glue("event_indicator ~ {subgroup} + sex + ns(age, 3)"))
  if (subgroup == "ageband4") poisson_formula <- as.formula(glue("event_indicator ~ ageband4 + sex"))
  if (subgroup == "ageband13") poisson_formula <- as.formula(glue("event_indicator ~ ageband13 + sex"))


  data_outcome <-
    data_combined |>
    mutate(
      event_time = {{ event_time }},
      event_indicator =  {{ event_indicator }}
    ) |>
    select(
      all_of(subgroup),
      sex, age,
      event_time,
      event_indicator
    )

  data_cox <-
    data_outcome |>
    coxph(
      data = _,
      formula = cox_formula,
      ties = "breslow"
    ) |>
    broom.helpers::tidy_plus_plus() |>
    filter(variable == subgroup) |>
    transmute(
      variable, label, reference_row,
      n_obs, n_event, exposure,
      hr = exp(estimate),
      hr.low = exp(conf.low),
      hr.high = exp(conf.high),
      hr.ln.std.error = std.error,
    )

  parglm_control <- parglm.control(maxit = 40, nthreads = 4)

  data_poisson <-
    data_outcome |>
    glm(
      data = _,
      formula = poisson_formula,
      family = poisson,
      offset = log(event_time),
      control = parglm_control
    ) |>
    broom.helpers::tidy_plus_plus() |>
    filter(variable == subgroup) |>
    transmute(
      variable, label, reference_row,
      irr = exp(estimate),
      irr.low = exp(conf.low),
      irr.high = exp(conf.high),
      irr.ln.std.error = std.error
    )

  data_estimates <-
    data_cox |>
    left_join(data_poisson, by = c("variable", "label", "reference_row"))

  # write_csv(data_cox, fs::path(output_dir, glue("cox_{event_name}_{subgroup}.csv")))
  return(data_estimates)
}

# adjusted_estimates("ageband4", "admitte", covid_admitted_time, covid_admitted_indicator)


get_all_estimates <- function(event_name, event_time, event_indicator) {
  # cox / glm function does not work when the contrast is a single valued vector
  # so creating the summary info manually here
  estimates_event_all <-
    data_combined |>
    summarise(
      variable = "all",
      label = NA_character_,
      reference_row = TRUE,
      n_obs = round_any(n(), sdc_threshold),
      n_event = round_any(sum({{ event_indicator }}), sdc_threshold),
      exposure = round_any(sum({{ event_time }}), sdc_threshold),
    )

  ## --VARIABLES--

  # Demographic
  estimates_event_sex <- adjusted_estimates("sex", event_name, {{ event_time }}, {{ event_indicator }})
  estimates_event_ageband4 <- adjusted_estimates("ageband4", event_name, {{ event_time }}, {{ event_indicator }})
  estimates_event_ethnicity5 <- adjusted_estimates("ethnicity5", event_name, {{ event_time }}, {{ event_indicator }})
  estimates_event_region <- adjusted_estimates("region", event_name, {{ event_time }}, {{ event_indicator }})
  estimates_event_imd_quintile <- adjusted_estimates("imd_quintile", event_name, {{ event_time }}, {{ event_indicator }})
  estimates_event_carehome_status <- adjusted_estimates("carehome_status", event_name, {{ event_time }}, {{ event_indicator }})

  # PRIMIS
  estimates_event_primis_atrisk <- adjusted_estimates("primis_atrisk", event_name, {{ event_time }}, {{ event_indicator }})

  estimates <-
    bind_rows(
      estimates_event_all,
      estimates_event_sex,
      estimates_event_ageband4,
      estimates_event_ethnicity5,
      estimates_event_imd_quintile,
      estimates_event_carehome_status,
      estimates_event_primis_atrisk
    ) |>
    mutate(
      ir = n_event / exposure,
      ir.ln.std.error = 1 / sqrt(n_event),
      ir.low = exp(log(ir) + qnorm(0.025) * ir.ln.std.error),
      ir.high = exp(log(ir) + qnorm(0.975) * ir.ln.std.error)
    )

  write_csv(estimates, fs::path(output_dir, glue("contrasts_{event_name}.csv")))

}

get_all_estimates("covid_admitted", covid_admitted_time, covid_admitted_indicator)
get_all_estimates("covid_critcare", covid_critcare_time, covid_critcare_indicator)
get_all_estimates("covid_death", covid_death_time, covid_death_indicator)
