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
# use fix width, rather than months or bi-months or quarters, so that temporal denominator is even and ratio of weekends to weekdays is fixed
temporal_resolution <- 28

# maximum follow-up after snapshot date
max_fup <- 24*7 # 24 weeks

#Create next campaign start date
next_campaign_date <- min(campaign_dates$start[campaign_dates$start > snapshot_date], as.Date(Inf), na.rm=TRUE)

# dates to round down to
# use this with `findInterval` until lubridate package is updated in the opensafely R image
# (then use `floor_date(date, unit=floor_dates`)
floor_dates <- seq(
  as.Date("2020-06-01"), # monday
  as.Date("2029-12-31"),  # to monday!
  by = temporal_resolution
)

# create string representation of date in compact format YYYMMDD
snapshot_date_compact <- format(snapshot_date, "%Y%m%d")

# use this to indicate a "last vaccination date" for unvaccinated people
default_date <- firstpossiblevax_date

## Create output directory
output_dir <- here("output", "4-snapshot", glue("report_snapshot_{snapshot_date_compact}"))
fs::dir_create(output_dir)
options(width=200) # set output width for capture.output


stopifnot("snapshot date is greater than observation end date - extend end date to pick up all vaccinations prior to snapshot date" = snapshot_date <= end_date)


# Import processed data ----
data_snapshot <- read_feather(here("output", "1-extract", glue("extract_snapshot_{snapshot_date_compact}.arrow")))
#data_vax <- read_feather(here("output", "2-prepare", "prepare", "data_vax.arrow"))
#data_vax_clean <- read_feather(here("output", "2-prepare", "prepare", "data_vax_clean.arrow"))
data_fixed <- read_feather(here("output", "2-prepare", "prepare", "data_fixed.arrow"))

capture.output(
  skimr::skim_without_charts(data_snapshot),
  file = fs::path(output_dir, "data_snapshot_skim.txt"),
  split = FALSE
)

# select most recent vaccine _before_ snapshot date, and summarise
# ignore this for now as we're extracting most recent 3 vaccines and next 2 vaccines in the snapshot extract itself
# data_last_vax_date_clean <-
#   data_vax_clean |>
#   lazy_dt() |>
#   filter(vax_date < snapshot_date ) |>
#   group_by(patient_id) |>
#   filter(vax_index == max(vax_index, na.rm=TRUE)) |>
#   ungroup() |>
#   transmute(
#     patient_id,
#     vax_count = vax_index,
#     last_vax_date = vax_date,
#     last_vax_product = vax_product,
#     days_since_vax = snapshot_date - last_vax_date,
#   )
#
# rm(data_vax_clean)
#
# check there's only one patient per row:
# check_last_1rpp <-
#   data_last_vax_date_clean %>%
#   group_by(patient_id) %>%
#   filter(row_number() != 1) %>%
#   as_tibble()
# stopifnot("data_last_vax_date_clean should not have multiple rows per patient" = nrow(check_last_1rpp) == 0)


# select earliest vaccine _after_ snapshot date and before next campaign, and summarise
# data_next_vax_date_clean <-
#   data_vax_clean |>
#   lazy_dt() |>
#   group_by(patient_id) |>
#   filter(vax_date >= snapshot_date &  vax_date < next_campaign_date) %>%
#   group_by(patient_id) |>
#   filter(vax_index == min(vax_index, na.rm=TRUE)) %>%
#   ungroup() |>
#   transmute(
#     patient_id,
#     vax_date,s
#     vax_product,
#   )

# check there's only one patient per row:
# check_next_1rpp <-
#   data_next_vax_date_clean %>%
#   group_by(patient_id) %>%
#   filter(row_number() != 1) %>%
#   as_tibble()
# stopifnot("data_next_vax_date_clean should not have multiple rows per patient" = nrow(check_next_1rpp) == 0)
#
# rm(data_vax_clean)


# merge fixed data and vaccine data onto snapshot data
# note that in dummy data this doesn't work very well because patient IDs might not be matched across all datasets
data_combined <-
  data_snapshot |>
  lazy_dt() |>
  left_join(
    lazy_dt(data_fixed) |> select(patient_id, sex, ethnicity5, ethnicity16, death_date),
    by = "patient_id"
  ) |>
  mutate(
    all = "",
    !!!standardise_characteristics,

    # previous vaccine summary
    # add more variables here based on covid_vax_prior_1_date, covid_vax_prior_2_date,... etc if needed
    vax_count = covid_vax_prior_count,
    vax_count_group = ,
    last_vax_date = covid_vax_prior_1_date,
    last_vax_product = covid_vax_prior_1_product,
    days_since_vax = snapshot_date - last_vax_date,

    last_vax_product = fct_explicit_na(last_vax_product, "unvaccinated"),
    last_vax_date = if_else(vax_count == 0, default_date + as.integer(runif(n(), 0, 10)), last_vax_date),
    last_vax_week = floor_date(last_vax_date, unit = "week", week_start = 1), # starting on a monday
    last_vax_period = floor_dates[findInterval(last_vax_date, floor_dates)], # use floor_date(last_vax_date, unit = floor_dates) when lubridate package is updated

    # info on first vaccine(s) received after snapshot date
    next_vax_date = covid_vax_1_date,
    next_vax_product = covid_vax_1_product,
    next2_vax_date = covid_vax_2_date,

    censor_date = pmin(
      death_date,
      deregistered_date,
      next_campaign_date - 1,
      snapshot_date + max_fup,
      na.rm=TRUE
    ),
    # time from snapshot date until next vaccination
    event_time = as.integer(pmin(next_vax_date, censor_date, na.rm=TRUE) - snapshot_date) + 1L, # +1 because vaccination on snapshot date is allowed, but events at time zero are not
    event_indicator = (!is.na(next_vax_date)) & (next_vax_date <= censor_date)
   ) |>
  as_tibble() |>
  mutate(
   across(where(is.factor) | where(is.character), ~fct_na_value_to_level(.x, level ="Unknown"))
  )


# ________________________________________________________________________________________
# Pre-snapshot date vaccine history, stratified by characteristic recorded on the snapshot_date ----
# ________________________________________________________________________________________


## _______________________________________________________________________________________
## Report info on date of last vaccination
## _______________________________________________________________________________________


plot_date_of_last_dose <- function(subgroup) {
  summary_by <-
    data_combined |>
    lazy_dt() |>
    group_by({{ subgroup }}, last_vax_product, last_vax_period) |>
    summarise(
      n = ceiling_any(n(), sdc_threshold)
    ) |>
    ungroup() |>
    as_tibble() |>
    complete(
      {{ subgroup }}, last_vax_product, last_vax_period,
      fill = list(n = 0)
    )


  temp_plot <-
    ggplot(summary_by) +
    geom_col(
      aes(x = last_vax_period, y = n, fill = last_vax_product, group = last_vax_product),
      alpha = 0.5,
      position = position_stack(reverse = TRUE),
      # position=position_identity(),
      width = temporal_resolution
    ) +
    facet_grid(
      rows = vars({{ subgroup }}),
      # cols=vars({{cols}}),
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
      breaks = c(default_date - 30, as.Date(c("2021-01-01", "2022-01-01", "2023-01-01", "2024-01-01"))),
      date_minor_breaks = "month",
      # labels = ~{c("Unvaccinated", scales::label_date("%Y")(.x[-1]))},
      labels = c("Unvaccinated", scales::label_date("%Y")(as.Date(c("2021-01-01", "2022-01-01", "2023-01-01", "2024-01-01")))),
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

  print(temp_plot)

  subgroup_name <- deparse(substitute(subgroup))
  # col_name = deparse(substitute(cols))

  ggsave(fs::path(output_dir, glue("last_vax_date_{subgroup_name}.png")), plot = temp_plot)

 # write tables that capture underlying plotting data
  write_csv(summary_by, fs::path(output_dir, glue("last_vax_date_{subgroup_name}.csv")))
}


## --VARIABLES--
plot_date_of_last_dose(all)
plot_date_of_last_dose(sex)
plot_date_of_last_dose(ageband)
plot_date_of_last_dose(ethnicity5)
plot_date_of_last_dose(region)
plot_date_of_last_dose(imd_quintile)
#PRIMIS
plot_date_of_last_dose(crd) # cronic respiratory disease
plot_date_of_last_dose(chd) # cronic heart disease
plot_date_of_last_dose(ckd) # chronic kidney disease
plot_date_of_last_dose(cld) # cronic liver disease
plot_date_of_last_dose(cns) # chronic neurological
plot_date_of_last_dose(learndis) # learning disability
plot_date_of_last_dose(diabetes) # diabetes
plot_date_of_last_dose(immunosuppressed) # immunosuppressed
plot_date_of_last_dose(asplenia) # asplenia or dysfunction of the spleen
plot_date_of_last_dose(severe_obesity) # obesity
plot_date_of_last_dose(smi) # severe mental illness
plot_date_of_last_dose(primis_atrisk) # clinically vulnerable


## _______________________________________________________________________________________
## Report info on prior dose count and product type
## _______________________________________________________________________________________

plot_vax_count <- function(subgroup) {
  summary_by <-
    data_combined |>
    lazy_dt() |>
    group_by({{ subgroup }}, vax_count) |>
    summarise(
      n = ceiling_any(n(), sdc_threshold),
    ) |>
    ungroup() |>
    as_tibble() |>
    complete(
      {{ subgroup }}, vax_count,
      fill = list(n = 0)
    ) |>
    ungroup() |>
    group_by({{ subgroup }}) |>
    mutate(
      row_total = sum(n),
      prop = n / row_total,
    )

  temp_plot <-
    ggplot(summary_by) +
    geom_bar(
      aes(x = prop, y = {{ subgroup }}, width = row_total, fill = as.character(vax_count)),
      stat = "identity", position = "fill"
      # position = position_stack(reverse = TRUE),
    ) +
    facet_grid(
      rows = vars({{ subgroup }}),
      scales = "free_y",
      space = "free_y"
    ) +
    labs(
      x = "%",
      y = NULL,
      fill = "Vaccine count"
    ) +
    scale_fill_brewer(
      palette = "Set2",
      na.value = "grey50",
      labels = function(breaks) {
        breaks[is.na(breaks)] <- "Other"
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

  print(temp_plot)

  subgroup_name <- deparse(substitute(subgroup))
  # col_name = deparse(substitute(cols))

  ggsave(fs::path(output_dir, glue("vax_count_{subgroup_name}.png")), plot = temp_plot)

  # write tables that capture underlying plotting data
  write_csv(summary_by, fs::path(output_dir, glue("vax_count_{subgroup_name}.csv")))
}

## --VARIABLES--
plot_vax_count(all)
plot_vax_count(sex)
plot_vax_count(ageband)
plot_vax_count(ethnicity5)
plot_vax_count(region)
plot_vax_count(imd_quintile)

#PRIMIS
plot_vax_count(crd) # chronic respiratory disease
plot_vax_count(chd) # chronic heart disease
plot_vax_count(ckd) # chronic kidney disease
plot_vax_count(cld) # chronic liver disease
plot_vax_count(cns) # chronic neurological
plot_vax_count(learndis) # learning disability
plot_vax_count(diabetes) # diabetes
plot_vax_count(immunosuppressed) # immunosuppressed
plot_vax_count(asplenia) # asplenia or dysfunction of the spleen
plot_vax_count(severe_obesity) # obesity
plot_vax_count(smi) # severe mental illness
plot_vax_count(primis_atrisk) # clinically vulnerable


## _______________________________________________________________________________________
## Report info in a standardised table
## _______________________________________________________________________________________

#Table
create_summary_table <- function(subgroup) {
  summary_table <-
    data_combined |>
    lazy_dt() |>
    group_by({{ subgroup }}) |>
    summarise(
      # Dose counts
      total = ceiling_any(n(), sdc_threshold),
      `0` = ceiling_any(sum(vax_count == 0, na.rm = TRUE), sdc_threshold),
      `1` = ceiling_any(sum(vax_count == 1, na.rm = TRUE), sdc_threshold),
      `2` = ceiling_any(sum(vax_count == 2, na.rm = TRUE), sdc_threshold),
      `3` = ceiling_any(sum(vax_count == 3, na.rm = TRUE), sdc_threshold),
      `4` = ceiling_any(sum(vax_count == 4, na.rm = TRUE), sdc_threshold),
      `5+` = ceiling_any(sum(vax_count >= 5, na.rm = TRUE), sdc_threshold),
      # Dose summary
      Dose_median = quantile(vax_count, probs = 0.5, na.rm = TRUE),
      Dose_25 = quantile(vax_count, probs = 0.25, na.rm = TRUE),
      Dose_75 = quantile(vax_count, probs = 0.75, na.rm = TRUE),
      # Vaccination in past 12 and 24 months
      Vacc_12m_n = ceiling_any(sum(days_since_vax <= 365, na.rm = TRUE), sdc_threshold),
      Vacc_24m_n = ceiling_any(sum(days_since_vax <= 365*2, na.rm = TRUE), sdc_threshold),
      # Time since last dose
      Time_last_dose_median = quantile(days_since_vax, probs = 0.5, na.rm = TRUE),
      Time_last_dose_10 = quantile(days_since_vax, probs = 0.10, na.rm = TRUE),
      Time_last_dose_25 = quantile(days_since_vax, probs = 0.25, na.rm = TRUE),
      Time_last_dose_75 = quantile(days_since_vax, probs = 0.75, na.rm = TRUE),
      Time_last_dose_90 = quantile(days_since_vax, probs = 0.90, na.rm = TRUE),
    ) |>
    ungroup() |>
    mutate(
      # Dose percentages - put this here and not in earlier summarise step so that it works with dtplyr
      `0_per` = round(`0`*100 / total, 1),
      `1_per` = round(`1`*100 / total, 1),
      `2_per` = round(`2`*100 / total, 1),
      `3_per` = round(`3`*100 / total, 1),
      `4_per` = round(`4`*100 / total, 1),
      `5+_per` = round(`5+`*100 / total, 1),
      # Vaccination % in past 12 and 24 months
      Vacc_12m_per = round(Vacc_12m_n * 100 / total, 1),
      Vacc_24m_per = round(Vacc_24m_n * 100 / total, 1),
    ) |>
    as_tibble()
  subgroup_name <- deparse(substitute(subgroup))
  # Write table to a CSV file
  write_csv(summary_table, fs::path(output_dir, glue("summary_table_{subgroup_name}.csv")))
  #print(summary_table)
}

## --VARIABLES--
create_summary_table(all)
create_summary_table(sex)
create_summary_table(ageband)
create_summary_table(ethnicity5)
create_summary_table(region)
create_summary_table(imd_quintile)

#PRIMIS
create_summary_table(crd) # cronic respiratory disease
create_summary_table(chd) # cronic heart disease
create_summary_table(ckd) # chronic kidney disease
create_summary_table(cld) # cronic liver disease
create_summary_table(cns) # chronic neurological
create_summary_table(learndis) # learning disability
create_summary_table(diabetes) # diabetes
create_summary_table(immunosuppressed) # immunosuppressed
create_summary_table(asplenia) # asplenia or dysfunction of the spleen
create_summary_table(severe_obesity) # obesity
create_summary_table(smi) # severe mental illness
create_summary_table(primis_atrisk) # clinically vulnerable



# ________________________________________________________________________________________
# Post-snapshot vaccine uptake, stratified by characteristic recorded on the snapshot_date ----
# ________________________________________________________________________________________

# This code borrows heavily from the KM reusable action https://github.com/opensafely-actions/kaplan-meier-function/blob/main/analysis/km.R
# The reason not to use the KM reusable action directly is that we need to reuse it multiple times across many different stratification variables
# This would create a large number of project.yaml actions, so it's easier to do the repeats within a single script
# The reusable action could be modified to enable multiple stratified analyses to be run, but it's a bit faffy and a bit scope-creepy


## tests ----

times_count <- table(cut(data_combined$event_time, c(-Inf, 0, 1, Inf), right=FALSE, labels= c("<0", "0", ">0")), useNA="ifany")

if(!identical(as.integer(times_count), c(0L, 0L, nrow(data_combined)))) {
  print(times_count)
  stop("all event times must be strictly positive")
}

## Function to calculate KM estimates for a given stratification variable ----

km_estimates <- function(subgroup){

  data_surv <-
    data_combined |>
    group_by({{ subgroup }}) |>
    nest() |>
    mutate(
      surv_obj_tidy = map(data, ~ {
        survfit(
          Surv(event_time, event_indicator) ~ 1,
          data = .x,
          conf.type="log-log"
        ) |>
        broom::tidy() |>
        complete(
          time = seq_len(max_fup), # fill in 1 row for each day of follow up
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
      }),
    ) |>
    select(-data) |>
    unnest(surv_obj_tidy) |>
    mutate(

      # disclosure control
      estimate = plyr::round_any(estimate, sdc_threshold/first(n.risk)),
      conf.high = plyr::round_any(conf.high, sdc_threshold/first(n.risk)),
      conf.low = plyr::round_any(conf.low, sdc_threshold/first(n.risk)),

      # cumulative incidence
      cmlinc = 1-estimate,
      cmlinc.low = 1-conf.high,
      cmlinc.high = 1-conf.low,
    )

  data_with_time0 <-
    data_surv |>
    mutate(
      lagtime = lag(time, 1, 0), # assumes the time-origin is zero
    ) |>
    group_modify(
      ~ add_row(
        .x,
        time = 0, # assumes time origin is zero
        lagtime = 0,
        cmlinc = 0,
        cmlinc.low = 0,
        cmlinc.high = 0,
        .before = 0
      )
    )

  coverage_plot <-
    ggplot(data_with_time0) +
    geom_step(aes(x = time, y = cmlinc, group={{ subgroup }}, colour = {{ subgroup }}), direction = "vh") +
    geom_step(aes(x = time, y = cmlinc, group={{ subgroup }}, colour = {{ subgroup }}), direction = "vh", linetype = "dashed", alpha = 0.5) +
    geom_rect(aes(xmin = lagtime, xmax = time, ymin = cmlinc.low, ymax = cmlinc.high, group={{ subgroup }}, colour = {{ subgroup }}, fill = {{ subgroup }}), alpha = 0.1, colour = "transparent") +
    #facet_grid(rows = vars(!!!subgroup_syms)) +
    scale_color_brewer(type = "qual", palette = "Set1", na.value = "grey") +
    scale_fill_brewer(type = "qual", palette = "Set1", guide = "none", na.value = "grey") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.01))) +
    coord_cartesian(xlim = c(0, NA)) +
    labs(
      x = "Time",
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

  subgroup_name <- deparse(substitute(subgroup))

  ggsave(fs::path(output_dir, glue("km_{subgroup_name}.png")), plot = coverage_plot)

  # write tables that capture underlying plotting data
  data_surv |>
    select(
      {{ subgroup }},
      time,
      cmlinc,
      cmlinc.low,
      cmlinc.high,
    ) |>
    write_csv(fs::path(output_dir, glue("km_{subgroup_name}.csv")))
}

km_estimates(all)
km_estimates(sex)
km_estimates(ageband)
km_estimates(ethnicity5)
km_estimates(region)
km_estimates(imd_quintile)
km_estimates(primis_atrisk)
