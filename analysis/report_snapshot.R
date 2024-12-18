# _______________________________________________________________________________________
# Purpose:
# Report the distribution of vaccines schedules in different population subgroups as at a given date
# _______________________________________________________________________________________


# Preliminaries ----

# Import libraries
library("tidyverse")
library("lubridate")
library("glue")
library("here")
library("arrow")

# Import custom functions
source(here("analysis", "utility.R"))

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
output_dir <- here("output", glue("report_snapshot_{snapshot_date_compact}"))
fs::dir_create(output_dir)


stopifnot("snapshot date is greater than observation end date - extend end date to pick up all vaccinations prior to snapshot date" = snapshot_date <= end_date)


# Import processed data ----
data_snapshot <- read_feather(here("output", "extracts", glue("extract_snapshot_{snapshot_date_compact}.arrow")))
data_vax <- read_rds(here("output", "process", "data_vax.rds"))
data_vax_clean <- read_rds(here("output", "process", "data_vax_clean.rds"))
data_fixed <- read_rds(here("output", "process", "data_fixed.rds"))

# select most recent vaccine _before_ snapshot date, and summarise
data_last_vax_date_clean <-
  data_vax_clean %>%
  filter(vax_date < snapshot_date) %>%
  group_by(patient_id) %>%
  filter(vax_index == max(vax_index)) %>%
  transmute(
    patient_id,
    vax_count = vax_index,
    last_vax_date = vax_date,
    last_vax_type = vax_type,
    days_since_vax = snapshot_date - last_vax_date
  )

# check there's only one patient per row:
check_1rpp <-
  data_last_vax_date_clean %>%
  group_by(patient_id) %>%
  filter(row_number() != 1)
stopifnot("data_last_vax_date_clean should not have multiple rows per patient" = nrow(check_1rpp) == 0)

# merge fixed data and vaccine data onto snapshot data
# note that in dummy data this doesn't work very well because patient IDs might not be matched across all datasets
data_snapshot <-
  data_snapshot %>%
  left_join(
    data_fixed %>% select(patient_id, sex, ethnicity5, ethnicity16),
    by = "patient_id"
  ) %>%
  mutate(
    !!!standardise_characteristics
  ) %>%
  left_join(
    data_last_vax_date_clean,
    by = "patient_id"
  ) %>%
  mutate(
    # impute values for people with no previous vaccination
    vax_count = replace_na(vax_count, 0L),
    last_vax_type = fct_explicit_na(last_vax_type, "unvaccinated"),
    last_vax_date = if_else(vax_count == 0, default_date + as.integer(runif(n(), 0, 10)), last_vax_date),
    last_vax_week = floor_date(last_vax_date, unit = "week", week_start = 1), # starting on a monday
    last_vax_period = floor_dates[findInterval(last_vax_date, floor_dates)], # use floor_date(last_vax_date, unit = floor_dates) when lubridate package is updated 
    all = ""
  ) %>%
  mutate(
    across(where(is.factor) | where(is.character), ~fct_explicit_na(.x, na_level ="Unknown"))
  )


# _______________________________________________________________________________________
# Report vaccination info, stratifying by characteristics recorded on the "snapshot_date" ----
# _______________________________________________________________________________________

## output plots of date of last dose by type and other characteristics ----

plot_date_of_last_dose <- function(rows) {
  summary_by <- data_snapshot %>%
    group_by({{ rows }}, last_vax_type, last_vax_period) %>%
    summarise(
      n = ceiling_any(n(), 100)
    ) %>%
    ungroup() %>%
    complete(
      {{ rows }}, last_vax_type, last_vax_period,
      fill = list(n = 0)
    )

  temp_plot <-
    ggplot(summary_by) +
    geom_col(
      aes(x = last_vax_period, y = n, fill = last_vax_type, group = last_vax_type),
      alpha = 0.5,
      position = position_stack(reverse = TRUE),
      # position=position_identity(),
      width = temporal_resolution
    ) +
    facet_grid(
      rows = vars({{ rows }}),
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

  row_name <- deparse(substitute(rows))
  # col_name = deparse(substitute(cols))

  ggsave(fs::path(output_dir, glue("last_vax_date_{row_name}.png")), plot = temp_plot)

 # write tables that capture underlying plotting data
  write_csv(summary_by, fs::path(output_dir, glue("last_vax_date_{row_name}.csv")))
}


## --VARIABLES--
plot_date_of_last_dose(all)
plot_date_of_last_dose(sex)
plot_date_of_last_dose(ageband)
plot_date_of_last_dose(ethnicity5)
plot_date_of_last_dose(region)
plot_date_of_last_dose(imd_quintile)
#PRIMIS
plot_date_of_last_dose(crd) #cronic respiratory disease
plot_date_of_last_dose(chd) #cronic heart disease
plot_date_of_last_dose(ckd) #chronic kidney disease
plot_date_of_last_dose(cld) # cronic liver disease
plot_date_of_last_dose(cns) 
plot_date_of_last_dose(learndis) # Wider Learning Disability
plot_date_of_last_dose(diabetes) #diabetes
plot_date_of_last_dose(immunosuppressed) #immunosuppress grouped
plot_date_of_last_dose(asplenia) # Asplenia or Dysfunction of the Spleen codes
plot_date_of_last_dose(severe_obesity) #immunosuppress grouped
plot_date_of_last_dose(smi) #severe mental illness
plot_date_of_last_dose(primis_atrisk) # clinically vulnerable 
## output plots of dose count by type and other characteristics ----

plot_vax_count <- function(rows) {
  summary_by <- data_snapshot %>%
    group_by({{ rows }}, vax_count) %>%
    summarise(
      n = ceiling_any(n(), 100),
    ) %>%
    ungroup() %>%
    complete(
      {{ rows }}, vax_count,
      fill = list(n = 0)
    ) %>%
    ungroup() %>%
    group_by({{ rows }}) %>%
    mutate(
      row_total = sum(n),
      prop = n / row_total,
    )

  temp_plot <-
    ggplot(summary_by) +
    geom_bar(
      aes(x = prop, y = {{ rows }}, width = row_total, fill = as.character(vax_count)),
      stat = "identity", position = "fill"
      # position = position_stack(reverse = TRUE),
    ) +
    facet_grid(
      rows = vars({{ rows }}),
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

  row_name <- deparse(substitute(rows))
  # col_name = deparse(substitute(cols))

  ggsave(fs::path(output_dir, glue("vax_count_{row_name}.png")), plot = temp_plot)
 
  # write tables that capture underlying plotting data
  write_csv(summary_by, fs::path(output_dir, glue("vax_count_{row_name}.csv")))
}

## --VARIABLES--
plot_vax_count(all)
plot_vax_count(sex)
plot_vax_count(ageband)
plot_vax_count(ethnicity5)
plot_vax_count(region)
plot_vax_count(imd_quintile)

#PRIMIS
plot_vax_count(crd) #cronic respiratory disease
plot_vax_count(chd) #cronic heart disease
plot_vax_count(ckd) #chronic kidney disease
plot_vax_count(cld) # cronic liver disease
plot_vax_count(cns) 
plot_vax_count(learndis) # Wider Learning Disability
plot_vax_count(diabetes) #diabetes
plot_vax_count(immunosuppressed) #immunosuppress grouped
plot_vax_count(asplenia) # Asplenia or Dysfunction of the Spleen codes
plot_vax_count(severe_obesity) #immunosuppress grouped
plot_vax_count(smi) #severe mental illness
plot_vax_count(primis_atrisk) # clinically vulnerable 

#Table
create_summary_table <- function(rows) {
  summary_table <- data_snapshot %>%
    mutate(
      # Calculate months since last dose
      months_since_last_dose = round(as.numeric(days_since_vax/30.4), 1)
    ) %>%
    group_by({{ rows }}) %>%
    summarise(
      # Dose counts
      Total = n(),
      `0` = sum(vax_count == 0, na.rm = TRUE),
      `1` = sum(vax_count == 1, na.rm = TRUE),
      `2` = sum(vax_count == 2, na.rm = TRUE),
      `3` = sum(vax_count == 3, na.rm = TRUE),
      `4` = sum(vax_count == 4, na.rm = TRUE),
      `5+` = sum(vax_count >= 5, na.rm = TRUE),
      # Dose percentages
      `0_per` = round(sum(vax_count == 0, na.rm = TRUE) / n() * 100, 1),
      `1_per` = round(sum(vax_count == 1, na.rm = TRUE) / n() * 100, 1),
      `2_per` = round(sum(vax_count == 2, na.rm = TRUE) / n() * 100, 1),
      `3_per` = round(sum(vax_count == 3, na.rm = TRUE) / n() * 100, 1),
      `4_per` = round(sum(vax_count == 4, na.rm = TRUE) / n() * 100, 1),
      `5+_per` = round(sum(vax_count >= 5, na.rm = TRUE) / n() * 100, 1),
      # Dose summary
      Dose_median = median(vax_count, na.rm = TRUE),
      Dose_25 = quantile(vax_count, probs = 0.25, na.rm = TRUE), 
      Dose_75 = quantile(vax_count, probs = 0.75, na.rm = TRUE),
      # Vaccination in past 12 and 24 months
      Vacc_12m_n = sum(months_since_last_dose <= 12, na.rm = TRUE),
      Vacc_12m_perc = round(Vacc_12m_n / n() * 100, 1),
      Vacc_24m_n = sum(months_since_last_dose <= 24, na.rm = TRUE),
      Vacc_24m_perc = round(Vacc_24m_n / n() * 100, 1),
      # Time since last dose
      Time_last_dose_median = round(median(months_since_last_dose, na.rm = TRUE), 1),
      Time_last_dose_25 = round(quantile(months_since_last_dose, probs = 0.25, na.rm = TRUE), 1),
      Time_last_dose_75 = round(quantile(months_since_last_dose, probs = 0.75, na.rm = TRUE), 1)
    ) %>%
    ungroup()
  row_name <- deparse(substitute(rows))
  # Write table to a CSV file
  write_csv(summary_table, fs::path(output_dir, glue("summary_table_{row_name}.csv")))
  print(summary_table)
}

## --VARIABLES--
create_summary_table(all)
create_summary_table(sex)
create_summary_table(ageband)
create_summary_table(ethnicity5)
create_summary_table(region)
create_summary_table(imd_quintile)

#PRIMIS
create_summary_table(crd) #cronic respiratory disease
create_summary_table(chd) #cronic heart disease
create_summary_table(ckd) #chronic kidney disease
create_summary_table(cld) # cronic liver disease
create_summary_table(cns) 
create_summary_table(learndis) # Wider Learning Disability
create_summary_table(diabetes) #diabetes
create_summary_table(immunosuppressed) #immunosuppress grouped
create_summary_table(asplenia) # Asplenia or Dysfunction of the Spleen codes
create_summary_table(severe_obesity) #immunosuppress grouped
create_summary_table(smi) #severe mental illness
create_summary_table(primis_atrisk) # clinically vulnerable 
