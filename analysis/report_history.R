# _______________________________________________________________________________________
# Purpose:
# Report the distribution of vaccines schedules in different population subgroups
# _______________________________________________________________________________________

# Preliminaries ----

# Import libraries
library("tidyverse")
library("lubridate")
library("glue")
library("here")

# Import custom functions
source(here("analysis", "utility.R"))

## Create output directory
output_dir <- here("output", "report_history")
fs::dir_create(output_dir)

# Import processed data ----
data_fixed <- read_rds(here("output", "process", "data_fixed.rds"))
data_varying <- read_rds(here("output", "process", "data_vax.rds"))
data_varying_clean <- read_rds(here("output", "process", "data_vax_clean.rds"))

# _______________________________________________________________________________________
# Process datasets ----
# _______________________________________________________________________________________

## add time-invariant info from the fixed dataset to vaccine data and format variables for printing / plots ----

# only use first 8 vaccines in look up list
vax_shortname_8 <- vax_shortname_lookup[c(1:8)] # , length(vax_shortname_lookup))]
# vax_shortname_2 <- vax_shortname_lookup[c(1:2)]#, length(vax_shortname_lookup))]

data_vax <-
  left_join(
    data_varying,
    data_fixed %>% select(patient_id, sex, ethnicity5, ethnicity16, death_date),
    by = "patient_id"
  ) %>%
  mutate(
    vax_dosenumber = factor(vax_index, levels = sort(unique(vax_index)), labels = paste("Dose", sort(unique(vax_index)))),
    vax_week = floor_date(vax_date, unit = "week", week_start = 1),
    vax_type = fct_collapse(vax_type, !!!vax_shortname_8, other_level="Other"),
    all = ""
  )

data_vax_clean <-
  left_join(
    data_varying_clean,
    data_fixed %>% select(patient_id, sex, ethnicity5, ethnicity16, death_date),
    by = "patient_id"
  ) %>%
  mutate(
    vax_dosenumber = factor(vax_index, levels = sort(unique(vax_index)), labels = paste("Dose", sort(unique(vax_index)))),
    vax_week = floor_date(vax_date, unit = "week", week_start = 1),
    vax_type = fct_collapse(vax_type, !!!vax_shortname_8, other_level="Other"),
    all = "",
    all2 = ""
  )

# _______________________________________________________________________________________
# data validation checks ----
# _______________________________________________________________________________________

## output vax date validation info ----

summary_validation <-
  data_vax %>%
  summarise(
    n = ceiling_any(n(), 100),
    n_missing_date = ceiling_any(sum(is.na(vax_date)), 100),
    pct_missing_date = n_missing_date / n,
    n_earlier_than_start_date = ceiling_any(sum(vax_date < start_date, na.rm = TRUE)),
    pct_earlier_than_start_date = n_earlier_than_start_date / n,
    n_earlier_than_firstpossiblevax_date = ceiling_any(sum(vax_date < firstpossiblevax_date, na.rm = TRUE)),
    pct_earlier_than_firstpossiblevax_date = n_earlier_than_firstpossiblevax_date / n,
    n_interval_within_14days = sum(vax_interval < 14, na.rm = TRUE),
    pct_interval_within_14days = n_interval_within_14days / n,
  ) %>%
  ungroup()

write_csv(summary_validation, fs::path(output_dir, "validation.csv"))


## output vax date validation info, stratified by dose number and type ----

summary_validation_stratified <-
  data_vax %>%
  group_by(
    vax_index, vax_type
  ) %>%
  summarise(
    n = ceiling_any(n(), 100),
    n_missing_date = ceiling_any(sum(is.na(vax_date)), 100),
    pct_missing_date = n_missing_date / n,
    n_earlier_than_start_date = ceiling_any(sum(vax_date < start_date, na.rm = TRUE)),
    pct_earlier_than_start_date = n_earlier_than_start_date / n,
    n_earlier_than_firstpossiblevax_date = ceiling_any(sum(vax_date < firstpossiblevax_date, na.rm = TRUE)),
    pct_earlier_than_firstpossiblevax_date = n_earlier_than_firstpossiblevax_date / n,
    n_interval_within_14days = sum(vax_interval < 14, na.rm = TRUE),
    pct_interval_within_14days = n_interval_within_14days / n,
  ) %>%
  ungroup()

write_csv(summary_validation_stratified, fs::path(output_dir, "validation_stratified.csv"))


summary_vax_count <-
  data_vax %>%
  group_by(patient_id) %>%
  summarise(vax_count=n()) %>%
  group_by(vax_count) %>%
  summarise(frequency=ceiling_any(n(), 10))

write_csv(summary_vax_count, fs::path(output_dir, "validation_vax_count.csv"))

# _______________________________________________________________________________________
# Report info using characteristics recorded on each vaccination date ----
# _______________________________________________________________________________________

## note that all patient characteristics are determined as at the date of vaccination.
## for example, a person who moves from london to manchester between their first and second dose will be classed as in "london" for their first dose and "north west" for their second dose.
## except for fixed vairables extracted from the dataset_definition_fixed script

## output fully stratified vaccine counts ----
## this is useful for anyone wanting to externally re-construct different cuts of data for plotting etc

summary_stratified <-
  data_vax %>%
  group_by(
    vax_index, vax_type, vax_week,
    sex, ageband, ethnicity5, region, imd_quintile,
  ) %>%
  summarise(
    n = ceiling_any(n(), 100)
  ) %>%
  ungroup()

write_csv(summary_stratified, fs::path(output_dir, "vax_counts_stratified.csv"))



## output plots of vaccine counts per week ---
## stratified by type, dose number, and other characteristics

plot_vax_dates <- function(rows, cols) {
  summary_by <- data_vax_clean %>%
    mutate(
      "{{ rows }}" := fct_explicit_na({{ rows }}, na_level ="Unknown"),
      "{{ cols }}" := fct_explicit_na({{ cols }}, na_level ="Unknown"),
    ) %>%
    group_by(vax_type, vax_week) %>%
    group_by({{ rows }}, {{ cols }}, .add = TRUE) %>%
    summarise(
      n = roundmid_any(n(), 10)
    )

  temp_plot <-
    ggplot(summary_by) +
    geom_col(
      aes(x = vax_week, y = n, fill = vax_type, group = vax_type),
      alpha = 0.5,
      position = position_stack(reverse = TRUE),
      # position=position_identity(),
      width = 7
    ) +
    facet_grid(
      rows = vars({{ rows }}),
      cols = vars({{ cols }}),
      switch = "y",
      space = "free_x",
      scales = "free_x"
    ) +
    labs(
      x = "Date",
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
    # scale_fill_manual(values=c(RColorBrewer::brewer.pal(8, "Set2"), "grey50"))+
    scale_x_date(
      breaks = as.Date(c("2021-01-01", "2022-01-01", "2023-01-01", "2024-01-01")),
      date_minor_breaks = "month",
      date_labels = "%Y", # labels = scales::label_date("%b"),
      # sec.axis = sec_axis(
      #   breaks=as.Date(c("2021-01-01","2022-01-01","2023-01-01","2024-01-01")),
      #   trans = ~as.Date(.),
      #   labels = scales::label_date("%Y")
      # )
    ) +
    theme_minimal() +
    theme(
      axis.text.x.top = element_text(hjust = 0),
      axis.text.x.bottom = element_text(hjust = 0),
      strip.text.y.left = element_text(angle = 0, hjust = 1),
      strip.placement = "outside",
      axis.ticks.x = element_line(),
      legend.position = "bottom"
    )

  #print(temp_plot)

  row_name <- deparse(substitute(rows))
  col_name <- deparse(substitute(cols))

  ggsave(fs::path(output_dir, glue("vax_dates_{row_name}_{col_name}.png")), plot = temp_plot)
}



plot_vax_dates(all, all2)

plot_vax_dates(sex, all)
plot_vax_dates(ageband, all)
plot_vax_dates(ethnicity5, all)
plot_vax_dates(region, all)
plot_vax_dates(imd_quintile, all)
plot_vax_dates(vax_dosenumber, all)

plot_vax_dates(sex, vax_dosenumber)
plot_vax_dates(ageband, vax_dosenumber)
plot_vax_dates(ethnicity5, vax_dosenumber)
plot_vax_dates(region, vax_dosenumber)
plot_vax_dates(imd_quintile, vax_dosenumber)



## output plots of time since previous vaccination by type, dose number, and other characteristics ----

plot_vax_intervals <- function(rows, cols) {
  summary_by <- data_vax_clean %>%
    filter(vax_index != 1) %>%
    mutate(
      "{{ rows }}" := fct_explicit_na({{ rows }}, na_level ="Unknown"),
      "{{ cols }}" := fct_explicit_na({{ cols }}, na_level ="Unknown"),
      vax_interval = roundmid_any(vax_interval + 1, 7), # to split into 0-6, 7-13, 14-20, 21-28, ....
      vax_dosenumber = factor(vax_index, levels = sort(unique(vax_index)), labels = paste("Dose ", sort(unique(vax_index))-1, "-", sort(unique(vax_index)))),
    ) %>%
    group_by(vax_dosenumber, vax_type, vax_interval) %>%
    group_by({{ rows }}, {{ cols }}, .add = TRUE) %>%
    summarise(
      n = ceiling_any(n(), 10),
    )

  temp_plot <-
    ggplot(summary_by) +
    geom_col(
      aes(x = vax_interval, y = n, fill = vax_type, group = vax_type),
      alpha = 0.5,
      position = position_stack(reverse = TRUE),
      # position=position_identity(),
      width = 7
    ) +
    facet_grid(
      rows = vars({{ rows }}),
      cols = vars({{ cols }}),
      switch = "y",
      space = "free_x",
      scales = "free_x"
    ) +
    labs(
      x = "Interval (days)",
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
    scale_x_continuous(
      breaks = (0:100) * 4 * 7,
      # limits = c(0, NA),
      sec.axis = sec_axis(
        trans = ~ . / 7
      )
    ) +
    # scale_y_continuous(limits=c(0,100))+
    theme_minimal() +
    theme(
      strip.text.y.left = element_text(angle = 0, hjust = 1),
      strip.placement = "outside",
      axis.ticks.x = element_line(),
      # axis.text.y = element_blank(),
      legend.position = "bottom"
    )

  print(temp_plot)

  row_name <- deparse(substitute(rows))
  col_name <- deparse(substitute(cols))

  ggsave(fs::path(output_dir, glue("vax_intervals_{row_name}_{col_name}.png")), plot = temp_plot)
}

plot_vax_intervals(sex, vax_dosenumber)
plot_vax_intervals(ageband, vax_dosenumber)
plot_vax_intervals(ethnicity5, vax_dosenumber)
plot_vax_intervals(region, vax_dosenumber)
plot_vax_intervals(imd_quintile, vax_dosenumber)

plot_vax_intervals(vax_dosenumber, all)

