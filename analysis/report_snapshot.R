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
  snapshot_date <- as.Date("2023-09-30", format = "%Y%m%d")
} else {
  # removeobjects <- TRUE
  snapshot_date <- as.Date(args[[1]], format = "%Y%m%d")
}

# create string representation of date in compact format YYYMMDD
snapshot_date_compact <- format(snapshot_date, "%Y%m%d")

# use this to indicate a "last vaccination date" for unvaccinated people
default_date <- firstpossiblevax_date

## Create output directory
output_dir <- here("output", glue("report_snapshot_{snapshot_date_compact}"))
fs::dir_create(output_dir)


stopifnot("snapshot date is greater than observationa end date - extend end date to pick up all vaccinations prior to snapshot date" = snapshot_date <= end_date)


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


data_snapshot <-
  data_snapshot %>%
  left_join(
    data_fixed %>% select(patient_id, sex, ethnicity5, ethnicity16),
    by = "patient_id"
  ) %>%
  mutate(!!!standardise_characteristics) %>%
  left_join(
    data_last_vax_date_clean,
    by = "patient_id"
  ) %>%
  mutate(
    # impute values for people with no previous vaccination
    vax_count = replace_na(vax_count, 0L),
    last_vax_type = fct_explicit_na(last_vax_type, "unvaccinated"),
    last_vax_date = if_else(vax_count == 0, default_date + as.integer(runif(n(), 0, 10)), last_vax_date),
    last_vax_week = floor_date(last_vax_date, unit = "week", week_start = 1),
    all = ""
  )


# _______________________________________________________________________________________
# Report vaccination info, stratifying by characteristics recorded on the "snapshot_date" ----
# _______________________________________________________________________________________


## output plots of date of last dose by type and other characteristics ----

plot_date_of_last_dose <- function(rows) {
  summary_by <- data_snapshot %>%
    mutate(
      "{{ rows }}" := fct_explicit_na({{ rows }}, na_level ="Unknown"),
    ) %>%
    group_by(last_vax_type, last_vax_week) %>%
    group_by({{ rows }}, .add = TRUE) %>%
    summarise(
      n = ceiling_any(n(), 100)
    )

  temp_plot <-
    ggplot(summary_by) +
    geom_col(
      aes(x = last_vax_week, y = n, fill = last_vax_type, group = last_vax_type),
      alpha = 0.5,
      position = position_stack(reverse = TRUE),
      # position=position_identity(),
      width = 7
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
}


## --VARIABLES--
plot_date_of_last_dose(all)
plot_date_of_last_dose(sex)
plot_date_of_last_dose(ageband)
plot_date_of_last_dose(ethnicity5)
plot_date_of_last_dose(region)
plot_date_of_last_dose(imd_quintile)





## output plots of dose count by type and other characteristics ----

plot_vax_count <- function(rows) {
  summary_by <- data_snapshot %>%
    mutate(
      "{{ rows }}" := fct_explicit_na({{ rows }}, na_level ="Unknown"),
    ) %>%
    group_by(vax_count, {{ rows }}) %>%
    summarise(
      n = ceiling_any(n(), 100),
    ) %>%
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
}

## --VARIABLES--
plot_vax_count(all)
plot_vax_count(sex)
plot_vax_count(ageband)
plot_vax_count(ethnicity5)
plot_vax_count(region)
plot_vax_count(imd_quintile)


