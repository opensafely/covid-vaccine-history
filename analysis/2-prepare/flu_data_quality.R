# _________________________________________________
# purpose:
# import event-level flu vaccination data extracted by ehrql from three sources
# (vaccination table, clinical records, and medicines records)
# and report data quality across sources
#
# outputs (rounded):
# - table 1. flu vaccinations by campaign and data source combination:
#   - table1_flu_sources.csv
# - dataset used to produce upset plots by campaign:
#   - upset_plot_data.csv
# - table 2. agreement in vaccination date across sources:
#   - table_date_agreement.csv
#________________________________________________

# Preliminaries ----

# Import libraries
library("tidyverse")
library("dtplyr")
library("lubridate")
library("arrow")
library("here")
library("glue")
library("UpSetR")

# Import custom functions
source(here("analysis", "0-lib", "design.R"))

# create output directory
output_dir <- here("output", "2-prepare", "flu_data_quality")
fs::dir_create(output_dir)

# set output width for capture.output
options(width = 200)


# Import event-level flu vaccination data ----

data_flu_table_raw  <- read_feather(here("output", "1-extract", "extract_flu", "flu_vaccinations_table.arrow"))
data_flu_snomed_raw <- read_feather(here("output", "1-extract", "extract_flu", "flu_vaccinations_SNOMED.arrow"))
data_flu_drug_raw   <- read_feather(here("output", "1-extract", "extract_flu", "flu_vaccinations_drug.arrow"))

# For each source:
# - remove rows where vaccination date is missing
# - attach info about the campaign during which the vaccination was given
# - collapse exact duplicates (where patient id, date, and product all match)
#
# Exact duplicates are defined at the level of:
# - table source: patient_id + vax_date + vax_product + age
# - drug/SNOMED sources: patient_id + vax_date + age

# fucntion: add campaign label and campaign start date based on vaccination date
add_campaign_vars <- function(data) {
  data |>
    mutate(
      campaign = cut(
        vax_date,
        breaks = c(campaign_info$campaign_start_date, as.Date(Inf)),
        labels = campaign_info$campaign_label
      ),
      campaign_start = cut(
        vax_date,
        breaks = c(campaign_info$campaign_start_date, as.Date(Inf)),
        labels = campaign_info$campaign_start_date
      ),
      campaign = as.character(campaign),
      campaign_start = as.Date(as.character(campaign_start))
    )
}

# create a summary dataset for one source at person-campaign level
summarise_source_by_campaign <- function(data, source_name) {
  data |>
    group_by(patient_id, campaign, campaign_start) |>
    summarise(
      !!paste0("n_vax_", source_name) := n(),
      !!paste0("n_vax_including_exact_duplicates_", source_name) := sum(n),
      !!paste0("vax_dates_", source_name, "_list") := list(sort(unique(vax_date))),
      .groups = "drop"
    ) |>
    arrange(patient_id, campaign_start)
}

# process vaccinations table source ----

data_flu_table <-
  data_flu_table_raw |>
  lazy_dt() |>
  arrange(patient_id, vax_date) |>
  filter(!is.na(vax_date)) |>
  count(patient_id, vax_date, vax_product, age) |>
  as_tibble() |>
  add_campaign_vars()

data_flu_table_summary_campaign <-
  summarise_source_by_campaign(data_flu_table, "table")


# process drug source ----

data_flu_drug <-
  data_flu_drug_raw |>
  lazy_dt() |>
  arrange(patient_id, vax_date) |>
  filter(!is.na(vax_date)) |>
  count(patient_id, vax_date, age) |>
  as_tibble() |>
  add_campaign_vars()

data_flu_drug_summary_campaign <-
  summarise_source_by_campaign(data_flu_drug, "drug")


# process snomed source ----

data_flu_snomed <-
  data_flu_snomed_raw |>
  lazy_dt() |>
  arrange(patient_id, vax_date) |>
  filter(!is.na(vax_date)) |>
  count(patient_id, vax_date, age) |>
  as_tibble() |>
  add_campaign_vars()

data_flu_snomed_summary_campaign <-
  summarise_source_by_campaign(data_flu_snomed, "snomed")



# Combine source summaries ----

# Join person-campaign summaries across all three sources
data_flu_summary_campaign_all <-
  data_flu_table_summary_campaign |>
  full_join(
    data_flu_drug_summary_campaign,
    by = c("patient_id", "campaign", "campaign_start")
  ) |>
  full_join(
    data_flu_snomed_summary_campaign,
    by = c("patient_id", "campaign", "campaign_start")
  )

# Functions for comparing dates across sources ----

# Return TRUE if the two date vectors share at least one exact date
has_same_day <- function(dates1, dates2) {
  if (length(dates1) == 0 || length(dates2) == 0) {
    return(FALSE)
  } else {
    any(dates1 %in% dates2)
  }
}


# Return TRUE if any date pair across the two vectors is within n_days
has_within_n_days <- function(dates1, dates2, n_days = 7) {
  if (length(dates1) == 0 || length(dates2) == 0) {
    return(FALSE)
  } else {
    any(abs(outer(dates1, dates2, "-")) <= n_days) # if memory issues rethink outer()
  }

}

# Create dataset across sources ----
flu_sources <-
  data_flu_summary_campaign_all |>
  mutate(
    # indicators for whether the person appears in each source in a given campaign
    in_table  = !is.na(n_vax_table)  & n_vax_table  > 0,
    in_drug   = !is.na(n_vax_drug)   & n_vax_drug   > 0,
    in_snomed = !is.na(n_vax_snomed) & n_vax_snomed > 0,

    # mutually exclusive source combination
    source_combination = case_when(
      in_table & !in_drug & !in_snomed ~ "table only",
      !in_table & in_drug & !in_snomed ~ "drug only",
      !in_table & !in_drug & in_snomed ~ "snomed only",
      in_table & in_drug & !in_snomed ~ "table + drug",
      in_table & !in_drug & in_snomed ~ "table + snomed",
      !in_table & in_drug & in_snomed ~ "drug + snomed",
      in_table & in_drug & in_snomed ~ "table + drug + snomed"
    ),

    # agreement in vaccination dates across sources
    same_day_table_drug =
      map2_lgl(vax_dates_table_list, vax_dates_drug_list, has_same_day),

    same_day_table_snomed =
      map2_lgl(vax_dates_table_list, vax_dates_snomed_list, has_same_day),

    within_7d_table_drug =
      map2_lgl(vax_dates_table_list, vax_dates_drug_list, has_within_n_days),

    within_7d_table_snomed =
      map2_lgl(vax_dates_table_list, vax_dates_snomed_list, has_within_n_days)
  )

# Table 1. Source combinations by campaign ----
  table1_flu_sources <- flu_sources |>
  group_by(campaign, campaign_start, source_combination) |>
  summarise(
    n_source = round_any(n(), sdc_threshold),
    .groups = "drop"
  ) |>
  group_by(campaign, campaign_start) |>
  mutate(
    tot_camp = sum(n_source),
    perc_source = round(n_source / tot_camp * 100, 1),
    n_perc_source = glue("{n_source} ({round(perc_source, 1)}%)")
  ) |>
  ungroup() |>
  arrange(campaign_start, source_combination)

write_csv(
  table1_flu_sources,
  here(output_dir, "table1_flu_sources.csv")
)


# Figure 1. UpSet plot: Source combinations by campaign ----
campaigns <- unique(flu_sources$campaign)

# prepare data
upset_plot_data <- table1_flu_sources %>%
  mutate(
    in_table = source_combination %in% c(
      "table only", "table + drug", "table + snomed", "table + drug + snomed"
    ),
    in_drug = source_combination %in% c(
      "drug only", "table + drug", "drug + snomed", "table + drug + snomed"
    ),
    in_snomed = source_combination %in% c(
      "snomed only", "table + snomed", "drug + snomed", "table + drug + snomed"
    )
  ) %>%
  uncount(n_source) %>%
  select(campaign, campaign_start, in_table, in_drug, in_snomed) %>%
  mutate(across(c(in_table, in_drug, in_snomed), as.integer)) %>%
  as.data.frame()

write_csv(
  upset_plot_data,
  here(output_dir, "upset_plot_data.csv")
)
###############
# run locally # -----------------------------------------------------------
###############
# upset_plot_data <- readr::read_csv(
#   here("output", "2-prepare", "flu_data_quality", "upset_plot_data.csv")
# )

# loop campaigns
# for (camp in campaigns) {
#
#   plot_data <- upset_plot_data %>%
#     filter(campaign == camp) %>%
#     select(in_table, in_drug, in_snomed) %>%
#     as.data.frame()
#
#   p <- upset(
#     plot_data,
#     sets = c("in_table", "in_drug", "in_snomed"),
#     sets.bar.color = "grey40",
#     order.by = "freq",
#     main.bar.color = "grey20",
#     text.scale = 1.2,
#     mainbar.y.label = paste("intersection size -", camp),
#     sets.x.label = "people in each source"
#   )
#   dev.off()
#   print(p)
# }

###############################################################

# Table 2. Date agreement across sources by campaign ----
  table_date_agreement <-
    bind_rows(
      flu_sources |>
        filter(in_table, in_drug) |>
        group_by(campaign) |>
        summarise(
          comparison = "Same day: Table vs Drug",
          n = roundmid_any(sum(same_day_table_drug, na.rm = TRUE), sdc_threshold),
          denom = roundmid_any(n(), sdc_threshold),
          .groups = "drop"
        ),
      flu_sources |>
        filter(in_table, in_snomed) |>
        group_by(campaign) |>
        summarise(
          comparison = "Same day: Table vs SNOMED",
          n = roundmid_any(sum(same_day_table_snomed, na.rm = TRUE), sdc_threshold),
          denom = roundmid_any(n(), sdc_threshold),
          .groups = "drop"
        ),
      flu_sources |>
        filter(in_table, in_drug) |>
        group_by(campaign) |>
        summarise(
          comparison = "Within 7 days: Table vs Drug",
          n = roundmid_any(sum(within_7d_table_drug, na.rm = TRUE), sdc_threshold),
          denom = roundmid_any(n(), sdc_threshold),
          .groups = "drop"
        ),
      flu_sources |>
        filter(in_table, in_snomed) |>
        group_by(campaign) |>
        summarise(
          comparison = "Within 7 days: Table vs SNOMED",
          n = roundmid_any(sum(within_7d_table_snomed, na.rm = TRUE), sdc_threshold),
          denom = roundmid_any(n(), sdc_threshold),
          .groups = "drop"
        )
    ) |>
    mutate(
      pct = 100 * n / denom,
      n_pct = glue("{n} ({round(pct, 1)}%)")
    ) |>
    arrange(campaign, comparison)

write_csv(table_date_agreement,here(output_dir, "table_date_agreement.csv"))