# _______________________________________________________________________________________
# Purpose:
# Import released data and create some plots providing examples of how things might look
# in a dashboard
# _______________________________________________________________________________________


# Preliminaries ----

# Import libraries
library("tidyverse")
library("lubridate")
library("glue")
library("here")
library("arrow")

# Import custom functions
source(here("analysis", "0-lib", "design.R"))

campaign_info <-
  campaign_info |>
  filter(
    campaign_start_date >= as.Date("2020-12-07"), # only select actual campaigns
  ) |>
  mutate(
    campaign_start_date_compact = format(campaign_start_date, "%Y%m%d"),
  ) |>
  # selection of campaigns for testing
  filter(
    campaign_start_date >= as.Date("2023-08-28"),
    campaign_start_date <= as.Date("2025-03-31"),
  )

# Import released data ----

#input_dir_snapshot <- here("output", "4-snapshot")
input_dir_snapshot <- here("release", "4-snapshot")

event_name <- c(
  "vax",
  "vax_alive",
  "covid_admitted",
  #"covid_admitted_primary",
  "covid_critcare",
  "covid_death"
)

milestone <- fct_inorder(c("Early", "Primary", "Final"))

## import prior vaccine history ----

data_prior_vax <-
  campaign_info |>
  mutate(
    data = map(
      campaign_start_date_compact, \(.x)
      read_csv(
        fs::path(input_dir_snapshot, glue("report_snapshot_{.x}"), glue("prior_vax_table.csv")),
        col_types = c("cccciiiiiiidddddiiddddddddddddd")
      )
    )
  ) |>
  unnest(data)


## import vaccine cumulative incidence estimates ----

data_uptake <-
  campaign_info |>
  select(-age_date, -ends_with("_milestone_date"), -ends_with("_milestone_days")) |>
  expand_grid(event_name, milestone) |>
  filter(
    event_name %in% c("vax", "vax_alive")
  ) |>
  mutate(
    data = pmap(
      list(campaign_start_date_compact, event_name, milestone), \(.x, .y, .z)
        read_csv(
          fs::path(input_dir_snapshot, glue("report_snapshot_{.x}"), glue("km_estimates_{.y}_milestones_{.z}.csv")),
          col_types = c("ccccidddDc")
        ) |>
        select(-milestone)
    )
  ) |>
  unnest(data)

## import event IR and IRR estimates ----

data_contrasts <-
  campaign_info |>
  select(-age_date, -ends_with("_milestone_date"), -ends_with("_milestone_days")) |>
  expand_grid(event_name) |>
  mutate(
    campaign_start_date_compact = format(campaign_start_date, "%Y%m%d"),
    data = map2(
      campaign_start_date_compact, event_name, \(.x,.y)
        read_csv(
          fs::path(input_dir_snapshot, glue("report_snapshot_{.x}"), glue("contrasts_{.y}.csv")),
          col_types = c("cccciidldddddd") # "exposure" should be an integer col but some values exceed the int limit so coding as a double
        )
    )
  ) |>
  unnest(data) |>
  mutate(
    # add confidence limits for incidence rate
    ir.ln.se = 1/sqrt(n_event),
    ir.low = exp(log(ir) + ir.ln.se*qnorm(0.025)),
    ir.high = exp(log(ir) + ir.ln.se*qnorm(0.975)),
    .after = "ir"
  ) |>
  mutate(
    irr = if_else(reference_row, 1, irr),
    irr.low = if_else(reference_row, 1, irr.low),
    irr.high = if_else(reference_row, 1, irr.high),
  )

## Create example plots and tables for dashboard

## select some example campaigns, groups, outcomes...

select_campaign <- "Autumn 2024"
select_group1 <- "ageband4"
select_group2 <- "ckd"

data_uptake_select <-
  data_uptake |>
  filter(
    campaign_label == select_campaign,
    event_name == "vax",
    group1 == select_group1,
    group2 == select_group2,
  )

data_contrast_select <-
  data_contrasts |>
  filter(
    campaign_label == select_campaign,
    event_name == "covid_admitted",
    group1 == select_group1,
    group2 == select_group2,
  )


## plot uptake milestones ----
ggplot(data_uptake_select) +
  geom_pointrange(
    aes(
      x = cmlinc,
      xmin = cmlinc.low,
      xmax = cmlinc.high,
      colour = milestone,
      y = group2_value,
      #      size = log(n_event)
    ), alpha=0.2, position = position_dodge(width=0.5)
  ) +
  facet_grid(cols = vars(group1_value)) +
  scale_x_continuous(
    breaks = c(0, 0.5, 1),
    limits = c(0, 1)
  ) +
  labs(
    x = "Vaccine converage (%)",
    y = select_group2
  ) +
  theme_bw()


## plot incidence rates (IRs) ----
ggplot(data_contrast_select) +
  geom_pointrange(
    aes(
      x = ir,
      xmin = ir.low,
      xmax = ir.high,
      y = label2,
      #size = log(n_event)
    )
  ) +
  facet_grid(cols = vars(label1), scales = "free_x") +
  scale_x_continuous(
    labels = scales::label_number(scale = 365.25*1000),
    limits = c(0, NA)
  ) +
  labs(
    x = "Incidence rate per 1,000 person-years",
    y = select_group2
  ) +
  theme_bw()


## plot incidence rate ratios (IRRs) (contrasts) ----
ggplot(data_contrast_select) +
  geom_pointrange(
    aes(
      x = irr,
      xmin = irr.low,
      xmax = irr.high,
      y = label2,
    )
  ) +
  geom_vline(aes(xintercept=1), linetype="dotted") +
  facet_grid(cols = vars(label1))+
  scale_x_log10(
    breaks = c(0.125, 0.25, 0.5, 1, 2, 4, 8, 16, 32),
  ) +
  labs(
    x = "Incidence rate ratio (95% CI)",
    y = select_group2
  ) +
  theme_bw()

## gt table


