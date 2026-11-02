# _________________________________________________
# Purpose:
# define useful functions used in the codebase
# define key design features for the study
# define some look up tables to use in the codebase
# this script should be sourced (using `source(here("analysis", "design.R"))`) at the start of each R script
# _________________________________________________

library("tidyverse")

# utility functions ----

roundmid_any <- function(x, to = 1L) {
  # like ceiling_any, but centers on (integer) midpoint of the rounding points
  if (to == 0) {
    x
  } else {
    as.integer(ceiling(x / to) * to - (floor(to / 2) * (x != 0)))
  }
}

ceiling_any <- function(x, to = 1L) {
  # round to nearest 100 millionth to avoid floating point errors

  if (to == 0) {
    return(x)
  } else {
    return(ceiling(plyr::round_any(x / to, 1 / 100000000)) * to)
    # return(plyr::round_any(x, to, f=ceiling))
  }
}


floor_any <- function(x, to = 1L) {
  # round to nearest 100 millionth to avoid floating point errors

  if (to == 0) {
    return(x)
  } else {
    return(floor(plyr::round_any(x / to, 1 / 100000000)) * to)
    # return(plyr::round_any(x, to, f=floor))
  }
}


round_any <- function(x, to = 1) {
  if (to == 0) {
    x
  } else {
    if_else(x != 5, plyr::round_any(x, accuracy = to), 0)
  }
}

# get nth largest value from list
nthmax <- function(x, n = 1) {
  dplyr::nth(sort(x, decreasing = TRUE), n)
}

# get nth smallest value from list
nthmin <- function(x, n = 1) {
  dplyr::nth(sort(x, decreasing = FALSE), n)
}

# overwrite splice function to avoid deprecation warnings
splice <- function(...) {
  list_flatten(lst(...), name_spec = "{inner}", name_repair = "check_unique")
}

# uses dplyr::case_when but converts the output to a factor,
# with factors ordered as they appear in the case_when's  ... argument
fct_case_when <- function(...) {
  args <- as.list(match.call())
  levels <- sapply(args[-1], function(f) f[[3]])  # extract RHS of formula
  levels <- levels[!is.na(levels)]
  factor(dplyr::case_when(...), levels = levels)
}


# Design elements ----

# statistical disclosure control rounding precision
sdc_threshold <- 10L


# import processed campaign info
campaign_info <-
  read_feather(
    file = fs::path("analysis/0-lib/campaign_info"),
  ) |>
  mutate(
    early_milestone_days = as.integer(
      early_milestone_date - campaign_start_date
    ) +
      1L,
    primary_milestone_days = as.integer(
      primary_milestone_date - campaign_start_date
    ) +
      1L,
    final_milestone_days = as.integer(
      final_milestone_date - campaign_start_date
    ) +
      1L
  )


# import processed product info
# lookup to rename TPP product names to coding-friendly product names
# taking format: manufacturer/brand _ era/variant _ (dose/target) _ (modality)

product_info <-
  read_feather(
    file = fs::path("analysis/0-lib/product_info.arrow"),
  ) |>
  mutate(
    first_campaign_date = recode_values(
      first_campaign,
      from = campaign_info$campaign_label,
      to = campaign_info$campaign_start_date
    )
  )


# list of vaccine products of interest
vax_product_core_levels <- c(
  "pfizer_original",
  "pfizer_BA1",
  "pfizer_BA45",
  "pfizer_XBB15",
  "pfizer_JN1",
  "pfizer_LP81",
  "pfizer_KP2",

  "pfizer_original_children",

  "az_original",
  "az_original_half",

  "moderna_original",
  "moderna_omicron",
  "moderna_BA45",
  "moderna_XBB15",
  "moderna_JN1",
  "moderna_omicron2",

  "sanofigsk_B1",

  "novavax",

  "jansenn",
  "coronavac",
  "covishield"
)



## factor levels provided in a sensible order, as this won't happen directly from opensafely ----
factor_levels <-
  lst(
    ethnicity5 = c(
      "White",
      "Mixed",
      "Asian or Asian British",
      "Black or Black British",
      "Chinese or Other Ethnic Groups"
    ),
    ethnicity16 = c(
      "White - British",
      "White - Irish",
      "White - Any other White background",
      "Mixed - White and Black Caribbean",
      "Mixed - White and Black African",
      "Mixed - White and Asian",
      "Mixed - Any other mixed background",
      "Asian or Asian British - Indian",
      "Asian or Asian British - Pakistani",
      "Asian or Asian British - Bangladeshi",
      "Asian or Asian British - Any other Asian background",
      "Black or Black British - Caribbean",
      "Black or Black British - African",
      "Black or Black British - Any other Black background",
      "Other Ethnic Groups - Chinese",
      "Other Ethnic Groups - Any other ethnic group"
    ),

    learndis_cat = c(
      "No learning disability",
      "Down's syndrome",
      "Other learning disability",
      "Learning disability register"
    ),
    ckd_stage_3to5 = c(
      "no ckd",
      "3",
      "4",
      "5",
      "ckd, without ckd3-5 code"
    ),
    ckd_rrt = c(
      "No CKD or RRT",
      "RRT (transplant)",
      "RRT (dialysis)",
      "Stage 3",
      "Stage 4",
      "Stage 5",
      "CKD, without stage 3-5 code"
    )

  )


# template for standardising demographic characteristics that are extracted multiple times
# using this in mutate like this: `mutate(!!!standardise_demographic_characteristics)`
standardise_demographic_characteristics <-
  rlang::quos(

    ## --VARIABLES--
    ## demographics
    ageband4 = cut(
      age,
      breaks = c(-Inf, 12, 50, 65, 75, 105, Inf),
      labels = c("under 12", "12-49", "50-64", "65-74", "75-104", "105+"), # under 12 and 105+ should be excluded in analysis but include here to ensure nobody slipped through the net
      right = FALSE
    ),
    ageband13 = cut(
      age,
      breaks = c(-Inf, 12, 18, 30, 40, 50, 55, 60, 65, 70, 75, 80, 85, 90, 105, Inf),
      labels = c("under 12", "12-17", "18-29", "30-39", "40-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85-89", "90-104", "105+"), # under 12 and 105+ should be excluded in analysis but include here to ensure nobody slipped through the net
      right = FALSE
    ),
    region = fct_collapse(
      factor(region, ordered = FALSE),
      `East of England` = "East",
      `London` = "London",
      `Midlands` = c("West Midlands", "East Midlands"),
      `North East and Yorkshire` = c("Yorkshire and The Humber", "North East"),
      `North West` = "North West",
      `South East` = "South East",
      `South West` = "South West"
    ),
    imd_quintile = cut(
      imd,
      breaks = c(0, 32844 * (1:5) / 5),
      labels = c("1 (most deprived)", "2", "3", "4", "5 (least deprived)"),
      include.lowest = TRUE,
      right = FALSE
    ),
  )

## CKD-RRT classification
# adapted from https://github.com/opensafely/covid_mortality_over_time/blob/main/analysis/utils/kidney_functions.R

# egfr <- function(rrt_cat, sex, creatinine_umol, creatinine_age) {
#   # creatinine mg/dL from µmol/L
#   SCR_adj <- creatinine_umol / 88.4

#   # min/max creatinine
#   min_creat <- if_else(
#     sex == "male",
#     pmin(SCR_adj / 0.9, 1)^-0.411,
#     pmin(SCR_adj / 0.7, 1)^-0.329
#   )
#   max_creat <- if_else(
#     sex == "male",
#     pmax(SCR_adj / 0.9, 1)^-1.209,
#     pmax(SCR_adj / 0.7, 1)^-1.209
#   )
#   # eGFR (male/female)
#   egfr_male <- case_when(
#     is.na(creatinine_umol) ~ NA_real_,
#     is.na(creatinine_age) ~ NA_real_,
#     TRUE ~ (min_creat * max_creat * 141) * (0.993^creatinine_age)
#   )
#   egfr <- if_else(sex == "female", 1.018 * egfr_male, egfr_male)

#   return(egfr)
# }

standardise_primis_and_extended_characteristics <-
  rlang::quos(

    rrt_cat = factor(rrt_cat,  levels = levels(rrt_cat), ordered = FALSE),

    ckd_rrt = case_when(
      rrt_cat == "1 dialysis" ~ "RRT (dialysis)",
      rrt_cat == "2 transplant" ~ "RRT (transplant)",
      ckd_stage_3to5 == "5" ~ "Stage 5",
      ckd_stage_3to5 == "4" ~ "Stage 4",
      ckd_stage_3to5 == "3" ~ "Stage 3",
      ckd_stage_3to5 == "ckd, without stage 3-5 code" ~ "CKD, without stage 3-5 code",
      TRUE ~ "No CKD or RRT"
    ) |> factor(levels = factor_levels$ckd_rrt, ordered = FALSE),

    ckd_stage_3to5 = NULL, # remove ckd stage to save memory
    rrt_cat = NULL, # remove rrt_cat stage to save memory

    learndis_cat = factor(learndis_cat,  levels = factor_levels$learndis_cat, ordered = FALSE),

    cns_learndis = (cns | learndis),

    sickle_cell_asplenia = (sickle_cell | asplenia)
  )



# function to convert ethnicity 16 group into 5 group
ethnicity_16_to_5 <- function(x) {
  x1 <- fct_relabel(x, ~ str_extract(.x, ".*(?= - )")) # pick up everything before " - "
  x2 <- fct_recode(x1, `Chinese or Other Ethnic Groups` = "Other Ethnic Groups")
  return(x2)
}

# List of groups and subgroups for reporting summary statistics

## --VARIABLES--

level1_group <- c(

  # Level 1A (all)
  "all",

  # level 1B (age4)
  "ageband4",

  # level 1C (age13)
  "ageband13",

  # Level 1D (eligibility)
  "any_eligibility",
  "age_above_eligiblity_threshold",
  "clinical_priority",
  "clinical_priority_only",
  "carehome_status",

  # Level 1E (clinical risk)
  "primis_atrisk",
  "crd",
  "chd",
  "ckd",
  "cld",
  "cns_learndis",
  "diabetes",
  "immunosuppressed",
  "asplenia",
  "severe_obesity",
  "smi"
)

level2_group <- c(
  "all",

  # sociodemographic subgroups
  "ageband4",
  "ageband13",
  "sex",
  "ethnicity5",
  # "ethnicity16",
  "region",
  "imd_quintile",
  "carehome_status",

  # Core clinical risk subgroups
  "primis_atrisk",
  "crd",
  "chd",
  "ckd",
  "cld",
  "cns_learndis",
  "diabetes",
  "immunosuppressed",
  "asplenia",
  "severe_obesity",
  "smi",

  # Extended subgroups
  "ckd_rrt", # RRT - CKD3-5
  "copd", # Chronic obstructive pulmonary disease
  "learndis_cat", # Learning disabilities categories
  "sickle_cell_asplenia", # Sickle Cell or asplenia
  "cirrhosis",          # Cirrhosis
  "cochlear_implant",   # Cochlear implant
  "cystic_fibrosis",    # Cystic fibrosis
  "csfl",               # Cerebrospinal fluid leak
  "homeless"           # Homeless
)

level_combos <-
  expand_grid(
    group1 = level1_group,
    group2 = level2_group
  ) |>
  filter(
    (group1 == group2) %in% c(FALSE, NA) | (group1 == "all"),
    !((group1 %in% c("ageband4", "ageband13")) & (group2 %in% c("ageband4", "ageband13"))),
    !((group1 %in% c("age_above_eligiblity_threshold", "clinical_priority_only")) & (group2 %in% c("ageband4", "ageband13"))),
    !((group1 %in% c("clinical_priority_only")) & (group2 %in% c("primis_atrisk"))),
  )


# Local run flag ----
# is this script being run locally
# used to run bits using dummy data only, if needed
localrun <- Sys.getenv("OPENSAFELY_BACKEND") %in% c("", "expectations")