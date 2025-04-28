# _________________________________________________
# Purpose:
# define useful functions used in the codebase
# define key design features for the study
# define some look up tables to use in the codebase
# this script should be sourced (using `source(here("analysis", "utility.R"))`) at the start of each R script
# _________________________________________________

# utility functions ----

roundmid_any <- function(x, to = 1) {
  # like ceiling_any, but centers on (integer) midpoint of the rounding points
  ceiling(x / to) * to - (floor(to / 2) * (x != 0))
}

ceiling_any <- function(x, to = 1) {
  # round to nearest 100 millionth to avoid floating point errors
  ceiling(plyr::round_any(x / to, 1 / 100000000)) * to
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
  factor(dplyr::case_when(...), levels=levels)
}


# Design elements ----

# key study dates
# The dates are defined in json format so they can be read in by R and python scripts
# json has no easy way to comment, so explanation for dates is here:
# - firstpossiblevax_date is the date from which we want to identify covid vaccines. the mass vax programme was 8 Dec 2020 but other people were vaccinated earlier in trials, so want to pick these people up too (and possibly exclude them)
# - start_date is when we start the observational period proper, at the start of the mass vax programme
# - end_date is when we stopthe observation period. This may be extended as the study progresses
study_dates <-
  jsonlite::read_json(path = here("analysis", "0-lib", "dates.json")) %>%
  map(as.Date)

# make these available in the global environment
# so we don't have to use `study_dates$start_date` or `start_date <- study_dates$start_date` in each script
list2env(study_dates, globalenv())

# statistical disclosure control rounding precision
sdc_threshold <- 100

# covid-19 vaccine campaign dates
campaign_dates <-
  tribble(
    ~campaign,        ~start,       ~start_rounded,
    "Pre-roll-out",   as.character(firstpossiblevax_date), as.character(firstpossiblevax_date),
    "Primary series", "2020-12-08", "2020-12-01",
    "Autumn 2021",    "2021-09-06", "2021-09-01",
    "Spring 2022",    "2022-04-01", "2022-04-01",
    "Autumn 2022",    "2022-09-12", "2022-09-01",
    "Spring 2023",    "2023-04-03", "2023-04-01",
    "Autumn 2023",    "2023-09-11", "2023-09-01",
    "Spring 2024",    "2024-04-01", "2023-04-01",
    "Autumn 2024",    "2024-10-03", "2024-09-01",
    #"Spring 2025",    "", "",
  ) %>%
  mutate(
    across(c(start, start_rounded), as.Date)
  )


# output from https://jobs.opensafely.org/opensafely-internal/tpp-vaccination-names/ workspace
# shows all possible covid vaccination names in TPP

# lookup to rename TPP product names to coding-friendly porduct names
vax_product_lookup <- c(
  "pfizer" = "COVID-19 mRNA Vaccine Comirnaty 30micrograms/0.3ml dose conc for susp for inj MDV (Pfizer)",
  "az" = "COVID-19 Vaccine Vaxzevria 0.5ml inj multidose vials (AstraZeneca)",
  "moderna" = "COVID-19 mRNA Vaccine Spikevax (nucleoside modified) 0.1mg/0.5mL dose disp for inj MDV (Moderna)",
  "pfizerBA1" = "Comirnaty Original/Omicron BA.1 COVID-19 Vacc md vials",
  "pfizerBA45" = "Comirnaty Original/Omicron BA.4-5 COVID-19 Vacc md vials",
  "pfizerXBB15" = "Comirnaty Omicron XBB.1.5 COVID-19 Vacc md vials",
  "vidprevtyn" = "COVID-19 Vacc VidPrevtyn (B.1.351) 0.5ml inj multidose vials",
  "modernaomicron" = "COVID-19 Vac Spikevax (Zero)/(Omicron) inj md vials",
  "pfizerchildren" = "COVID-19 mRNA Vaccine Comirnaty Children 5-11yrs 10mcg/0.2ml dose conc for disp for inj MDV (Pfizer)",
  "azhalf" = "COVID-19 Vac AZD2816 (ChAdOx1 nCOV-19) 3.5x10*9 viral part/0.5ml dose sol for inj MDV (AstraZeneca)",
  "modernaXBB15" = "COVID-19 Vacc Spikevax (XBB.1.5) 0.1mg/1ml inj md vials",
  "novavax" = "COVID-19 Vac Nuvaxovid (recombinant, adj) 5micrograms/0.5ml dose susp for inj MDV (Novavax CZ a.s.)"
)


# lookup to rename coding-friendly product names to publication-friendly product names
vax_shortname_lookup <- c(
  "BNT162b2" = "pfizer",
  "ChAdOx1" = "az",
  "mRNA-1273" = "moderna",
  "BNT162b2/BA.1" = "pfizerBA1",
  "BNT162b2/BA.4-5" = "pfizerBA45",
  "BNT162b2/XBB.1.5" = "pfizerXBB15",
  "VidPrevtyn" = "vidprevtyn",
  "mRNA-1273/omicron" = "modernaomicron",
  "BNT162b2/children" = "pfizerchildren",
  "ChAdOx1/2" = "azhalf",
  "mRNA-1273/XBB.1.5" = "modernaXBB15",
  "Novavax" = "novavax",
  "Other" = "other"
)


# relabel_from_lookup <- function(x, from, to, source){
#   left_join(tibble(x=x), source, by = {{from}})[[{{to}}]]
# }


# template for standardising characteristics that are extracted multiple times
# using this in mutate like this: `mutate(!!!standardise_characteristics)`
standardise_characteristics <-
  rlang::quos(

    ## --VARIABLES--
    ## demographics
    ageband = cut(
      age,
      breaks = c(-Inf, 18, 40, 55, 65, 75, Inf),
      labels = c("under 18", "18-39", "40-54", "55-64", "65-74", "75+"),
      right = FALSE
    ),
    region = fct_collapse(
      region,
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
    ) #,
    ## PRIMIS clinical variables
   # cv = crd | ast | chd | ckd | cld | cns | learndis | diab | immuno | asplen | obes | sev_ment
 #FIXME add additional vulnerability variables when defined and extracted
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
  )

# function to convert ethnicity 16 group into 5 group
ethnicity_16_to_5 <- function(x) {
  x1 <- fct_relabel(x, ~ str_extract(.x, ".*(?= - )")) # pick up everything before " - "
  x2 <- fct_recode(x1, `Chinese or Other Ethnic Groups` = "Other Ethnic Groups")
  return(x2)
}



