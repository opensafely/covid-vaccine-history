library("tidyverse")
library("googlesheets4")



# key study dates
# The dates are saved in json format so they can be read in by R and python scripts
# - firstpossiblevax_date is the date from which we want to identify covid vaccines. the mass vax programme was 8 Dec 2020 but other people were vaccinated earlier in trials, so want to pick these people up too (and possibly exclude them)
# - start_date is when we start the observational period proper, at the start of the mass vax programme
# - end_date is when we stop the observation period. This must be extended as the study progresses

study_dates <-
  list(
    firstpossiblevax_date = "2020-03-02", # approximate date when people started to be included in trials
    start_date = "2020-12-07",
    end_date = "2026-03-31"
  ) |>
  lapply(as.Date)


jsonlite::write_json(
  study_dates,
  path = here::here("analysis", "0-lib", "study_dates.json"),
  pretty = TRUE, auto_unbox = TRUE
)



# make these available in the global environment
# so we don't have to use `study_dates$start_date` or `start_date <- study_dates$start_date` in each script
# list2env(study_dates, globalenv())


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# covid-19 vaccine campaign dates ----
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# when authentication is resolves use this, instead of copy-pasting into the repo
# campaign_info <- 
#   read_sheet(
#     ss = "1CsioktjYJAb8ngU_wqh8hJF8Px4x4j3xvwQfNZdQbBQ",
#     sheet = "campaigns",
#     range = cell_limits(c(2, 1), c(NA, 9)), #start from row 2 and col 1, end at last non-empy row and col 9.
#     col_names = TRUE
#   )

# use this until googlesheets authentication is sorted
campaign_info0 <- 
  read_csv(
    file = fs::path("analysis/0-lib/campaign_info0.csv"),
    col_types = "cccccccdd"
  ) |>
  mutate(
    across(c("campaign_start_date","early_milestone_date", "primary_milestone_date", "final_milestone_date", "age_date"), \(x) as.Date(x, "%d/%m/%Y"))
  ) 

campaign_info_pre <-
  tribble(
    ~campaign_label,  ~campaign_start_date, ~early_milestone_date, ~primary_milestone_date, ~final_milestone_date, ~clinical_priority, ~age_date, ~age_threshold,
    "Pre-pandemic", "1900-01-01", "1900-01-01", "1900-01-01", "1900-01-01", NA, NA, 16,
    "Pre-roll-out",  as.character(study_dates$firstpossiblevax_date), as.character(study_dates$firstpossiblevax_date), as.character(study_dates$firstpossiblevax_date), as.character(study_dates$firstpossiblevax_date), NA, as.character(study_dates$firstpossiblevax_date), 16, 
  ) |>
  mutate(
    across(c("campaign_start_date", "early_milestone_date", "primary_milestone_date", "final_milestone_date", "age_date"), as.Date)
  )

campaign_info <-
  bind_rows(
    campaign_info_pre,
    campaign_info0,
    
  ) |>
  arrange(campaign_start_date) |>
   mutate(
    campaign_label = fct_inorder(campaign_label),
    across(c(campaign_start_date, primary_milestone_date, age_date), as.Date),
  )


# check that import dates follow the expected rules
# even though early and final milestone dates can be derived from the campaign start dates
# these are hard-coded into the spreadsheet so that then can be viewed easily and don't have to be computed
# this check just ensures that they've been hard coded correctly. 

campaign_info_check <- 
  campaign_info |> 
  filter(
  !(campaign_label %in% c("Pre-pandemic", "Pre-roll-out"))
  ) |>
  mutate(
    early_milestone_date_check = campaign_start_date + (7 * 8) - 1, # end of eighth week after campaign_start_date
    final_milestone_date_check = lead(campaign_start_date, 1, NA) - 1 # day before next campaign date
  )

stopifnot(
  "early milestone date is not end of the 8th week after campaign start date, as required" = 
  identical(campaign_info_check$early_milestone_date, campaign_info_check$early_milestone_date_check)
)

stopifnot(
  "final milestone date is not one day before the start date of the previous campaign, as required" = 
  identical(campaign_info_check$final_milestone_date, campaign_info_check$final_milestone_date_check)
)



# write to disk
write_feather(campaign_info, fs::path("analysis/0-lib/campaign_info.arrow"))


# write to disk as json for use by ehrql scripts
jsonlite::write_json(
  split(campaign_info, f = campaign_info$campaign_start_date) |> lapply(as.list),
  path = here::here("analysis", "0-lib", "campaign_info.json"),
  pretty = TRUE, auto_unbox = TRUE,
)



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# covid-19 vaccine product look-up ----
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# when authentication is resolves use this, instead of copy-pasting into the repo
# campaign_info <- 
#   read_sheet(
#     ss = "1CsioktjYJAb8ngU_wqh8hJF8Px4x4j3xvwQfNZdQbBQ",
#     sheet = "products",
#     range = cell_limits(c(2, 1), c(NA, 7)), #start from row 2 and col 1, end at last non-empy row and col 7.
#     col_names = TRUE
#   )


# use this until googlesheets authentication is sorted
product_info0 <- 
  read_csv(
    file = fs::path("analysis/0-lib/product_info0.csv"),
    col_types = "ccccccc"
  ) |>
  mutate(
    across(c("uk_approval_date"), \(x) as.Date(x, "%d/%m/%Y"))
  )

# write to disk
write_feather(product_info0, fs::path("analysis/0-lib/product_info.arrow"))

# output from https://jobs.opensafely.org/opensafely-internal/tpp-vaccination-names/ workspace
# shows all possible covid vaccination names in TPP

