
##########################
# purpose:
# extract time-invariant patient information as at a fixed time at the end of observation period
# time-invariant info: sex, ethnicity (assumed invariant), death date
# alsoextract count of previous vaccines as at that date
##########################


from json import loads
from pathlib import Path

from ehrql import (
    case,
    create_dataset,
    days,
    when,
    minimum_of,
    maximum_of
)
from ehrql.tables.tpp import (
  patients,
  practice_registrations, 
  vaccinations, 
  clinical_events, 
  ons_deaths
)
# import codelists
from codelists import *


study_dates = loads(
    Path("lib/dates.json").read_text(),
)

# Change these in ./lib/dates.json if necessary
start_date = study_dates["start_date"]
end_date = study_dates["end_date"]

# all covid-19 vaccination events
covid_vaccinations = (
  vaccinations
  .where(vaccinations.target_disease.is_in(["SARS-2 CORONAVIRUS"]))
  .where(vaccinations.date.is_on_or_before(end_date))
  .sort_by(vaccinations.date)
)

registered_at_any_time_in_observation_period = practice_registrations.where(
  # starting during period
  practice_registrations.start_date.is_on_or_between(start_date, end_date) |
  
  # ending during period
  practice_registrations.end_date.is_on_or_between(start_date, end_date) | 
  
  # starting before and ending after
  (
    practice_registrations.start_date.is_on_or_before(start_date) &
    (practice_registrations.end_date.is_on_or_after(end_date + days(1)) | practice_registrations.end_date.is_null())
  )
)

# initialise dataset
dataset = create_dataset()

# define dataset poppulation
dataset.define_population(registered_at_any_time_in_observation_period.exists_for_patient())

# patient sex
dataset.sex = patients.sex

# patient ethnicity 
# note that enthicity is documented using codelists, not as a categorical variable
# if ethnicity was transferred from another practice, the date may not have been captured, and will default to 1900-01-01
# we choose here to look at the last known ethnicity recorded _across the entire record_ 
# rather than as known/recorded on the vaccination date, even though this looks into the future.

# ethnicity = (clinical_events
#   .where(clinical_events.snomedct_code.is_in(ethnicity_codelist16))
#   .sort_by(clinical_events.date)
#   .where(clinical_events.date.is_on_or_before(end_date))
#   .last_for_patient()
# )
# 
# # ethnicity using 5 groups + unknown
# dataset.ethnicity5 = ethnicity.snomedct_code.to_category(ethnicity_codelist5)
# 
# # ethnicity using 15 groups + unknown
# dataset.ethnicity16 = ethnicity.snomedct_code.to_category(ethnicity_codelist16)

# patient death date
dataset.death_date = ons_deaths.date

# number of covid vaccines as at end of observation period
dataset.covid_vax_count = covid_vaccinations.count_for_patient()
