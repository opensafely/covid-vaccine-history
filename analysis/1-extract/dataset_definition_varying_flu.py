# _________________________________________________
# Purpose:
# extract event level flu vaccination from vaccinations table, medicines and clinical_event
# _________________________________________________

# import libraries and functions

from json import loads
from pathlib import Path

from ehrql import (
   # case,
    create_dataset,
    # days,
    # when,
    # minimum_of,
    # maximum_of,
    claim_permissions
)
from ehrql.tables.tpp import (
  patients,
  practice_registrations, 
  medications,
  vaccinations, 
  clinical_events, 
#   ons_deaths,
#   addresses,
)
# import codelists
import codelists

study_dates = loads(
    Path("analysis/0-lib/study_dates.json").read_text(),
)

# Change these in ./0-lib/deisgn.R if necessary
start_date = study_dates["start_date"]
end_date = study_dates["end_date"]

# all covid-19 vaccination events

## Vaccineations table 
flu_vaccinations_table = (
  vaccinations
  .where(vaccinations.target_disease.is_in(["INFLUENZA"]))
  .sort_by(vaccinations.date)
)

## Clinical events
flu_vaccinations_SNOMED = (clinical_events
  .where(clinical_events.snomedct_code.is_in(codelists.flu_vac_SNOMED))
  .sort_by(clinical_events.date)
)

# Medications
flu_vaccinations_drug = (medications
  .where(medications.dmd_code.is_in(codelists.flu_vac_drug))
  .sort_by(medications.date)
)


# initialise dataset
dataset = create_dataset()
dataset.configure_dummy_data(population_size=1000)

# define dataset poppulation
dataset.define_population(
   (flu_vaccinations_table.exists_for_patient() |
    flu_vaccinations_SNOMED.exists_for_patient()|    
    flu_vaccinations_drug.exists_for_patient()
   )
   & (patients.age_on(end_date) >= 12) # only include people who are aged 12 or over during at least one season
)

# event level permissions
claim_permissions("event_level_data")


# Create datasets 
dataset.add_event_table(
    "flu_vaccinations_table",
    vax_date = flu_vaccinations_table.date,
    vax_product = flu_vaccinations_table.product_name,
    age = patients.age_on(flu_vaccinations_table.date),
)

dataset.add_event_table(
    "flu_vaccinations_SNOMED",
    vax_date = flu_vaccinations_SNOMED.date,
#    vax_snomed = flu_vaccinations_SNOMED.product_name,
    age = patients.age_on(flu_vaccinations_SNOMED.date),
)

dataset.add_event_table(
    "flu_vaccinations_drug",
    vax_date = flu_vaccinations_drug.date,
#    vax_drug = flu_vaccinations_drug.product_name,
    age = patients.age_on(flu_vaccinations_drug.date),
)