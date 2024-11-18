
##########################
# extract patient information as at a fixed date
# vaccination dates and info will be joined onto this later using the "varying" dataset
##########################



from json import loads
from sys import argv
from pathlib import Path
from datetime import datetime


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
  ons_deaths,
  addresses,
)
# import codelists
from codelists import *

from cx_function import *

study_dates = loads(
    Path("lib/dates.json").read_text(),
)

# get arguments supplied in the yaml file, stored in argv[1], argv[2], etc
snapshot_date = datetime.strptime(argv[1], '%Y%m%d').strftime('%Y-%m-%d') 

# Change these in ./lib/dates.json if necessary
start_date = study_dates["start_date"]
end_date = study_dates["end_date"]

# initialise dataset

dataset = create_dataset()

dataset.configure_dummy_data(population_size=1000)

registered_patients = practice_registrations.for_patient_on(snapshot_date)

registered = registered_patients.exists_for_patient()
alive = (ons_deaths.date>snapshot_date) | ons_deaths.date.is_null()

# define dataset poppulation
dataset.define_population(
  registered 
  & alive
)

# --VARIABLES--

dataset.age = patients.age_on(snapshot_date)
dataset.region = registered_patients.practice_nuts1_region_name
dataset.stp = registered_patients.practice_stp
dataset.imd = addresses.for_patient_on(snapshot_date).imd_rounded

## PRIMIS Variables ##

## Chronic heart disease
dataset.chd = has_prior_event(chd_cov, snapshot_date)
dataset.cld = has_prior_event(cld, snapshot_date)
