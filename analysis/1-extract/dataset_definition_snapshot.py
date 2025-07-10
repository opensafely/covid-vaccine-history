
##########################
# extract patient information as at a fixed date
# vaccination dates and info will be joined onto this later using the "varying" dataset
##########################

# import libraries and functions

from json import loads
from pathlib import Path
from datetime import datetime

from ehrql import (
    show,
    get_parameter,
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
  ons_deaths,
)

# import codelists
from codelists import *

from variables_function import *

study_dates = loads(
    Path("analysis/0-lib/dates.json").read_text(),
)

# get arguments supplied in the yaml file, stored in argv[1], argv[2], etc
snapshot_date = datetime.strptime(get_parameter(name="snapshot_date"), '%Y%m%d').strftime('%Y-%m-%d') 

# Change these in ./lib/dates.json if necessary
start_date = study_dates["start_date"]
end_date = study_dates["end_date"]

# initialise dataset

dataset = create_dataset()

dataset.configure_dummy_data(population_size=1000)

registered_patients = practice_registrations.for_patient_on(snapshot_date)

registered = registered_patients.exists_for_patient()
alive = (ons_deaths.date>snapshot_date) | ons_deaths.date.is_null()
age = patients.age_on(snapshot_date)

# define dataset poppulation
dataset.define_population(
  registered 
  & alive
  & (age >= 16)
)

# --VARIABLES--

## demographic variables ----

demographic_variables(dataset = dataset, index_date = snapshot_date)

## primis variables ----

primis_variables(dataset = dataset, index_date = snapshot_date)


# other_cx_variables(dataset = dataset, index_date = snapshot_date)

# COVID-19 vaccination history ----

covid_vaccinations = (
  vaccinations
  .where(vaccinations.target_disease.is_in(["SARS-2 CORONAVIRUS"]))
  .sort_by(vaccinations.date)
)

# retrieve first 3 vaccines before and first 2 vaccines on or after snapshot date


# add next 2 recorded covid vaccination
add_n_vaccines(
    dataset = dataset, 
    index_date = snapshot_date, 
    target_disease = "SARS-2 CORONAVIRUS", 
    name = "covid_vax", 
    direction = "on_or_after",
    number_of_vaccines = 2
)

# add most recent 3 vaccines prior to snapshot date
add_n_vaccines(
    dataset = dataset, 
    index_date = snapshot_date, 
    target_disease = "SARS-2 CORONAVIRUS", 
    name = "covid_vax_prior", 
    direction = "before",
    number_of_vaccines = 3
)

# count total number of vaccines prior to vaccine date
dataset.covid_vax_prior_count = (
  covid_vaccinations
  .where(vaccinations.date.is_before(snapshot_date))
  .count_for_patient()
)

# Deregistration dates after the snapshot date
dataset.deregistered_date = registered_patients.end_date
