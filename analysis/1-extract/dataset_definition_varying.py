# _________________________________________________
# Purpose:
# extract patient information as at each vaccination date
# column set 1 = details of first vaccination event
# column set 2 = details of second vaccination event
# _________________________________________________

# import libraries and functions

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
  ons_deaths,
  addresses,
)
# import codelists
from codelists import *


study_dates = loads(
    Path("analysis/0-lib/dates.json").read_text(),
)

# Change these in ./lib/dates.json if necessary
start_date = study_dates["start_date"]
end_date = study_dates["end_date"]

#import function for clinical variables
from variables_function import *

# all covid-19 vaccination events
covid_vaccinations = (
  vaccinations
  .where(vaccinations.target_disease.is_in(["SARS-2 CORONAVIRUS"]))
  .sort_by(vaccinations.date)
)

# initialise dataset
dataset = create_dataset()
dataset.configure_dummy_data(population_size=1000)

# define dataset poppulation
dataset.define_population(
   covid_vaccinations.exists_for_patient()
   & (patients.age_on(end_date) >=16) # only include people who are aged 16 or over during at least one season
)


# Arbitrary date guaranteed to be before any vaccination events of interest
previous_vax_date = "1899-01-01"

# loop over first, second, ..., nth vaccination event for each person
# extract info on vaccination date and product, and basic demographics
for i in range(1, 16+1):

    suffix = f"_{i}"

    ## --VARIABLES--
    
    # vaccine variables
    current_vax = covid_vaccinations.where(covid_vaccinations.date>previous_vax_date).first_for_patient()
    dataset.add_column(f"covid_vax_{i}_date", current_vax.date)
    dataset.add_column(f"covid_vax_product_{i}", current_vax.product_name)
    
    # registration variables
    registration = practice_registrations.for_patient_on(current_vax.date)
    dataset.add_column(f"registered_{i}", registration.exists_for_patient())
    dataset.add_column(f"deregistered_{i}_date", registration.end_date)
    
    # deomgraphic variables
    demographic_variables(dataset = dataset, index_date = current_vax.date, var_name_suffix = suffix)
    
    # primis variables
#    primis_variables(dataset = dataset, index_date = current_vax.date, var_name_suffix = suffix)
#    dataset.add_column(f"primis_atrisk_{i}", primis_atrisk(current_vax.date)) # at risk 

    # other clinical variables
#    other_cx_variables(dataset = dataset, index_date = current_vax.date, var_name_suffix = suffix)

    previous_vax_date = current_vax.date
    


### Add event-level data extract to test equivalence of ELD extract versus patient-level extract 

# all covid-19 vaccination events
# i don't think it's possible to restrict to first 16 events (as above for patient level data)
# so I'll do this post-extract
dataset.add_event_table(
    "vaccinations",
    vax_date = covid_vaccinations.where(covid_vaccinations.date>"1899-01-01").date,
    vax_product = covid_vaccinations.product_name,
    age = patients.age_on(covid_vaccinations.date),
)
