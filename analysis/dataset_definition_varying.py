# _________________________________________________
# Purpose:
# extract patient information as at each vaccination date
# column set 1 = details of first vaccination event
# column set 2 = details of second vaccination event
# _________________________________________________

# import libraries
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

#import function for clinical variables
from analysis.variables_function import *

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
dataset.define_population(covid_vaccinations.exists_for_patient())


# Arbitrary date guaranteed to be before any vaccination events of interest
previous_vax_date = "1899-01-01"

# loop over first, second, ..., nth vaccination event for each person
# extract info on vaccination date and type, and basic demographics
for i in range(1, 16+1):

    suffix = f"_{i}"

    ## --VARIABLES--
    
    # vaccine variables
    current_vax = covid_vaccinations.where(covid_vaccinations.date>previous_vax_date).first_for_patient()
    dataset.add_column(f"covid_vax_{i}_date", current_vax.date)
    dataset.add_column(f"covid_vax_type_{i}", current_vax.product_name)
    
    # registration variables
    registration = practice_registrations.for_patient_on(current_vax.date)
    dataset.add_column(f"registered_{i}", registration.exists_for_patient())
    dataset.add_column(f"deregistered_{i}_date", registration.end_date)
    
    # deomgraphic variables
    demographic_variables(dataset = dataset, index_date = current_vax.date, var_name_suffix = suffix)
    
    # primis variables
    primis_variables(dataset = dataset, index_date = current_vax.date, var_name_suffix = suffix)

    # primis variables
    other_cx_variables(dataset = dataset, index_date = current_vax.date, var_name_suffix = suffix)

    previous_vax_date = current_vax.date
    
