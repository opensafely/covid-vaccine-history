# #######################################################################################
# # Common functions for contructing clinical queries
# #######################################################################################

from ehrql.codes import CTV3Code, ICD10Code

from ehrql import case, days, when

from codelists import *

from ehrql.tables.core import (
  medications,
  patients
)

from ehrql.tables.tpp import (
  addresses,
#  opa_cost,
  clinical_events,
  practice_registrations,
# appointments,
# vaccinations
)


#######
# Clinical functions
########

# events occurring before spec date
# prior_events = clinical_events.where(clinical_events.date.is_on_or_before(index_date))

# query prior_events for existence of event-in-codelist
def has_prior_event(codelist, index_date, where=True):
    prior_events = clinical_events.where(clinical_events.date.is_on_or_before(index_date))
    return (
        prior_events
        .where(where)
        .where(prior_events.snomedct_code.is_in(codelist))
        .exists_for_patient()
    )

# query prior_events for date of most recent event-in-codelist
def last_prior_event(codelist, index_date, where=True):
    prior_events = clinical_events.where(clinical_events.date.is_on_or_before(index_date))
    return (
        prior_events.where(where)
        .where(prior_events.snomedct_code.is_in(codelist))
        .sort_by(clinical_events.date)
        .last_for_patient()
    )

# query prior_events for date of earliest event-in-codelist
def first_prior_event(codelist, index_date, where=True):
    prior_events = clinical_events.where(clinical_events.date.is_on_or_before(index_date))
    return (
        prior_events.where(where)
        .where(prior_events.snomedct_code.is_in(codelist))
        .sort_by(clinical_events.date)
        .first_for_patient()
    )

# meds occurring before spec date

# query prior_meds for existence of event-in-codelist
def has_prior_meds(codelist, index_date, where=True):
    prior_meds = medications.where(medications.date.is_on_or_before(index_date))
    return (
        prior_meds.where(where)
        .where(prior_meds.dmd_code.is_in(codelist))
        .exists_for_patient()
    )
    
# query prior meds for date of most recent med-in-codelist
def last_prior_meds(codelist, index_date, where=True):
    prior_meds = medications.where(medications.date.is_on_or_before(index_date))
    return (
        prior_meds.where(where)
        .where(prior_meds.dmd_code.is_in(codelist))
        .sort_by(medications.date)
        .last_for_patient()
    )

# query prior_events for date of earliest event-in-codelist
def first_prior_meds(codelist, index_date, where=True):
    prior_meds = medications.where(medications.date.is_on_or_before(index_date))
    return (
        prior_meds.where(where)
        .where(prior_meds.dmd_code.is_in(codelist))
        .sort_by(medications.date)
        .first_for_patient()
    )

# BMI
#def most_recent_bmi(*, minimum_age_at_measurement, where=True):
#    events = schema.clinical_events
#    age_threshold = schema.patients.date_of_birth + days(
#        # This is obviously inexact but, given that the dates of birth are rounded to
#        # the first of the month anyway, there's no point trying to be more accurate
#        int(365.25 * minimum_age_at_measurement)
#    )
#    return (
#        # This captures just explicitly recorded BMI observations rather than attempting
#        # to calculate it from height and weight measurements. Investigation has shown
#       # this to have no real benefit it terms of coverage or accuracy.
#        events.where(events.ctv3_code == CTV3Code("22K.."))
#        .where(events.date >= age_threshold)
#        .where(where)
#        .sort_by(events.date)
#        .last_for_patient()
#    )


## functions to define variables across  multiple study definitions

# demogrpahic variables
def demographic_variables(dataset, index_date, var_name_suffix=""):
    registration = practice_registrations.for_patient_on(index_date)
    setattr(dataset, f"age{var_name_suffix}", patients.age_on(index_date))
    setattr(dataset, f"region{var_name_suffix}", registration.practice_nuts1_region_name)
    setattr(dataset, f"stp{var_name_suffix}", registration.practice_stp)
    setattr(dataset, f"imd{var_name_suffix}", addresses.for_patient_on(index_date).imd_rounded)

# PRIMIS variables
def primis_variables(dataset, index_date, var_name_suffix=""):
    setattr(dataset, f"chd{var_name_suffix}", has_prior_event(chd_cov, index_date))
    setattr(dataset, f"cld{var_name_suffix}", has_prior_event(cld, index_date))
    ## more primis variables to go here!
