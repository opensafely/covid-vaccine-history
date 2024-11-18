# #######################################################################################
# # Common functions for contructing clinical queries
# #######################################################################################

from ehrql.codes import CTV3Code, ICD10Code

from ehrql import case, days, when

from ehrql.tables.core import (
  medications,
  patients
)

from ehrql.tables.tpp import (
#  addresses,
#  opa_cost,
  clinical_events,
# practice_registrations,
# appointments,
# vaccinations
)


#######
# Clinical functions
########

# events occurring before spec date
# prior_events = clinical_events.where(clinical_events.date.is_on_or_before(spec_date))

# query prior_events for existence of event-in-codelist
def has_prior_event(codelist, spec_date, where=True):
    prior_events = clinical_events.where(clinical_events.date.is_on_or_before(spec_date))
    return (
        prior_events
        .where(where)
        .where(prior_events.snomedct_code.is_in(codelist))
        .exists_for_patient()
    )

# query prior_events for date of most recent event-in-codelist
def last_prior_event(codelist, spec_date, where=True):
    prior_events = clinical_events.where(clinical_events.date.is_on_or_before(spec_date))
    return (
        prior_events.where(where)
        .where(prior_events.snomedct_code.is_in(codelist))
        .sort_by(clinical_events.date)
        .last_for_patient()
    )

# query prior_events for date of earliest event-in-codelist
def first_prior_event(codelist, spec_date, where=True):
    prior_events = clinical_events.where(clinical_events.date.is_on_or_before(spec_date))
    return (
        prior_events.where(where)
        .where(prior_events.snomedct_code.is_in(codelist))
        .sort_by(clinical_events.date)
        .first_for_patient()
    )

# meds occurring before spec date

# query prior_meds for existence of event-in-codelist
def has_prior_meds(codelist, spec_date, where=True):
    prior_meds = medications.where(medications.date.is_on_or_before(spec_date))
    return (
        prior_meds.where(where)
        .where(prior_meds.dmd_code.is_in(codelist))
        .exists_for_patient()
    )
    
# query prior meds for date of most recent med-in-codelist
def last_prior_meds(codelist, spec_date, where=True):
    prior_meds = medications.where(medications.date.is_on_or_before(spec_date))
    return (
        prior_meds.where(where)
        .where(prior_meds.dmd_code.is_in(codelist))
        .sort_by(medications.date)
        .last_for_patient()
    )

# query prior_events for date of earliest event-in-codelist
def first_prior_meds(codelist, spec_date, where=True):
    prior_meds = medications.where(medications.date.is_on_or_before(spec_date))
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


# Asthma as per green book
# Poorly controlled asthma is defined as:
# - ≥2 courses of oral corticosteroids in the preceding 24 months OR
# - on maintenance oral corticosteroids OR
# - ≥1 hospital admission for asthma in the preceding 24 months

# From PRIMIS

