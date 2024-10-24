# Purpose:
# define codelist objects from codelist files imported by codelist.txt spec

# Import code building blocks from cohort extractor package
from ehrql import codelist_from_csv


## --VARIABLES--
# if the variable uses a codelist then it should be added below
# after updating the codelist.txt configuration and importing the codelist

# Ethnicity

ethnicity_codelist5 = codelist_from_csv(
  "codelists/opensafely-ethnicity-snomed-0removed.csv",
  column="code",
  category_column="Label_6", # it's 6 because there is an additional "6 - Not stated" but this is not represented in SNOMED, instead corresponding to no ethnicity code
)

ethnicity_codelist16 = codelist_from_csv(
  "codelists/opensafely-ethnicity-snomed-0removed.csv",
  column="code",
  category_column="Label_16",
)

