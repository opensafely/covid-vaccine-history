# Purpose:
# define codelist objects from codelist files imported by codelist.txt spec

# Import code building blocks from cohort extractor package
from ehrql import codelist_from_csv


# Ethnicity

ethnicity_codelist5 = codelist_from_csv(
  "codelists/opensafely-ethnicity-snomed-0removed.csv",
  column="snomedcode",
  category_column="Grouping_6", # it should be called "grouping_5" but it's not!
)


ethnicity_codelist16 = codelist_from_csv(
  "codelists/opensafely-ethnicity-snomed-0removed.csv",
  column="snomedcode",
  category_column="Grouping_16",
)

