# covid-vaccine-history

[View on OpenSAFELY](https://jobs.opensafely.org/repo/https%253A%252F%252Fgithub.com%252Fopensafely%252Fcovid-vaccine-history)

Details of the purpose and any published outputs from this project can be found at the link above.

The contents of this repository MUST NOT be considered an accurate or valid representation of the study or its purpose. 
This repository may reflect an incomplete or incorrect analysis with no further ongoing work.
The content has ONLY been made public to support the OpenSAFELY [open science and transparency principles](https://www.opensafely.org/about/#contributing-to-best-practice-around-open-science) and to support the sharing of re-usable code for other subsequent users.
No clinical, policy or safety conclusions must be drawn from the contents of this repository.

# About the OpenSAFELY framework

The OpenSAFELY framework is a Trusted Research Environment (TRE) for electronic
health records research in the NHS, with a focus on public accountability and
research quality.

Read more at [OpenSAFELY.org](https://opensafely.org).

# Licences
As standard, research projects have a MIT license. 


# Project details

This repository contains code to describe patterns of COVID-19 vaccine receipt in England since December 2020. The are two key strands:

1. Describing vaccine receipt -- dates and products -- over time across multiple vaccination campaigns, stratified by demographic and clinical characteristics;
2. Describing vaccine receipt within each campaign (now settled into a regular Spring and Autumn rhythm) stratified by demographic, clinical, and prior COVID-19 vaccine characteristics.

## Repository navigation

- The [`codelists/`](./codelists/) directory contains all the codelists used to define variables in analysis. 
- The [`analysis/`](./analysis) directory contains the executable scripts used to conduct the analysis. 
- The [`project.yaml`](./project.yaml) defines run-order and dependencies for all the analysis scripts.
- Non-disclosive model outputs, including tables, figures, etc, are available via the OpenSAFELY job server.


## Analysis scripts

The analysis scripts in the [`analysis/`](./analysis) directory are organised into sub-directories as follows:

- [`0-lib/`](./analysis/0-lib/):
  - [`design.R`](./analysis/0-lib/design.R) defines the design configurations used throughout the study - start and end dates, eligibility, products, look-up dictionaries, etc.
  It also defines R functions used throughout the codebase.
  This script is run at the start of all relevant R scripts. 
  It must also be run manually if values in either of the [`study_dates.json`](./analysis/0-lib/study_dates.json) or [`campaign_info.json`](./analysis/0-lib/campaign_info.json) files need to be updated, 
  so that the ehrQL scripts use the most up-to-date values too. 
- [`1-extract/`](./analysis/1-extract/):
  - [`dataset_definition_fixed.py`](./analysis/1-extract/dataset_definition_fixed.py) is the ehrQL script for selecting all variables that are fixed (e.g., date of death), or assumed to be fixed (e.g., ethnicity), as at the study end date.
  This information is extracted once and then joined onto other datasets where needed, to save computation time.
  - [`dataset_definition_varying.py`](./analysis/1-extract/dataset_definition_varying.py) is the ehrQL script to extract information as at the time of each COVID-19 vaccine event.
  The outputted dataset includes a set of columns for each vaccination event, with one column for each variable of interest (e.g., vaccination date, product, GP practice, deprivation level, other clinical characteristics).
  For instance, region_i (region_1, region_2, region_3, ...) represents the region of the person's registered address at the time of the i-th COVID-19 vaccine. 
  This may be modified in future to use ehrQL's new event-level data features.
  - [`dataset_definition_snapshot.py`](./analysis/1-extract/dataset_definition_snapshot.py) is the ehrQL script to extract information as at the start of a given vaccine campaign, for anyone alive and registered on the snapshot date. 
  `snapshot_date` is a parameter supplied to this script in the project.yaml file, representing the start date of the campaign of interest, which must be supplied in the format `YYYYMMDD`.
  - [`dummy_dataset_definition_....R`](./analysis/1-extract/) are the corresponding R scripts that create custom dummy data for each of the 3 dataset definitions. 
  Custom dummy data is used instead of the dummy data created by ehrQL as it provides more control over the structure in the data, such as more realistic vaccination dates or event rates.
  If a dataset definition is updated, the dummy dataset definition script must also be updated to ensure all variable names and types match.
  - [`variables.py`](./analysis/1-extract/variables_function.py) contains some function and variable definitions to be read in by the dataset definition.
  - [`codelist.py`](./analysis/1-extract/codelists.py) pulls the codelists from the [`codelists/`](./codelists/) directory to be usable in the dataset definition. 
- [`2-prepare/`](./analysis/2-prepare/):
  - [`prepare.R`](./analysis/2-prepare/prepare.R) this script imports the extracted database data (or dummy data) from the `fixed` and `varying` datasets, tidies some variables, derives some new ones, and and reshapes the time-varying data to be one-row-per-vaccine.
- [`3-history/`](./analysis/3-history/)
  - [`report_history.R`](./analysis/3-history/report_history.R) collects all processed vaccination data and reports vaccine counts and products, by week, over the observation period, across a number of subgroups.
- [`4-snapshot/`](./analysis/4-snapshot/)
  - [`report_snapshot.R`](./analysis/4-snapshot/report_snapshot.R) reports vaccine history for people on the start of a given vaccine campaign, and subsequent vaccine coverage within this campaign. 
  It imports the vaccination data from the `prepare.R` script, processes date-specific info from the `dataset_definition_snapshot.py` script, combines them, then produces a collection of outputs about the eligible vaccine population and subsequent coverage on that date. 

## Output files

The structure of the output directory (not tracked via git) is designed to match the structure of the [analysis directory](./analysis). For example, all files created by the action `report_snapshot_20210906`, which runs the [`./output/4-snapshot/report_snapshot.R`](./analysis/4-snapshot/report_snapshot.R) script for `snapshot_date=20210906`, will be saved in the `./output/4-snapshot/report_snapshot_20210906/` directory. 

## Adding variables

To add a new variable for stratifying analyses, searching for the `--VARIABLES--` string in the codebase (ctrl+shift+F in RStudio on Windows) will indicate where changes may need to be made.
