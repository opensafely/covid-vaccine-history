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

# Repository navigation


## R scripts

#### Pre-server scripts
These scripts are run locally, as they do not depend on any data extracted from the database. 
- [`dummydata_....R`](./analysis/) these dummy scripts generate custom-made dummy datasets for their respective dataset definitions written in ehrQL. This is used instead of the usual dummy data specified in the dataset definition because it is then possible to impose some more useful structure in the data, such as more realistic vaccination schedules and events that occur in the expected order. If the dataset definition is updated, this script must also be updated to ensure variable names and types match. 
- [`utility.R`](./analysis/utility.R) contains useful functions, look-up tables, and design configurations for the study, and is run towards the start of each R script. 

#### On-server scripts
These scripts extract or use data in the OpenSAFELY-TPP server.
- [`analysis/dataset_definition_fixed.py`](analysis/dataset_definition_fixed.py) extracts variables that are fixed (or assumed to be fixed) over time, as at the end of the observation period. 
- [`analysis/dataset_definition_varying.py`](analysis/dataset_definition_varying.py) extracts vaccination dates and types for all vaccines received up to the end of the observation period. It also extracts recipient characteristics as at the date of each vaccination, such as age and region of GP practice for the first, second, ..., nth vaccination.
- [`analysis/dataset_definition_snapshot.py`](analysis/dataset_definition_snapshot.py) extracts characteristics of people alive and registered on the snapshot date. `snapshot_date` is a parameter supplied to this script in the project.yaml file.
- [`process.R`](./analysis/process.R) imports the extracted data from the `fixed` and `varying` datasets, standardises them, and reshapes the time-varying data to be one-row-per-vaccine.
- [`report_history.R`](./analysis/report_history.R) collects all processed vaccination data and reports vaccine counts and type, by week, over the observation period, across a number of subgroups.
- [`report_snapshot.R`](./analysis/report_snapshot.R) reports vaccine history for individuals on the specific snapshot date. It imports the vaccination data from the `process.R` script, processes date-specific info from the `dataset_definition_snapshot.py` script, combines them, then produces a collection of outputs about vaccine histories on that date. 

## Adding variables

To add a new variable for stratifying analyses, searching for the `--VARIABLES--` string in the codebase (ctrl+shift+F in RStudio on Windows) will indicate where changes may need to be made.
