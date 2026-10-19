# Historical Ray Fishery Data from Northwestern Mexico (2000–2022)

## Overview

This repository contains historical fisheries data used to examine spatial and temporal patterns in ray landings in northwestern Mexico between 2000 and 2022.

The dataset supports descriptive analyses of reported landings by year, month, state, fishing office, and commercial ray group.

## Repository structure

### Data/
- `datos.csv`: Historical fisheries landing records.
- `coords.csv`: Geographic coordinates used for spatial representation.
- `oficinas.csv`: Reference information on fisheries landing offices.
- `data.rds`: Processed dataset containing 40,539 records and 13 variables, including geographic coordinates. Used by the Shiny application.

### Script/
### Script/
- `Map/app.R`: Shiny application for interactive visualization of historical ray landings.

Statistical analysis scripts are not included in this release.

## Data source and limitations

The data originate from administrative fisheries landing records (*avisos de arribo*) in Mexico.

Reported landings should not be interpreted as direct measures of species abundance or fishing effort.

## Data availability

The CSV files are publicly available in this repository to support transparency and further examination of the reported results.

The processed dataset and Shiny application code are also provided. Statistical analysis scripts are not currently available.

## Citation

When using these data, please cite the associated scientific publication, when available, and this repository:

https://github.com/v0ya/Rays_historical
