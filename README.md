## Bayesian TB incident contact investigation costing

This repo contains the R code to calculate TB outbreak costing using data from incidents in Birmingham, UK.

### Quick start

- Clone the repository:

```
git clone https://github.com/n8thangreen/tb.outbreak.costing.git
```

- Install dependencies (example):

  - R (>= 4.0)
  - `devtools`
  - `dplyr`, `purrr`, `ggplot2`, `reshape2`, `here`
  - `R2WinBUGS` / `R2OpenBUGS` or `rjags` (if running BUGS models)

  In R:

```r
install.packages(c("devtools", "dplyr", "purrr", "ggplot2", "reshape2", "here"))
# then load and install the package for development
devtools::load_all(".")
```

### Overview
#### Background
Tuberculosis remains an import public health challenge in England, despite 7 years of declining cases.
In 2018 there were 4,655 TB cases in England (8.3 per 100,000 population).

#### Contact tracing
Contact tracing of individuals exposed to infectious active TB cases is an important component of TB control, highlighted in the the Public Health England / National Health Service England Collaborative.
The purpose of contact tracing is to identify recently-infected individuals as rapidly as possible to reduce morbidity and mortality in those who have developed active TB disease and reduce transmission.
The majority of contact tracing is small-scale, typically involving a small number of household and social contacts.
However, there are occasions when large-scale incident responses are required.

#### Cost analyses
To ensure appropriate resource allocation it is necessary to assess the costs and effectiveness of public health interventions, and TB incident investigation has the potential to be particularly costly.
There have been recent analyses of routine TB contact tracing in England, but analysis of incident responses is currently lacking.
To address this important evidence gap, we analysed data from Birmingham and Solihull TB Service, whose geographic area has a three year average of 177 TB cases (15.1 per 100,000 population). 

### Related work
#### Excel model
Originally this model was implemented in Excel and VBA [here](https://github.com/n8thangreen/tb_incident_contact_tracing_costing) but, in order to further develop it, it was rewritten in to R.

#### Shiny app
A Shiny app running this model can be accessed [here](https://n8thangreen.shinyapps.io/incidentCostingShiny/).
The GitHub repo for this is [here](https://github.com/n8thangreen/incidentCostingShiny).

### Folder structure

Folder | Purpose
---|---
[`output_data`](output_data/) | Results data
[`BUGS`](BUGS/) | BUGS code and R script to run it

### Files and scripts of interest
- `R/config.R`: package configuration and parameter loading.
- `scripts/model_data.R`: loads parameters and computes derived values for analysis and outputs param_vals.csv for reference.
- `scripts/posterior_predictive_analysis.R`: runs posterior predictive simulations and compares expected-value and posterior predictive approaches.
- `inst/extdata/parameters.csv`: central parameter table used by load_parameters().
- `input_data/`: cleaned and processed input datasets and BUGS output files.

### Reproducing results
- Most analyses are driven from the `scripts/` directory and the `BUGS/` folder. To reproduce the posterior predictive analysis run:

  1. Ensure required R packages are installed.
  2. Generate or place BUGS output at `input_data/BUGS_output.RData` (or run the BUGS model in `BUGS/`).
  3. From the project root in R: `source("scripts/model_data.R")` and then run `scripts/posterior_predictive_analysis.R` (or source it).

### Data
- `input_data/cleaned_data.csv`: cleaned incident data used for model fitting and analysis.
- `input_data/BUGS_output.RData`: BUGS model output (required for posterior predictive analysis).
- `inst/extdata/parameters.csv`: the master parameter table used by `R/config.R` and `scripts/model_data.R`.

### Usage notes
- The package provides a helper `load_parameters()` to load default parameter values from `inst/extdata/parameters.csv` into an environment; `scripts/model_data.R` uses this to populate the global environment when run interactively.
- Scripts often rely on global variables; running scripts via `source()` (as used in this project) will create those variables in the calling environment.

### Contributing
Contributions, issues and feature requests are welcome. Please open an issue describing the change or a pull request with proposed fixes and tests where appropriate.

### License
This repository's license should be added here. If none is present, please add a suitable license (e.g., MIT, GPL-3) via a LICENSE file.

### Contact
For questions about the model or data, open an issue or contact the repository owner: n8thangreen on GitHub.
