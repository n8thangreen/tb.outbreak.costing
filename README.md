## Bayesian TB incident contact investigation costing

[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)

This repo contains the R code to calculate TB outbreak costing using data from incidents in Birmingham, UK.

### Quick start

- Clone the repository:

```bash
git clone https://github.com/n8thangreen/tb.outbreak.costing.git
```

- Install dependencies and package:

  In R:

```r
# Install development package tools and dependencies
install.packages(c("devtools", "dplyr", "purrr", "ggplot2", "reshape2", "here", "gridExtra", "tidybayes"))

# Load package for development
devtools::load_all(".")

# Or install package locally
devtools::install()
```

### Overview
#### Background
Tuberculosis remains an important public health challenge in England, despite 7 years of declining cases.
In 2018 there were 4,655 TB cases in England (8.3 per 100,000 population).

#### Contact tracing
Contact tracing of individuals exposed to infectious active TB cases is an important component of TB control, highlighted in the Public Health England / National Health Service England Collaborative.
The purpose of contact tracing is to identify recently-infected individuals as rapidly as possible to reduce morbidity and mortality in those who have developed active TB disease and reduce transmissi[...]
The majority of contact tracing is small-scale, typically involving a small number of household and social contacts.
However, there are occasions when large-scale incident responses are required.

#### Cost analyses
To ensure appropriate resource allocation it is necessary to assess the costs and effectiveness of public health interventions, and TB incident investigation has the potential to be particularly costl[...]
There have been recent analyses of routine TB contact tracing in England, but analysis of incident responses is currently lacking.
To address this important evidence gap, we analysed data from Birmingham and Solihull TB Service, whose geographic area has a three year average of 177 TB cases (15.1 per 100,000 population). 

### Related work
#### Excel model
Originally this model was implemented in Excel and VBA [here](https://github.com/n8thangreen/tb_incident_contact_tracing_costing) but, in order to further develop it, it was rewritten into R.

#### Shiny app
A Shiny app running this model can be accessed [here](https://n8thangreen.shinyapps.io/incidentCostingShiny/).
The GitHub repo for this is [here](https://github.com/n8thangreen/incidentCostingShiny).

### Folder structure

Folder | Purpose
---|---
[`R`](R/) | Package source R functions and configuration
[`inst/extdata`](inst/extdata/) | Central parameter configuration CSV files
[`scripts`](scripts/) | Analysis, plotting, and costing scripts
[`BUGS`](BUGS/) | BUGS model code and fitting scripts
[`input_data`](input_data/) | Processed input datasets, parameter values, and BUGS output files
[`output_data`](output_data/) | Model output and simulation results data
[`plots`](plots/) | Generated figures and visualization outputs

### Files and scripts of interest
- `R/config.R`: Package configuration and parameter loading logic (`load_parameters()` / `load_parameter()`).
- `scripts/model_data.R`: Loads parameters and computes derived cost variables for analysis, outputting `param_vals.csv` for reference.
- `scripts/costs_with_BUGS.R`: Runs probabilistic sensitivity analysis and total costing using BUGS output.
- `scripts/posterior_predictive_analysis.R`: Runs posterior predictive simulations and compares expected-value and posterior predictive approaches.
- `inst/extdata/parameters.csv`: Central parameter table used by `load_parameters()`.
- `input_data/`: Cleaned and processed input datasets and BUGS output files.

### Reproducing results
- Most analyses are driven from the `scripts/` directory and the `BUGS/` folder. To reproduce the posterior predictive analysis:

  1. Ensure required R packages are installed.
  2. Generate or place BUGS output at `input_data/BUGS_output.RData` (or run the BUGS model in `BUGS/`).
  3. From the project root in R: `source("scripts/model_data.R")` and then run `scripts/costs_with_BUGS.R` or `scripts/posterior_predictive_analysis.R`.

### Data
- `input_data/cleaned_data.csv`: Cleaned incident data used for model fitting and analysis.
- `input_data/BUGS_output.RData`: BUGS model output (required for BUGS costing and posterior predictive analysis).
- `inst/extdata/parameters.csv`: The master parameter table used by `R/config.R` and `scripts/model_data.R`.

### Usage notes
- The package exports helper functions `load_parameters()` (and `load_parameter()`) to load default parameter values from `inst/extdata/parameters.csv` into a specified environment; `scripts/model[...]
- Scripts often rely on global variables; running scripts via `source()` (as used in this project) will create those variables in the calling environment.

### Contributing
Contributions, issues and feature requests are welcome. Please open an issue describing the change or a pull request with proposed fixes and tests where appropriate.

### License
This repository is licensed under the GNU General Public License v3.0 (GPL-3.0). See the [LICENSE](LICENSE) file for details.

### Contact
For questions about the model or data, open an issue or contact the @n8thangreen on GitHub.
