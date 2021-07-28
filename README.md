## TB Outbreak Costing

This repo contains the R code to calculate TB outbreak costing using data from incidents in Birmingham, UK.

### Overview
#### Background
Tuberculosis remains an import public health challenge in England, despite 7 years of declining cases.
In 2018 there were 4,655 TB cases in England (8.3 per 100,000 population).

#### Contact tracing
Contact tracing of individuals exposed to infectious active TB cases is an important component of TB control, highlighted in the the Public Health England / National Health Service England Collaborative TB strategy 2015-2020 and World Health Organization (WHO) End TB Strategy.
The purpose of contact tracing is to identify recently-infected individuals as rapidly as possible to reduce morbidity and mortality in those who have developed active TB disease and reduce transmission from those who are infectious, and to offer preventative treatment to those with latent TB infection (LTBI) to prevent progression to active disease.
The majority of contact tracing is small-scale, typically involving a small number of household and social contacts.
However, there are occasions when large-scale incident responses are required.

#### Cost analyses
To ensure appropriate resource allocation it is necessary to assess the costs and effectiveness of public health interventions, and TB incident investigation has the potential to be particularly costly as it is labour-intensive.
There have been recent analyses of routine TB contact tracing in England, but analysis of incident responses is currently lacking.
To address this important evidence gap, we analysed data from Birmingham and Solihull TB Service, whose geographic area has a three year average of 177 TB cases (15.1 per 100,000 population), to determine the costs and yield of TB incident investigations performed in 2013-2018 and investigate reasons for any variation .

### Related work
#### Excel model
Originally this model was implemented in Excel and VBA [here](https://github.com/n8thangreen/tb_incident_contact_tracing_costing) but, in order to further develop it, it was rewritten in to R.

#### Shiny app
A Shiny app running this model can be accessed [here](https://n8thangreen.shinyapps.io/incidentCostingShiny/).
The GitHub repo for this is [here](https://github.com/n8thangreen/incidentCostingShiny).
