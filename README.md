# Mentally_green
## CASA0010_Dissertation
 - The dataset modelled in this study comes from HES – Admitted Patient Care and was accessed via the Data Access Request Service (DARS) under the project reference number DARS-NIC-28051-Q3K7L. HES is a data warehouse containing all admissions, outpatient appointments and A&E attendances at NHS hospitals in England. Admitted patient records were obtained for the period from April 1999-March 2014. Ethical approval was obtained from Bromley Research Ethics Committee (Reference: 13/LO/1355). All data exploration and analysis were conducted in the UCL Data Safe Haven to ensure that the data was stored safely, which uses a walled garden approach. 

- The Hospital Episodes Statistics data that is the focus of this analysis is not exportable, but the aim of this repository is to enable those with access to the data or other dataset to reproduce this.

The cornwall.R and london.R files read in the results of the models computed in the DSH. 
The ucl__dsh_exports folder contains the scripts that can be followed in the event that you access the data, or anything that can be similarly modelled. ldn.rds and cornwall.rds are the exported results (with sensitive data removed).
The green_covariates_maps.R file displays some static maps explored during the exploratory analysis.

Data Sources:
- The London and Cornwall LSOA Boundaries can be downloaded from: https://geoportal.statistics.gov.uk/maps/b7c49538f0464f748dd7137247bbc41c/about
- The Access to Healthy Assets & Hazards (AHAH) data set can be downloaded from: https://data.cdrc.ac.uk/dataset/access-healthy-assets-hazards-ahah
- Each of the 2011 Census sociodemographic variables (resident BAME population %, Unemployment rate %, Households living in socially rented housing %, 2011 Population Density) used can be downloaded from: https://www.nomisweb.co.uk/census/2011/quick_statistics 
