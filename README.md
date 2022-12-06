## Reconstructing subdistrict-level population denominators in Yemen after six years of armed conflict and forced displacement
### Explanation of R scripts and input datasets
Checchi & Koum-Besson (2022) https://www.sciencedirect.com/science/article/pii/S2666623522000289?via%3Dihub
-----------------------

17 December 2021

Francesco Checchi (francesco.checchi@lshtm.ac.uk)

Funding: United Kingdom Foreign, Commonwealth and Development Office

### Background
The London School of Hygiene and Tropical Medicine (LSHTM) has been conducting a project to estimate crisis- and COVID-19-attributable mortality in Yemen. In the context of this project, we wished to estimate the population of Yemen at subdistrict (administrative level 3) and monthly resolution, over the period June 2014 to September 2021. These population denominators are needed to estimate death rates and tolls, but, more widely, may be useful for humanitarian service planning in a settings where vital events registration is not reliable, and the last census was conducted in 2004.

This repository contains R scripts and input datasets with which to replicate the analysis to reconstruct Yemen's population.

### Description of input datasets
The repository contains the following input data files:

* `yem_demog_data.xslx` , which contains analysis parameters, yearly population estimates by WorldPop and internal displacement datasets collected by the International Organisation for Migration and the United Nations High Commissioner for Refugees: the latter include data on internally displaced persons (IDPs) and returnees (former IDPs), and are further broken down into 'prevalent' (i.e. point-in-time assessments of existing IDPs/returnees) and 'incident' (i.e. records of new displacement flows); we hereafter refer to diplacement data as the Displacement Tracking Matrix (DTM) dataset.
* `yem_pop_other_data.zip`, which should be unzipped to the same directory as all other files / scripts. It contains the following:
  * `yem_gazetteers.xlsx`, which contains both the UN OCHA and Yemen Central Statistical Organisation (CSO) gazetteers (directories) of place names from administrative level 1 down to locality;
  * `yem_insecurity_data.csv`, which contains data on conflict fatalities and insecurity events collected by the ACLED project (see https://acleddata.com/research-hub-war-in-yemen/);
  * a `health_data` subdirectory, which contains a shape file of crowd-sourced health facility locations, published by the Humanitarian OpenStreetMap Team;
  * a `transport_data` subdirectory, which contains a shape file of major roads in Yemen, published by the CSO;
  * a `mapping` subdirectory, which contains UN OCHA shape files for administrative boundaries.

### Description of R scripts
The repository contains several R scripts, numbered in the order in which they should be run in order to replicate the analysis:
* `yem_pop_0_control_script.R` sets general parameters, loads or installs required packages, reads files and calls all other scripts;
* `yem_pop_0_user_functions.R` contains several user-defined functions used in later scripts;
* `yem_pop_1_clean_data_various.R` performs cleaning and management for all input datasets other than DTM data;
* `yem_pop_2_clean_data_displacement.R` performs initial cleaning and management for DTM data; 
* `yem_pop_3_equivalence_gazetteers.R` maps all place names in the DTM datasets to official OCHA and CSO gazetteers;
* `yem_pop_4_missing_subdistricts_matching.R` performs four successive data management / text analysis steps to reduce missingness in the DTM dataset in terms of subdistricts of origin and arrival of IDPs;
* `yem_pop_5_missing_subdistricts_models.R` addresses DTM subdistrict missingness further by implementing several machine learning imputation models, and preparing the imputed dataset for further analysis;
* `yem_pop_6_prepare_displacement_flows.R` explores the DTM dataset and fits a generalised additive model to predict the evolution of IDP groups as a function of time since displacement: this model is then applied to both prevalent and incident DTM data to predict IDP figures by subdistrict or origin, arrival and month;
* `yem_pop_7_reconstruct_population.R` reconstructs populations by subdistrict-month based on a main analysis as well as reasonable-high and reasonable-low sensitivity scenarios; this is done by aggregating DTM, WorldPop and geographic datasets, and preparing assumptions for the evolution of crude birth and death rates in both urban and rural subdistricts.

As R scripts are run, they produce intermediate and final output files, as well as graphs.

### How to replicate the analysis
Analysts should make sure all files, scripts and subdirectories are saved to the same folder, where output files will also appear. The directory for reading files is set automatically when `yem_pop_0_control_script.R` is run. An updated version of R software should be installed (https://www.r-project.org/). It is also recommended to run the code from the open-source RStudio interface (https://www.rstudio.com/products/rstudio/download/). Both R and RStudio are free and open-source.

Depending on the computing equipment used, some scripts will run slowly. In particular, `yem_pop_5_missing_subdistricts_models.R` and `yem_pop_6_prepare_displacement_flows.R` take hours to run on a standard, cheap laptop (but probably much less time on a more powerful machine, and a fraction of that on a computing cluster). They both also generate very large files (>1 GB), thus requiring a lot of free memory; in our case, scripts were run off an external hard drive and yet still sometimes crashed if too many other applications were running.



