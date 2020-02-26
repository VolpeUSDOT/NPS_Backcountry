---
output:
  html_document: default
  pdf_document: default
---

# NPS_Backcountry

Analysis of dayhike versus overnight hike Noise and Natural Sounds work for NPS/FAA.

Built from the `2016_2017_Analysis/Overnight/` directory on the Volpe project server.

All necessary code and data are provided to create the output reported in `Multi-day Dose-Response 2017-06-07.docx`.

To facilitate running this code locally, use RStudio and launch the code by double-clicking `NPS_Backcountry.Rproj`. This will ensure the working directory is set to the location of this project.

# Data

#### Input 

Raw files by park, site, and survey are individual `.csv` files in the directory Data/Input.

#### Compiled

- ATMP2011_CompleteDose_Vars_dprime.csv

- DRMerged2011subset_dprime.csv

# Scripts

#### 1. `ATMP_2011DataProcess_Dprime.R`
This prepares the compiled data files from the original raw input files. Provided for completeness, not necessary to run since compiled data files are also provided.

#### 2. `RunModelScript_Manuscript_CoeffOnly_Overnight.R`
This is a master script which calls the multiple different types of individual modeling scripts, with different inputs depending on which combinations of predictors is used. 47 models are run.

To navigate among the different models being run, in RStudio use Code > Show Document Outline.

The output of this script is 94 files, with an output file containing the coefficients and p-values (`*_CoeffProbs.csv`) and model performance by AIC (`*_CoeffAIC.csv`) for each model.
Finally, this script runs a model comparison script which evaluates all models by AIC and selects the best model for each of the three levels of Annoy and Interfere responses.

Scripts called by this master script are the following:

- `ATMP_2011Overnight_LogRegPEn_Annoy_ACHR1HR2_CoeffOnly-nosurvey.R`
- `ATMP_2011Overnight_LogRegPEn_Annoy_ACHR1HR2_CoeffOnly.R`
- `ATMP_2011Overnight_LogRegPEn_ImpCP_Annoy_ACHR1HR2.R`
- `ATMP_2011Overnight_LogRegPEn_Interfere_ACHR1HR2_CoeffOnly-nosurvey.R`
- `ATMP_2011Overnight_LogRegPEn_Interfere_ACHR1HR2_CoeffOnly.R`
- `ATMP_2011Overnight_LogRegPEn_Interfere_CoeffOnly.R`
- `Model_Setup_Overnight.R`
- `Model_Selection_Overnight.R`

The script may take several minutes to complete.

#### 3. Plotting and final tables

- `Plot_Overnight_Descriptors.R`
This script generates the descriptive tables and figures in the report.

- `Plot_Run_Final_Overnight_Models.R`
This script generates the logistic curves from the best fit models for dayhike and overnight.
It also produces the inputs for the final model fit tables, tables 4 and 5 in the report. These are generated in the `tab_model()` commands for each of the six final models.
