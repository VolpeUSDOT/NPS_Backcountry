## Merge 1990s and 2000s data files for grand analyis proposed by Kurt in spring 2020.
## Also include Rainbow Bridge
## Modified from ATMP_2011DataProcess_Dprime.R


# <<>><<>><<>><<>><<>><<>><<>><<>><<>><<>>
# Setup ---- 
rm(list = ls())	

source("get_packages.R")

library(tidyverse)
library(readxl)


project_shared_drive = "//vntscex/DFS/Projects/PROJ-VXK600/MLB48"

dat_1990 = '1990s FC Database/1990_Master_Survey_Response_Calculated_Dose_Database_GSAMods_14Sep09.xls'

dat_2000 = '2016_2017_Analysis/Overnight/Standalone_2020/NPS_Backcountry/Data/DRMerged2011subset_dprime.csv'
  
dat_RABR = '2014_RABR/RABR Processing/DATABASE/BCsub_CompleteDoseVars.csv'


# read in files

d90 = read_xls(file.path(project_shared_drive, dat_1990))

d00 = read_csv(file.path(project_shared_drive, dat_2000))

dRB = read_csv(file.path(project_shared_drive, dat_RABR))

# <<>><<>><<>><<>><<>><<>><<>><<>><<>><<>>
# Merge ----

# Establish data columns present in all data sets

all_vars = unique(c(names(d90),
                    names(d00),
                    names(dRB)))


# P/A table by data set

var_presence = data.frame(all_vars)

var_presence = var_presence %>%
  mutate(`In 1990s` = all_vars %in% names(d90),
         `In 2000s` = all_vars %in% names(d00),
         `In RABR` = all_vars %in% names(dRB),
         `In all` = `In 1990s` == TRUE & `In 2000s` == TRUE & `In RABR` == TRUE)




# <<>><<>><<>><<>><<>><<>><<>><<>><<>><<>>
# Subset to complete cases ----

# complete.dose <- c("Site", "LmaxAllAC", "SELAllAC", "PTAudAllAC", "LeqTresp", "LeqTAC", "L50NatQuiet", "PEnHelos", "PEnProps", "SiteType", "ImpNQ_VorMore", "ImpHC_VorMore", "ImpVS_VorMore", "ImpCP_VorMore",  "SiteVisitBefore", "AdultsOnly", "Survey", "Annoy_SorMore", "Annoy_MorMore", "Annoy_VorMore", "EarlyStart", "Talk", "HikeBeginMinAfterMidnt")
# 
# dose.cols <- DRMerged2011sub[,complete.dose]
# dose.rows <- complete.cases(dose.cols)
# length(dose.rows)
# 
# DRMerged2011subComplete <- DRMerged2011sub[dose.rows==TRUE,]
# dim(DRMerged2011subComplete)
# table(DRMerged2011subComplete$Survey)
# table(DRMerged2011subComplete$Survey,DRMerged2011subComplete$HearAircraft)
# 
# write.csv(DRMerged2011subComplete, file = file.path(datadir, "ATMP2011_CompleteDoseVars_dprime.csv"), row.names = FALSE)
# 
