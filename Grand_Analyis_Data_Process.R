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


d90 <- d90[-1,] # Remove first non-header row, column numbers manually entered.

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

write.csv(var_presence, file = 'Vars_Compare_Grand.csv', row.names = F)

# Create necessary columns in 90s
# PEnHelos and Props from ATMP_2011DataProcess_Dp.r in 
# \\vntscex\DFS\Projects\PROJ-VXK600\MLB48\2016_2017_Analysis\EAS old analysis archives\Rwork\DprimeScripts
# But depends on HierSELHelos and HierSELProps, not named in 90s data. Per Amanda, use SELHelos and SELProps here.
# 'Hier' was a hierarchical method for if multiple sources available at the same time


d90 <- d90 %>%
  mutate(AdultsOnly = ifelse(NumbChildren < 1, TRUE, FALSE),
         Survey = 'HR0',
         lg10.PTAudAllAC = log10(PTAudAllAC),
         PEnHelos	= 100*((10^(SELHelos/10))/(10^(SELAllAC/10))),
         PEnProps	= 100*((10^(SELProps/10))/(10^(SELAllAC/10)))
         )  %>%
   mutate(PEnHelos = ifelse(is.na(SELHelos) & SELAllAC > 0, 0, PEnHelos),
          PEnHelos = ifelse(is.na(SELProps) & SELAllAC > 0, 0, PEnProps)
          )

# Filter out BackCty and PimaTr from 90s

d90 <- d90 %>%
  filter(SiteType != 'BackCty' & SiteType != 'Rim')

# Filter out all except for Rainbow Bridge from RB

dRB <- dRB %>% 
  filter(Site == 'RainbowBridge')


# <<>><<>><<>><<>><<>><<>><<>><<>><<>><<>>

# Merge ----

use_vars = c('Site', 'SiteType','SiteFirstVisit', 'Survey',
             'ImpNQ_VorMore',
             'ImpHistCult',
             'Annoy_VorMore',
             'Annoy_MorMore',
             'Annoy_SorMore',
             'IntWithNQ_VorMore',
             'IntWithNQ_MorMore',
             'IntWithNQ_SorMore',
             'LmaxAllAC',
             'SELAllAC',
             'PTAudHelos',
             'PTAudProps',
             'PTAudJets',
             'PTAudAllAC',
             'lg10.PTAudAllAC',
             'LeqHelos',
             'LeqProps',
             'LeqJets',
             'PEnHelos',
             'PEnProps',
             'AdultsOnly')


d90_use <- d90 %>% dplyr::select(all_of(use_vars))

d90_use <- d90_use %>%
  mutate(Dataset = '90s')

d00_use <- d00 %>% dplyr::select(all_of(use_vars))

d00_use <- d00_use %>%
  mutate(Dataset = '00s')

dRB_use <- dRB %>% dplyr::select(all_of(use_vars))

dRB_use <- dRB_use %>%
  mutate(Dataset = 'RB')


# NA in 1990's response variables ----
# What do NA values represent in 1990's data? 
d90 %>% filter(is.na(IntWithNQ_VorMore)) %>% group_by(Site) %>% summarize(n())

d90 %>% filter(is.na(Annoy_VorMore)) %>% group_by(Site) %>% summarize(n())

d90 %>% filter(is.na(Annoy_VorMore)) %>% select(Annoy)


# Compile ---
dAll <- rbind(d90_use, d00_use)
dAll <- rbind(dAll, dRB_use)

# Create 3-level ordinal variables for response. ----
# as.factor from character string of No and Yes: No = 1, Yes = 2. Subtract one and make numeric.
dAll <- dAll %>%
  filter(!is.na(Annoy_SorMore) & !is.na(IntWithNQ_VorMore)) %>%
  mutate(IntWithNQ_SorMore = as.numeric(as.factor(IntWithNQ_SorMore)) - 1,
         IntWithNQ_MorMore = as.numeric(as.factor(IntWithNQ_MorMore)) - 1,
         IntWithNQ_VorMore = as.numeric(as.factor(IntWithNQ_VorMore)) - 1,
         Annoy_SorMore = as.numeric(as.factor(Annoy_SorMore)) - 1,
         Annoy_MorMore = as.numeric(as.factor(Annoy_MorMore)) - 1,
         Annoy_VorMore = as.numeric(as.factor(Annoy_VorMore)) - 1)

# Make ordered factor out of the sum of Annoy and sum of IntWithNQ
dAll <- dAll %>%
  mutate(Annoy3 = as.ordered(as.factor(rowSums(select_(., "Annoy_SorMore", "Annoy_MorMore", "Annoy_VorMore")))),
         IntWithNQ3 = as.ordered(as.factor(rowSums(select_(., "IntWithNQ_SorMore", "IntWithNQ_MorMore", "IntWithNQ_VorMore")))))


dAll %>% select(Annoy_VorMore, Annoy_MorMore, Annoy_SorMore, Annoy3)
dAll %>% select(IntWithNQ_VorMore, IntWithNQ_MorMore, IntWithNQ_SorMore, IntWithNQ3)

# Make factors and numeric mediator variables as appropriate
dAll <- dAll %>%
  mutate(Site = as.factor(Site),
         SiteType = as.factor(SiteType),
         Survey = as.factor(Survey),
         SiteFirstVisit = as.factor(SiteFirstVisit),
         ImpNQ_VorMore = as.factor(ifelse(dAll$ImpNQ_VorMore == 'Yes', 1, 0)),
         ImpHistCult_VorMore =  as.factor(ifelse(dAll$ImpHistCult == 'Very' | dAll$ImpHistCult == 'Extremely' , 1, 0)),
         AdultsOnly = as.factor(ifelse(dAll$AdultsOnly == TRUE | dAll$AdultsOnly == 'Yes', 1, 0)),
         Dataset = as.factor(Dataset)
         )

# Check data 
key_vars = c('SELAllAC', 'PEnProps','PEnHelos', 'PTAudAllAC', 'lg10.PTAudAllAC',
             'Dataset', 'Site', 'SiteType',
             'ImpHistCult_VorMore','ImpNQ_VorMore','SiteFirstVisit','Survey',
             'Annoy3', 'IntWithNQ3')

pairs(dAll[,key_vars])


save(list = 'dAll',
     file = file.path(project_shared_drive,
                      '2020 Grand Analysis',
                      'GrandAnalysis_CompleteDoseVars.RData')
     )

write.csv(dAll,
          file = file.path(project_shared_drive,
                           '2020 Grand Analysis',
                           'GrandAnalysis_CompleteDoseVars.csv'
                           ),
          row.names = F)
