# Dose-response work 2016, updated for Grand Analysis using CLMM
# Dan Flynn | daniel.flynn@dot.gov

library(lme4)   # for glmer() generalized linear mixed effects models
library(sjPlot) # for summary tables using tab_model
library(scales) # for alpha() and muted()
library(ordinal) # for clmm

project_shared_drive = "//vntscex/DFS/Projects/PROJ-VXK600/MLB48"

output = file.path(project_shared_drive,
                   '2020 Grand Analysis',
                   'Output')

if(!dir.exists(output)){ dir.create(output) }

# For dAll data frame, 5233 observations and 31 variables
load(file.path(project_shared_drive,
               '2020 Grand Analysis',
               'GrandAnalysis_CompleteDoseVars.RData'))

dos_vars = c('SELAllAC', 'PEnProps','PEnHelos', 'PTAudAllAC', 'lg10.PTAudAllAC')
dat_vars = c('Dataset', 'Site', 'SiteType', 'Park')
med_vars = c('ImpHistCult_VorMore','ImpNQ_VorMore','SiteFirstVisit', 'DurVisitMinutes')
res_vars = c('Annoy3', 'IntWithNQ3')

# <<>><<>><<>><<>><<>><<>><<>><<>><<>>
# Re-run final models ----

# models 1 and 7 for annoy and interfere

# Manual model runs to interpret thresholds
annoy_01 <- clmm(Annoy3 ~ SELAllAC + PEnProps + PEnHelos + Dataset + SiteType + (1 | Site),
             data = dAll,
             Hess = 1,
             link = "logit") 


pa1 <- confint(annoy_01)
profile.clmm(annoy_01)
