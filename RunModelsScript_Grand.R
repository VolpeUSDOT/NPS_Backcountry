# Grand Analysis of all sites, using approach from previous backcountry work.
# Important differences from previous work:
# 1. Many fewer possible models, since only one model for Annoy, and one for Interfere.


#1) Compare alternate dose models and importance of natural quiet vs calm/peace. Include single and compound doses, plus Survey and ImpNQ_VorMore OR ImpCP_VorMore.

#2) Test binary mediators (coded as 0 or 1). Add individually and in combination. Accept new mediator if average of AIC values for the Annoy or Interfere models drops  

# General structure for CLMM models:

# m{model_number} <- clmm({ordered_response} ~ {dose_var} + {mediator_vars} +
#            + (1|Site)
#            + SiteType,
#            Hess = T, 
#            data = dAll,
#            link = "logit")

# Setup ---- 
rm(list = ls())	

source("get_packages.R")

library(lme4)   # for glmer() generalized linear mixed effects models
library(sjPlot) # for summary tables using sjt.glmer and summary plots using sjp.glmer
library(scales) # for alpha() and muted()
library(MASS) # for polr
library(ordinal) # for clmm

project_shared_drive = "//vntscex/DFS/Projects/PROJ-VXK600/MLB48"

# For dAll data frame, 5233 observations and 30 variables
load(file.path(project_shared_drive,
          '2020 Grand Analysis',
          'GrandAnalysis_CompleteDoseVars.RData'))

dos_vars = c('SELAllAC', 'PEnProps','PEnHelos', 'PTAudAllAC', 'lg10.PTAudAllAC')
dat_vars = c('Dataset', 'Site', 'SiteType', 'Park')
med_vars = c('ImpHistCult_VorMore','ImpNQ_VorMore','SiteFirstVisit','Survey')
res_vars = c('Annoy3', 'IntWithNQ3')

# Model run function ----

# Run for Annoy and Interfere, using model number as a 
# run_clmm <- function()
# m{model_number} <- clmm({ordered_response} ~ {dose_var} + {mediator_vars} +
#            + (1|Site)
#            + SiteType,
#            Hess = T,
#            data = dAll,
#            link = "logit")

# Model 1: Base ----

model_number = 1


assign(paste0('annoy_',
              formatC(model_number, width = 2, flag = 0)),

  clmm(Annoy3 ~ SELAllAC + PEnProps + PEnHelos + PTAudAllAC + 
         Dataset + SiteType  + 
         (1|Site),
         Hess = T,
         data = dAll,
         link = "logit")
  )


summary(annoy_01)



ctable <- coef(summary(annoy_01))
(ci <- confint(annoy_01)) # default method gives profiled CIs


# odds ratio
exp(coef(m1))
# odds ratio
exp(coef(annoy_01))
round(exp(cbind(OR = coef(annoy_01), ci)), 3)

# Model 2: Survey	----


# Model 3: All Mediators ----


# Model 4: Mediators + Survey  ---- 


#	Model  5: 1 + log(PTAudAllAC)  ----


#	Model  6: 2 + log(PTAudAllAC)  ----


#	Model  7: 3 + log(PTAudAllAC)  ----

#	Model  8: 4 + log(PTAudAllAC)  ---- 


# Run model selection script ----
source("Model_Selection_Overnight.R")