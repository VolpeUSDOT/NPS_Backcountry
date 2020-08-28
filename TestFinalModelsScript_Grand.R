# Grand Analysis of all sites, using approach from previous backcountry work.
# Many fewer possible models, since only one model for Annoy, and one for Interfere.
# Test models with combinations of: 
# 1) Two dose variables: LEQ vs. SEL + PTAudAllAC
# 2) Random effects (Data set, Survey, Site, Site Type - can use 2 of the 4)
# 3) Mediators


# https://cran.r-project.org/web/packages/ordinal/vignettes/clm_article.pdf
# https://cran.r-project.org/web/packages/ordinal/vignettes/clmm2_tutorial.pdf

# General structure for CLMM models:

# m{model_number} <- clmm({ordered_response} ~ {dose_var} + {mediator_vars} +
#            + (1|Site) or (1|Park) or (1|Dataset)
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

output = file.path(project_shared_drive,
                   '2020 Grand Analysis',
                   'Output')

if(!dir.exists(output)){ dir.create(output) }

# For dAll data frame, 5233 observations and 31 variables
load(file.path(project_shared_drive,
          '2020 Grand Analysis',
          'GrandAnalysis_CompleteDoseVars.RData'))

## Variables to test ####
dos_vars = c('LeqAllAC', 'SELAllAC', 'PEnProps','PEnHelos', 'PTAudAllAC', 'lg10.PTAudAllAC')
dat_vars = c('Site', 'SiteType', 'Park', 'Survey')
med_vars = c('ImpHistCult_VorMore','ImpNQ_VorMore','SiteFirstVisit', 'DurVisitMinutes', 'AdultsOnly')
res_vars = c('Annoy3', 'IntWithNQ3')


#### Manual model runs to compare features ####

## QUestions:
# clmm v clmm2?
# SiteType as random (can't pick it as category in tool if it's random)?

#Base
m1 <- clmm(Annoy3 ~ SELAllAC + PEnHelos + PEnProps + SiteType + (1|Site),
     data = dAll,
     Hess = T,
     link = "logit") # for proportional odds mixed model

# Extract confidence intervals for plotting
summary(m1)
confint(m1)
m1$Theta
exp(confint(m1)) #?

#Base (Site random)
m1 <- clmm(Annoy3 ~ SELAllAC + PEnHelos + PEnProps + (1|Site),
           data = dAll,
           Hess = T,
           link = "logit") # for proportional odds mixed model

# m1 summary
summary(m1)
confint(m1)
m1$Theta
m1$info[,c('nobs','AIC')]


# m2(site type - fixed)
m2 <- clmm(Annoy3 ~ SELAllAC + PEnHelos + PEnProps + SiteType + (1|Site),
           data = dAll,
           Hess = T,
           link = "logit") # for proportional odds mixed model

# m2 summary
summary(m2)
confint(m2)
m2$Theta
m2$info[,c('nobs','AIC')]

#Adding site type improves model



