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

## Questions:
# clmm v clmm2?
# SiteType as random (can't pick it as category in tool if it's random)?
# confirm link = 'logit'

##
#Base (Site random - need random effect in model?)
m1 <- clmm(Annoy3 ~ SELAllAC + PEnHelos + PEnProps + (1|Site),
           data = dAll,
           Hess = T,
           link = "logit") # for proportional odds mixed model

# m1 summary
summary(m1)
confint(m1)
m1$Theta
m1$info[,c('nobs','AIC')] #nobs 3433 AIC 5898.2 

##
# m2(site type - fixed)
m2 <- clmm(Annoy3 ~ SELAllAC + PEnHelos + PEnProps + SiteType + (1|Site),
           data = dAll,
           Hess = T,
           link = "logit") # for proportional odds mixed model

# m2 summary
summary(m2)
confint(m2)
m2$Theta
m2$info[,c('nobs','AIC')] #nobs 3433 AIC 5890.2
##Adding site type improves model

## 
# m3(park instead of site)
m3 <- clmm(Annoy3 ~ SELAllAC + PEnHelos + PEnProps + SiteType + (1|Park),
           data = dAll,
           Hess = T,
           link = "logit") # for proportional odds mixed model

# m3 summary
summary(m3)
confint(m3)
m3$Theta
m3$info[,c('nobs','AIC')] #nobs 3433 AIC 5963.3 
##m2 is best - Site RE, plus site type fixed

## 
# m4(add PTAuddAllAC)
m4 <- clmm(Annoy3 ~ SELAllAC + PTAudAllAC + PEnHelos + PEnProps + SiteType + (1|Site),
           data = dAll,
           Hess = T,
           link = "logit") # for proportional odds mixed model

# m4 summary
summary(m4)
confint(m4)
m4$Theta
m4$info[,c('nobs','AIC')] #nobs 3433 AIC 5882
##m4 is best - Site RE, plus site type fixed plus PTAudAllAC

## 
# m5(LeqAllAC instead of PTAuddAllAC + SELAllAC)
m5 <- clmm(Annoy3 ~ LeqAllAC + PEnHelos + PEnProps + SiteType + (1|Site),
           data = dAll,
           Hess = T,
           link = "logit") # for proportional odds mixed model

# m5 summary
summary(m5)
confint(m5)
m5$Theta
m5$info[,c('nobs','AIC')] #nobs 3400 AIC 5813
##m5 is best BUT LOWER N (subset and retest) - Site RE, plus site type fixed with LeqAllAC dose

## 
# m6(log(PTAudAllAC) instead of PTAuddAllAC)
m6 <- clmm(Annoy3 ~ SELAllAC + lg10.PTAudAllAC + PEnHelos + PEnProps + SiteType + (1|Site),
           data = dAll,
           Hess = T,
           link = "logit") # for proportional odds mixed model

# m6 summary
summary(m6)
confint(m6)
m6$Theta
m6$info[,c('nobs','AIC')] #nobs 3429 AIC 5869.2
##m5 is still best BUT LOWER N (subset and retest) - Site RE, plus site type fixed with LeqAllAC dose

## 
# m7(Add durvisit)
m7 <- clmm(Annoy3 ~ LeqAllAC + PEnHelos + PEnProps + SiteType + DurVisitMinutes +(1|Site),
           data = dAll,
           Hess = T,
           link = "logit") # for proportional odds mixed model

# m7 summary
summary(m7)
confint(m7)
m7$Theta
m7$info[,c('nobs','AIC')] #nobs 3400 AIC 5808.8
##m7 is best BUT LOWER N (subset and retest) - Site RE, plus site type fixed with LeqAllAC dose

## 
# m8(+ Adults only + SiteFirstVisit)
m8 <- clmm(Annoy3 ~ LeqAllAC + PEnHelos + PEnProps + SiteType 
           + DurVisitMinutes 
           + AdultsOnly + SiteFirstVisit
           + (1|Site),
           data = dAll,
           Hess = T,
           link = "logit") # for proportional odds mixed model

# m8 summary
summary(m8)
confint(m8)
m8$Theta
m8$info[,c('nobs','AIC')] #nobs 3393 AIC 5820.8
##m8 is worse than m7, even with lower n

## 
# m9(+ ImpHist + ImpNQ)
m9 <- clmm(Annoy3 ~ LeqAllAC + PEnHelos + PEnProps + SiteType 
           + DurVisitMinutes 
           + ImpHistCult_VorMore + ImpNQ_VorMore
           + (1|Site),
           data = dAll,
           Hess = T,
           link = "logit") # for proportional odds mixed model

# m9 summary
summary(m9)
confint(m9)
m9$Theta
m9$info[,c('nobs','AIC')] #nobs 3351 AIC 5718.1
##m9 is better than m7 BUT LOWER N (subset and retest)
