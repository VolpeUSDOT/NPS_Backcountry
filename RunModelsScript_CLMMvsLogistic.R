# Grand Analysis of all sites, using approach from previous backcountry work.
# Many fewer possible models, since only one model for Annoy, and one for Interfere.

# https://cran.r-project.org/web/packages/ordinal/vignettes/clm_article.pdf
# https://cran.r-project.org/web/packages/ordinal/vignettes/clmm2_tutorial.pdf

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
library(tidyverse)
#library(readxl)
#library(tidyselect)


project_shared_drive = "//vntscex/DFS/Projects/PROJ-VXK600/MLB48"

output = file.path(project_shared_drive,
                   '2020 Grand Analysis',
                   'Output')

if(!dir.exists(output)){ dir.create(output) }

# For dAll data frame, 5233 observations and 31 variables
load(file.path(project_shared_drive,
          '2020 Grand Analysis',
          'GrandAnalysis_CompleteDoseVars.RData'))

#Variables used for models in RunModelScript
#dos_vars = c('SELAllAC', 'PEnProps','PEnHelos', 'PTAudAllAC', 'lg10.PTAudAllAC')
#dat_vars = c('Dataset', 'Site', 'SiteType', 'Park')
#med_vars = c('ImpHistCult_VorMore','ImpNQ_VorMore','SiteFirstVisit', 'DurVisitMinutes')
#res_vars = c('Annoy3', 'IntWithNQ3')

### Compare: 1) Logistic vs clmm for just d00, 2) d90 vs d00 for logistic, and 3) d90 vs d00 for CLMM----

# d90 vs d00 for CLMM 
d90sub <- dAll %>%
  filter(Dataset == "90s")

d00sub <- dAll %>%
  filter(Dataset == "00s")

#Run models directly for different datasets

#dAll dataset----

#Model 1
annoy_01_dAll = clmm(Annoy3 ~ SELAllAC + PTAudAllAC + (1|Site), 
            Hess = T,
            data = dAll,
            link = "logit")

interfere_01_dAll = clmm(IntWithNQ3 ~ SELAllAC + PTAudAllAC + (1|Site), 
                    Hess = T,
                    data = dAll,
                    link = "logit")


#Model 7
annoy_07_dAll = clmm(Annoy3 ~ SELAllAC + lg10.PTAudAllAC + 
                  ImpHistCult_VorMore + ImpNQ_VorMore + SiteFirstVisit +
                  (1|Site), 
                Hess = T,
                data = dAll,
                link = "logit")

interfere_07_dAll = clmm(IntWithNQ3 ~ SELAllAC + lg10.PTAudAllAC + 
                      ImpHistCult_VorMore + ImpNQ_VorMore + SiteFirstVisit +
                      (1|Site), 
                    Hess = T,
                    data = dAll,
                    link = "logit")

#d00s dataset----

#Model 1
annoy_01_d00 = clmm(Annoy3 ~ SELAllAC + PTAudAllAC + (1|Site), 
                Hess = T,
                data = d00sub,
                link = "logit")

interfere_01_d00 = clmm(IntWithNQ3 ~ SELAllAC + PTAudAllAC + (1|Site), 
                    Hess = T,
                    data = d00sub,
                    link = "logit")


#Model 7
annoy_07_d00 = clmm(Annoy3 ~ SELAllAC + lg10.PTAudAllAC + 
                  ImpHistCult_VorMore + ImpNQ_VorMore + SiteFirstVisit +
                  (1|Site), 
                Hess = T,
                data = d00sub,
                link = "logit")

interfere_07_d00 = clmm(IntWithNQ3 ~ SELAllAC + lg10.PTAudAllAC + 
                      ImpHistCult_VorMore + ImpNQ_VorMore + SiteFirstVisit +
                      (1|Site), 
                    Hess = T,
                    data = d00sub,
                    link = "logit")

#d90s dataset----

#Model 1
annoy_01_d90 = clmm(Annoy3 ~ SELAllAC + PTAudAllAC + (1|Site), 
                    Hess = T,
                    data = d90sub,
                    link = "logit")

interfere_01_d90 = clmm(IntWithNQ3 ~ SELAllAC + PTAudAllAC + (1|Site), 
                        Hess = T,
                        data = d90sub,
                        link = "logit")

#Model 7
annoy_07_d90 = clmm(Annoy3 ~ SELAllAC + lg10.PTAudAllAC + 
                      ImpHistCult_VorMore + ImpNQ_VorMore + SiteFirstVisit +
                      (1|Site), 
                    Hess = T,
                    data = d90sub,
                    link = "logit")

interfere_07_d90 = clmm(IntWithNQ3 ~ SELAllAC + lg10.PTAudAllAC + 
                          ImpHistCult_VorMore + ImpNQ_VorMore + SiteFirstVisit +
                          (1|Site), 
                        Hess = T,
                        data = d90sub,
                        link = "logit")

###################################
#Model compare (CLMM for dAll, d90s, d00s)

CLMM_Models01 <- c("annoy_01_dAll", "interfere_01_dAll", 
               "annoy_01_d00", "interfere_01_d00", 
               "annoy_01_d90", "interfere_01_d90")

CLMM_Mods1Table <- NULL

for (i in 1:length(CLMM_Models01)){
  
  modName <- CLMM_Models01[i] 
  
  perf <- summary(get(CLMM_Models01[i]))$info[,c('nobs','AIC')]
  
  all_coefs <- exp(coef(get(CLMM_Models01[i])))
  thresholds <- data.frame(t(all_coefs[1:3]))
  coefx <- data.frame(t(all_coefs[4:length(all_coefs)]))
  
  model_summary <- data.frame(modName, perf, thresholds,coefx) 
  
  CLMM_Mods1Table <- rbind(CLMM_Mods1Table,model_summary)
}  
CLMM_Mods1Table
 
#####################################################################
CLMM_Models07 <- c("annoy_07_dAll", "interfere_07_dAll", 
              "annoy_07_d00", "interfere_07_d00", 
              "annoy_07_d90", "interfere_07_d90")

CLMM_Mods7Table <- NULL
for (i in 1:length(CLMM_Models07)){
  modName <- CLMM_Models07[i] 
  perf <- summary(get(CLMM_Models07[i]))$info[,c('nobs','AIC')]
  all_coefs <- exp(coef(get(CLMM_Models07[i])))
  thresholds <- data.frame(t(all_coefs[1:3]))
  coefx <- data.frame(t(all_coefs[4:length(all_coefs)]))
  
  model_summary <- data.frame(modName, perf, thresholds,coefx) 
  
  CLMM_Mods7Table <- rbind(CLMM_Mods7Table,model_summary)
}  
CLMM_Mods7Table



write.csv(CLMM_Mods1Table, file = file.path(output, 'CLMM_Mods1Table.csv'), row.names = F)
write.csv(CLMM_Mods7Table, file = file.path(output, 'CLMM_Mods7Table.csv'), row.names = F)


############################################################################
## Logistic regression
#Model 1 - dAll
annoy_SorMore_01_dAll = glmer(Annoy_SorMore ~ SELAllAC + PTAudAllAC + (1|Site),
                family = binomial(link="logit"),
                verbose = FALSE,
                data = dAll)

annoy_MorMore_01_dAll = glmer(Annoy_MorMore ~ SELAllAC + PTAudAllAC + (1|Site),
                             family = binomial(link="logit"),
                             verbose = FALSE,
                             data = dAll)

annoy_VorMore_01_dAll = glmer(Annoy_VorMore ~ SELAllAC + PTAudAllAC + (1|Site),
                             family = binomial(link="logit"),
                             verbose = FALSE,
                             data = dAll)


IntWithNQ_SorMore_01_dAll = glmer(IntWithNQ_SorMore ~ SELAllAC + PTAudAllAC + (1|Site),
                             family = binomial(link="logit"),
                             verbose = FALSE,
                             data = dAll)

IntWithNQ_MorMore_01_dAll = glmer(IntWithNQ_MorMore ~ SELAllAC + PTAudAllAC + (1|Site),
                             family = binomial(link="logit"),
                             verbose = FALSE,
                             data = dAll)

IntWithNQ_VorMore_01_dAll = glmer(IntWithNQ_VorMore ~ SELAllAC + PTAudAllAC + (1|Site),
                             family = binomial(link="logit"),
                             verbose = FALSE,
                             data = dAll)

#Model 1 - d90
annoy_SorMore_01_d90 = glmer(Annoy_SorMore ~ SELAllAC + PTAudAllAC + (1|Site),
                              family = binomial(link="logit"),
                              verbose = FALSE,
                              data = d90sub)

annoy_MorMore_01_d90 = glmer(Annoy_MorMore ~ SELAllAC + PTAudAllAC + (1|Site),
                              family = binomial(link="logit"),
                              verbose = FALSE,
                              data = d90sub)

annoy_VorMore_01_d90 = glmer(Annoy_VorMore ~ SELAllAC + PTAudAllAC + (1|Site),
                              family = binomial(link="logit"),
                              verbose = FALSE,
                              data = d90sub)


IntWithNQ_SorMore_01_d90 = glmer(IntWithNQ_SorMore ~ SELAllAC + PTAudAllAC + (1|Site),
                                  family = binomial(link="logit"),
                                  verbose = FALSE,
                                  data = d90sub)

IntWithNQ_MorMore_01_d90 = glmer(IntWithNQ_MorMore ~ SELAllAC + PTAudAllAC + (1|Site),
                                  family = binomial(link="logit"),
                                  verbose = FALSE,
                                  data = d90sub)

IntWithNQ_VorMore_01_d90 = glmer(IntWithNQ_VorMore ~ SELAllAC +  PTAudAllAC + (1|Site),
                                  family = binomial(link="logit"),
                                  verbose = FALSE,
                                  data = d90sub)

#Model 1 - d00
annoy_SorMore_01_d00 = glmer(Annoy_SorMore ~ SELAllAC + PTAudAllAC + (1|Site),
                              family = binomial(link="logit"),
                              verbose = FALSE,
                              data = d00sub)

annoy_MorMore_01_d00 = glmer(Annoy_MorMore ~ SELAllAC + PTAudAllAC + (1|Site),
                              family = binomial(link="logit"),
                              verbose = FALSE,
                              data = d00sub)

annoy_VorMore_01_d00 = glmer(Annoy_VorMore ~ SELAllAC + PTAudAllAC + (1|Site),
                              family = binomial(link="logit"),
                              verbose = FALSE,
                              data = d00sub)


IntWithNQ_SorMore_01_d00 = glmer(IntWithNQ_SorMore ~ SELAllAC + PTAudAllAC + (1|Site),
                                  family = binomial(link="logit"),
                                  verbose = FALSE,
                                  data = d00sub)

IntWithNQ_MorMore_01_d00 = glmer(IntWithNQ_MorMore ~ SELAllAC + PTAudAllAC + (1|Site),
                                  family = binomial(link="logit"),
                                  verbose = FALSE,
                                  data = d00sub)

IntWithNQ_VorMore_01_d00 = glmer(IntWithNQ_VorMore ~ SELAllAC + PTAudAllAC + (1|Site),
                                  family = binomial(link="logit"),
                                  verbose = FALSE,
                                  data = d00sub)

##############################
#Model 07 - dAll
annoy_SorMore_07_dAll = glmer(Annoy_SorMore ~ SELAllAC + lg10.PTAudAllAC + 
                                ImpHistCult_VorMore + ImpNQ_VorMore + SiteFirstVisit +
                                (1|Site),
                              family = binomial(link="logit"),
                              verbose = FALSE,
                              data = dAll)

annoy_MorMore_07_dAll = glmer(Annoy_MorMore ~ SELAllAC + lg10.PTAudAllAC + 
                                ImpHistCult_VorMore + ImpNQ_VorMore + SiteFirstVisit +
                                (1|Site),
                              family = binomial(link="logit"),
                              verbose = FALSE,
                              data = dAll)

annoy_VorMore_07_dAll = glmer(Annoy_VorMore ~ SELAllAC + lg10.PTAudAllAC + 
                                ImpHistCult_VorMore + ImpNQ_VorMore + SiteFirstVisit +
                                (1|Site),
                              family = binomial(link="logit"),
                              verbose = FALSE,
                              data = dAll)


IntWithNQ_SorMore_07_dAll = glmer(IntWithNQ_SorMore ~ SELAllAC + lg10.PTAudAllAC + 
                                    ImpHistCult_VorMore + ImpNQ_VorMore + SiteFirstVisit +
                                    (1|Site),
                                  family = binomial(link="logit"),
                                  verbose = FALSE,
                                  data = dAll)

IntWithNQ_MorMore_07_dAll = glmer(IntWithNQ_MorMore ~ SELAllAC +  lg10.PTAudAllAC + 
                                    ImpHistCult_VorMore + ImpNQ_VorMore + SiteFirstVisit +
                                    (1|Site),
                                  family = binomial(link="logit"),
                                  verbose = FALSE,
                                  data = dAll)

IntWithNQ_VorMore_07_dAll = glmer(IntWithNQ_VorMore ~ SELAllAC + lg10.PTAudAllAC + 
                                    ImpHistCult_VorMore + ImpNQ_VorMore + SiteFirstVisit +
                                    (1|Site),
                                  family = binomial(link="logit"),
                                  verbose = FALSE,
                                  data = dAll)

#Model 07 - d90
annoy_SorMore_07_d90 = glmer(Annoy_SorMore ~ SELAllAC + lg10.PTAudAllAC + 
                               ImpHistCult_VorMore + ImpNQ_VorMore + SiteFirstVisit +
                               (1|Site),
                             family = binomial(link="logit"),
                             verbose = FALSE,
                             data = d90sub)

annoy_MorMore_07_d90 = glmer(Annoy_MorMore ~ SELAllAC + lg10.PTAudAllAC + 
                               ImpHistCult_VorMore + ImpNQ_VorMore + SiteFirstVisit +
                               (1|Site),
                             family = binomial(link="logit"),
                             verbose = FALSE,
                             data = d90sub)

annoy_VorMore_07_d90 = glmer(Annoy_VorMore ~ SELAllAC + lg10.PTAudAllAC + 
                               ImpHistCult_VorMore + ImpNQ_VorMore + SiteFirstVisit +
                               (1|Site),
                             family = binomial(link="logit"),
                             verbose = FALSE,
                             data = d90sub)


IntWithNQ_SorMore_07_d90 = glmer(IntWithNQ_SorMore ~ SELAllAC + lg10.PTAudAllAC + 
                                   ImpHistCult_VorMore + ImpNQ_VorMore + SiteFirstVisit +
                                   (1|Site),
                                 family = binomial(link="logit"),
                                 verbose = FALSE,
                                 data = d90sub)

IntWithNQ_MorMore_07_d90 = glmer(IntWithNQ_MorMore ~ SELAllAC +  lg10.PTAudAllAC + 
                                   ImpHistCult_VorMore + ImpNQ_VorMore + SiteFirstVisit +
                                   (1|Site),
                                 family = binomial(link="logit"),
                                 verbose = FALSE,
                                 data = d90sub)

IntWithNQ_VorMore_07_d90 = glmer(IntWithNQ_VorMore ~ SELAllAC + lg10.PTAudAllAC + 
                                   ImpHistCult_VorMore + ImpNQ_VorMore + SiteFirstVisit +
                                   (1|Site),
                                 family = binomial(link="logit"),
                                 verbose = FALSE,
                                 data = d90sub)

#Model 07 - d00
annoy_SorMore_07_d00 = glmer(Annoy_SorMore ~ SELAllAC + lg10.PTAudAllAC + 
                               ImpHistCult_VorMore + ImpNQ_VorMore + SiteFirstVisit +
                               (1|Site),
                             family = binomial(link="logit"),
                             verbose = FALSE,
                             data = d90sub)

annoy_MorMore_07_d00 = glmer(Annoy_MorMore ~ SELAllAC + lg10.PTAudAllAC + 
                               ImpHistCult_VorMore + ImpNQ_VorMore + SiteFirstVisit +
                               (1|Site),
                             family = binomial(link="logit"),
                             verbose = FALSE,
                             data = d00sub)

annoy_VorMore_07_d00 = glmer(Annoy_VorMore ~ SELAllAC + lg10.PTAudAllAC + 
                               ImpHistCult_VorMore + ImpNQ_VorMore + SiteFirstVisit +
                               (1|Site),
                             family = binomial(link="logit"),
                             verbose = FALSE,
                             data = d00sub)


IntWithNQ_SorMore_07_d00 = glmer(IntWithNQ_SorMore ~ SELAllAC + lg10.PTAudAllAC + 
                                   ImpHistCult_VorMore + ImpNQ_VorMore + SiteFirstVisit +
                                   (1|Site),
                                 family = binomial(link="logit"),
                                 verbose = FALSE,
                                 data = d00sub)

IntWithNQ_MorMore_07_d00 = glmer(IntWithNQ_MorMore ~SELAllAC +  lg10.PTAudAllAC + 
                                   ImpHistCult_VorMore + ImpNQ_VorMore + SiteFirstVisit +
                                   (1|Site),
                                 family = binomial(link="logit"),
                                 verbose = FALSE,
                                 data = d00sub)

IntWithNQ_VorMore_07_d00 = glmer(IntWithNQ_VorMore ~ SELAllAC + lg10.PTAudAllAC + 
                                   ImpHistCult_VorMore + ImpNQ_VorMore + SiteFirstVisit +
                                   (1|Site),
                                 family = binomial(link="logit"),
                                 verbose = FALSE,
                                 data = d00sub)




###################################
#Model compare (LogReg for dAll, d90s, d00s)
##DOES NOT WORK YET - tables in wrong direction

#Model 1
LR_Models01 <- c("annoy_SorMore_01_dAll", "annoy_MorMore_01_dAll", "annoy_VorMore_01_dAll",
                 "annoy_SorMore_01_d90", "annoy_MorMore_01_d90", "annoy_VorMore_01_d90",
                 "annoy_SorMore_01_d00", "annoy_MorMore_01_d00", "annoy_VorMore_01_d00", 
                 "IntWithNQ_SorMore_01_dAll","IntWithNQ_MorMore_01_dAll","IntWithNQ_VorMore_01_dAll",
                 "IntWithNQ_SorMore_01_d90","IntWithNQ_MorMore_01_d90","IntWithNQ_VorMore_01_d90",
                 "IntWithNQ_SorMore_01_d00","IntWithNQ_MorMore_01_d00","IntWithNQ_VorMore_01_d00")

# Outputs to get:
# modName, nobs, AIC, coef for SELAllAC S/M/VorMore  

LR_Mods1Table <- NULL

i=1

for (i in 1:length(LR_Models01)){
  modName <- LR_Models01[i] 
  betas = (fixef(get(LR_Models01[i])))
  AIC <- round(summary(get(LR_Models01[i]))$AICtab,1)[1]
  nobs = length(summary(get(LR_Models01[i]))$residuals)
  
  model_summary <- data.frame(modName, AIC, nobs, t(exp(betas)))
  
  LR_Mods1Table <- rbind(LR_Mods1Table, model_summary)
}  
LR_Mods1Table


#Model 7

LR_Models07 <- c("annoy_SorMore_07_dAll", "annoy_MorMore_07_dAll", "annoy_VorMore_07_dAll",
                 "annoy_SorMore_07_d90", "annoy_MorMore_07_d90", "annoy_VorMore_07_d90",
                 "annoy_SorMore_07_d00", "annoy_MorMore_07_d00", "annoy_VorMore_07_d00", 
                 "IntWithNQ_SorMore_07_dAll","IntWithNQ_MorMore_07_dAll","IntWithNQ_VorMore_07_dAll",
                 "IntWithNQ_SorMore_07_d90","IntWithNQ_MorMore_07_d90","IntWithNQ_VorMore_07_d90",
                 "IntWithNQ_SorMore_07_d00","IntWithNQ_MorMore_07_d00","IntWithNQ_VorMore_07_d00")

LR_Mods7Table <- NULL
i=1
for (i in 1:length(LR_Models07)){
  modName <- LR_Models07[i] 
  betas = c(fixef(get(LR_Models07[i])))
  AIC <- round(summary(get(LR_Models01[i]))$AICtab,1)[1]
  nobs = length(summary(get(LR_Models01[i]))$residuals)
  
  model_summary <- data.frame(modName, AIC, nobs, t(exp(betas)))
  
  LR_Mods7Table <- rbind(LR_Mods7Table,model_summary)
}  
LR_Mods7Table



#####################################################################

write.csv(LR_Mods1Table, file = file.path(output, 'LR_Mods1Table.csv'), row.names = F)
write.csv(LR_Mods7Table, file = file.path(output, 'LR_Mods7Table.csv'), row.names = F)

