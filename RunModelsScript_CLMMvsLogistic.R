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

#source("get_packages.R")

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
annoy_01_dAll = clmm(Annoy3 ~ PTAudAllAC + (1|Site), 
            Hess = T,
            data = dAll,
            link = "logit")

interfere_01_dAll = clmm(IntWithNQ3 ~ PTAudAllAC + (1|Site), 
                    Hess = T,
                    data = dAll,
                    link = "logit")


#Model 7
annoy_07_dAll = clmm(Annoy3 ~ lg10.PTAudAllAC + 
                  ImpHistCult_VorMore + ImpNQ_VorMore + SiteFirstVisit +
                  (1|Site), 
                Hess = T,
                data = dAll,
                link = "logit")

interfere_07_dAll = clmm(IntWithNQ3 ~ lg10.PTAudAllAC + 
                      ImpHistCult_VorMore + ImpNQ_VorMore + SiteFirstVisit +
                      (1|Site), 
                    Hess = T,
                    data = dAll,
                    link = "logit")

#d00s dataset----

#Model 1
annoy_01_d00 = clmm(Annoy3 ~ PTAudAllAC + (1|Site), 
                Hess = T,
                data = d00sub,
                link = "logit")

interfere_01_d00 = clmm(IntWithNQ3 ~ PTAudAllAC + (1|Site), 
                    Hess = T,
                    data = d00sub,
                    link = "logit")


#Model 7
annoy_07_d00 = clmm(Annoy3 ~ lg10.PTAudAllAC + 
                  ImpHistCult_VorMore + ImpNQ_VorMore + SiteFirstVisit +
                  (1|Site), 
                Hess = T,
                data = d00sub,
                link = "logit")

interfere_07_d00 = clmm(IntWithNQ3 ~ lg10.PTAudAllAC + 
                      ImpHistCult_VorMore + ImpNQ_VorMore + SiteFirstVisit +
                      (1|Site), 
                    Hess = T,
                    data = d00sub,
                    link = "logit")

#d90s dataset----

#Model 1
annoy_01_d90 = clmm(Annoy3 ~ PTAudAllAC + (1|Site), 
                    Hess = T,
                    data = d90sub,
                    link = "logit")

interfere_01_d90 = clmm(IntWithNQ3 ~ PTAudAllAC + (1|Site), 
                        Hess = T,
                        data = d90sub,
                        link = "logit")

#Model 7
annoy_07_d90 = clmm(Annoy3 ~ lg10.PTAudAllAC + 
                      ImpHistCult_VorMore + ImpNQ_VorMore + SiteFirstVisit +
                      (1|Site), 
                    Hess = T,
                    data = d90sub,
                    link = "logit")

interfere_07_d90 = clmm(IntWithNQ3 ~ lg10.PTAudAllAC + 
                          ImpHistCult_VorMore + ImpNQ_VorMore + SiteFirstVisit +
                          (1|Site), 
                        Hess = T,
                        data = d90sub,
                        link = "logit")

###################################
#Model compare (CLMM for dAll, d90s, d00s)

Models01 <- c("annoy_01_dAll", "interfere_01_dAll", 
               "annoy_01_d00", "interfere_01_d00", 
               "annoy_01_d90", "interfere_01_d90")

Mods1Table <- NULL
for (i in 1:length(Models01)){
  modName <- Models01[i] 
  perf <- summary(get(Models01[i]))$info[,c('nobs','AIC')]
  all_coefs <- exp(coef(get(Models01[i])))
  thresholds <- data.frame(t(all_coefs[1:3]))
  coefx <- data.frame(t(all_coefs[4:length(all_coefs)]))
  
  model_summary <- data.frame(modName, perf, thresholds,coefx) 
  
  Mods1Table <- rbind(Mods1Table,model_summary)
}  
Mods1Table
 
#####################################################################
Models07 <- c("annoy_07_dAll", "interfere_07_dAll", 
              "annoy_07_d00", "interfere_07_d00", 
              "annoy_07_d90", "interfere_07_d90")

Mods7Table <- NULL
for (i in 1:length(Models07)){
  modName <- Models07[i] 
  perf <- summary(get(Models07[i]))$info[,c('nobs','AIC')]
  all_coefs <- exp(coef(get(Models07[i])))
  thresholds <- data.frame(t(all_coefs[1:3]))
  coefx <- data.frame(t(all_coefs[4:length(all_coefs)]))
  
  model_summary <- data.frame(modName, perf, thresholds,coefx) 
  
  Mods7Table <- rbind(Mods7Table,model_summary)
}  
Mods7Table



write.csv(Mods1Table, file = file.path(output, 'Mods1Table.csv'), row.names = F)


write.csv(Mods7Table, file = file.path(output, 'Mods7Table.csv'), row.names = F)




# Summary: 




