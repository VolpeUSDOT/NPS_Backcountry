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


#### Manual model runs to interpret thresholds ####
m1_1 <- clmm(Annoy3 ~ SELAllAC + (1 | Site),
     data = dAll,
     Hess = T,
     link = "logit") # for proportional odds mixed model


# Omitted values from data

omitted <- dAll[1:nrow(dAll) %in% m1_1$na.action,]
used <- dAll[!1:nrow(dAll) %in% m1_1$na.action,]
obs <- as.numeric(used$Annoy3)
pred <- exp(m1_1$fitted.values)

plot(pred ~ used$Annoy3)

# CLMM 2 version
m1_2 <- clmm2(Annoy3 ~ SELAllAC,
              random = Site,
              data = dAll,
              Hess = T,
              link = "logistic") 

# Extract confidence intervals for plotting
exp(confint(m1_1))

m1_1$Theta


omitted <- dAll[1:nrow(dAll) %in% m1_2$na.action,]
used <- dAll[!1:nrow(dAll) %in% m1_2$na.action,]
obs <- as.numeric(used$Annoy3)
pred <- exp(m1_2$fitted.values)

plot(pred ~ used$Annoy3)

# profile(m1_1)

# plot(m1_1)


# 
# m1_2 <- clmm(IntWithNQ3 ~ SELAllAC + (1 | Site),
#              data = dAll,
#              Hess = T,
#              link = "logit")

# Model 2: Survey	----

run_clmm(model_no = 2,
         PTAud = 'PTAudAllAC',
         use_survey = 'Survey',
         med_vars = NULL,
         GeoVar = 'Site')

# Model 3: All Mediators ----

run_clmm(model_no = 3,
         PTAud = 'PTAudAllAC',
         use_survey = NULL,
         med_vars = c('ImpHistCult_VorMore','ImpNQ_VorMore','SiteFirstVisit', 'DurVisitMinutes'),
         GeoVar = 'Site')


# Model 4: Mediators + Survey  ---- 

run_clmm(model_no = 4,
         PTAud = 'PTAudAllAC',
         use_survey = 'Survey',
         med_vars = c('ImpHistCult_VorMore','ImpNQ_VorMore','SiteFirstVisit'), #'DurVisitMinutes'),
         GeoVar = 'Site')

#	Model  5: 1 + log(PTAudAllAC)  ----

run_clmm(model_no = 5, 
         PTAud = 'lg10.PTAudAllAC',
         med_vars = NULL,
         GeoVar = 'Site')

#	Model  6: 2 + log(PTAudAllAC)  ----

run_clmm(model_no = 6,
         PTAud = 'lg10.PTAudAllAC',
         use_survey = 'Survey',
         med_vars = NULL,
         GeoVar = 'Site')

#	Model  7: 3 + log(PTAudAllAC)  ----


run_clmm(model_no = 7,
         PTAud = 'lg10.PTAudAllAC',
         #use_survey = NULL,
         med_vars = c('ImpHistCult_VorMore','ImpNQ_VorMore','SiteFirstVisit', 'DurVisitMinutes'),# doesn't run - b/c not a factor?
         GeoVar = 'Site')

#	Model  8: 4 + log(PTAudAllAC)  ---- 

run_clmm(model_no = 8,
         PTAud = 'lg10.PTAudAllAC',
         use_survey = 'Survey',
         med_vars = c('ImpHistCult_VorMore','ImpNQ_VorMore','SiteFirstVisit', 'DurVisitMinutes'),
         GeoVar = 'Site')

# These don't work because RB == Rainbow Bridge at park level, so completely singular geographic variable 
# # Model 9: 1 with Park instead of Site ----
# 
# run_clmm(model_no = 9,
#          PTAud = 'PTAudAllAC',
#          med_vars = NULL,
#          GeoVar = 'Park')
# 
# # Model 10: 2 with Park instead of Site	----
# 
# run_clmm(model_no = 10,
#          PTAud = 'PTAudAllAC',
#          use_survey = 'Survey',
#          med_vars = NULL,
#          GeoVar = 'Park')
# 
# 
# # Model 11: 3 with Park instead of Site ----
# 
# run_clmm(model_no = 11,
#          PTAud = 'PTAudAllAC',
#          use_survey = NULL,
#          med_vars = c('ImpHistCult_VorMore','ImpNQ_VorMore','SiteFirstVisit', 'DurVisitMinutes'),
#          GeoVar = 'Park')
# 
# 
# # Model 12: Mediators + Survey with Park ---- 
# 
# run_clmm(model_no = 12,
#          PTAud = 'PTAudAllAC',
#          use_survey = 'Survey',
#          med_vars = c('ImpHistCult_VorMore','ImpNQ_VorMore','SiteFirstVisit', 'DurVisitMinutes'),
#          GeoVar = 'Park')


# Model comparison formatting ----

model_compare$response = unlist(lapply(strsplit(model_compare$model_name, "_"), function(x) x[[1]]))
model_compare$model_no = unlist(lapply(strsplit(as.character(model_compare$model_name), "_"), function(x) x[[2]]))


model_compare$model_name <- as.factor(model_compare$model_name)
model_compare$model_no <- as.factor(model_compare$model_no)
model_compare$response <- as.factor(model_compare$response)
model_compare$AIC <- as.numeric(model_compare$AIC)


write.csv(model_compare, file = file.path(output, 'Model_Compare.csv'), row.names = F)

names(thresholds) = c('model_name',
                      '0|1',
                      '1|2',
                      '2|3')


write.csv(thresholds, file = file.path(output, 'Thresholds.csv'), row.names = F)


write.csv(coefs, file = file.path(output, 'Model_Coeffs.csv'), row.names = F)




library(tidyverse)

ggplot(model_compare, aes(x = model_no, y = AIC)) +
  geom_point() + 
  facet_wrap(~response + PTAud, scales = 'free_y')


# Summary: 
# Park instead of Site is worse, stick with site but consider Park/Site nested
# log10(PTAud) can't compare by default, since different number of observations (0's omitted), need to compare with same data, but appears to be much worse
# Including survey improves every model BUT is singular within Dataset, so produces illogical outputs.
# Including mediators improves models a tiny bit
# Next steps: move backwards on indivdiaul mediators to simplify
# Model 7 is the best by AIC of the reasonable models, but uses the fewest data points. Site, log Pct Aud, and mediators
# Model 1 is a very reasonable model overall, and is most straightforward to interpret. Site, PTAud, no mediators

#summary()


### Compare: 1) Logistic vs clmm for just d00, 2) d90 vs d00 for logistic, and 3) d90 vs d00 for CLMM----

# d90 vs d00 for CLMM 
d90sub <- dAll %>%
  filter(Dataset == "90s")

d00sub <- dAll %>%
  filter(Dataset == "00s")

#Need to adjust function to specify dataset to run models for different datasets

#Model 1
run_clmm(model_no = 1,
         PTAud = 'PTAudAllAC',
         med_vars = NULL,
         GeoVar = 'Site')

#Model 7
run_clmm(model_no = 7,
         PTAud = 'lg10.PTAudAllAC',
         use_survey = NULL,
         med_vars = c('ImpHistCult_VorMore','ImpNQ_VorMore','SiteFirstVisit'), 
         GeoVar = 'Site')



