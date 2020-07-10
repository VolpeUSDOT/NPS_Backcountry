# Grand Analysis of all sites, using approach from previous backcountry work.
# Important differences from previous work:
# 1. Many fewer possible models, since only one model for Annoy, and one for Interfere.

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

# Model run function ----

run_clmm <- function(model_no, use_survey = NULL,
                     PTAud = c('PTAudAllAC', 'lg10.PTAudAllAC'),
                     med_vars = c('ImpHistCult_VorMore','ImpNQ_VorMore','SiteFirstVisit', 'DurVisitMinutes'),
                     addl_vars = NULL,
                     GeoVar = c('Site', 'Park'),
                     res_vars = c('Annoy3', 'IntWithNQ3')) {

  # Set up variables
  
  # Add survey and additional variables to mediator variables if they exist
  if(!is.null(med_vars)) {
    if(!is.null(use_survey)) med_vars = c(med_vars, use_survey)
    if(!is.null(addl_vars)) med_vars = c(med_vars, addl_vars)
  
    formula_strings = paste(paste0(res_vars, ' ~ SELAllAC + PEnProps + PEnHelos + Dataset + SiteType'),
                         PTAud,
                         paste(med_vars, collapse = ' + '),
                         paste0('(1|', GeoVar,')'),
                         sep = ' + ')   
  } 
  
  if (is.null(med_vars) & !is.null(use_survey)) {
    # Mediators are null, add survey and addl_variables to formula string
    if(!is.null(use_survey)) sur_vars = use_survey
    if(!is.null(addl_vars)) sur_vars = c(sur_vars, addl_vars)
    
    formula_strings = paste(paste0(res_vars, ' ~ SELAllAC + PEnProps + PEnHelos + Dataset + SiteType'),
                            PTAud,
                            paste(sur_vars, collapse = ' + '),
                            paste0('(1|', GeoVar,')'),
                            sep = ' + ')  
    
  } 
  if(is.null(med_vars) & is.null(use_survey)) {

    formula_strings = paste(paste0(res_vars, ' ~ SELAllAC + PEnProps + PEnHelos + Dataset + SiteType'),
                            PTAud,
                            paste0('(1|', GeoVar,')'),
                            sep = ' + ')  
    
  } 
    
  
  # Annoy Model
  model_name = paste0('annoy_',
                      formatC(model_no, width = 2, flag = 0))
  
  mod_formula = as.formula(formula_strings[1]) 
  
  assign(model_name,
         clmm(mod_formula,
              Hess = T,
              data = dAll,
              link = "logit")
  )
  
  # summary(annoy_01)
  
  tab_model(get(model_name),
            file = file.path(output, paste0(model_name, ".html")))
  
  model_compare <- rbind(model_compare, 
                         data.frame(model_name,
                                    summary(get(model_name))$info[,c('nobs','AIC')],
                                    use_survey = ifelse(is.null(use_survey), 'NULL', use_survey),
                                    PTAud,
                                    GeoVar,
                                    med_vars = ifelse(is.null(med_vars), 'NULL', paste(med_vars, collapse = ','))))
  
  # Interfere Model
  model_name = paste0('interfere_',
                      formatC(model_no, width = 2, flag = 0))
  
  mod_formula = as.formula(formula_strings[2])
  
  assign(model_name,
         clmm(mod_formula,
              Hess = T,
              data = dAll,
              link = "logit")
  )
  
  tab_model(get(model_name),
            file = file.path(output, paste0(model_name, ".html")))
  
  model_compare <- rbind(model_compare, 
                         data.frame(model_name,
                                    summary(get(model_name))$info[,c('nobs','AIC')],
                                    use_survey = ifelse(is.null(use_survey), 'NULL', use_survey),
                                    PTAud,
                                    GeoVar,
                                    med_vars = ifelse(is.null(med_vars), 'NULL', paste(med_vars, collapse = ','))))
  
  model_compare
  
}

# To store results
model_compare <- vector()

# Model 1: Base ----

model_compare <- run_clmm(model_no = 1,
                          PTAud = 'PTAudAllAC',
                          med_vars = NULL,
                          GeoVar = 'Site')

# Model 2: Survey	----

model_compare <- run_clmm(model_no = 2,
                          PTAud = 'PTAudAllAC',
                          use_survey = 'Survey',
                          med_vars = NULL,
                          GeoVar = 'Site')


# Model 3: All Mediators ----

model_compare <- run_clmm(model_no = 3,
                          PTAud = 'PTAudAllAC',
                          use_survey = NULL,
                          med_vars = c('ImpHistCult_VorMore','ImpNQ_VorMore','SiteFirstVisit', 'DurVisitMinutes'),
                          GeoVar = 'Site')


# Model 4: Mediators + Survey  ---- 

model_compare <- run_clmm(model_no = 4,
                          PTAud = 'PTAudAllAC',
                          use_survey = 'Survey',
                          med_vars = c('ImpHistCult_VorMore','ImpNQ_VorMore','SiteFirstVisit', 'DurVisitMinutes'),
                          GeoVar = 'Site')

#	Model  5: 1 + log(PTAudAllAC)  ----

model_compare <- run_clmm(model_no = 5,
                          PTAud = 'lg10.PTAudAllAC',
                          med_vars = NULL,
                          GeoVar = 'Site')


#	Model  6: 2 + log(PTAudAllAC)  ----

model_compare <- run_clmm(model_no = 6,
                          PTAud = 'lg10.PTAudAllAC',
                          use_survey = 'Survey',
                          med_vars = NULL,
                          GeoVar = 'Site')

#	Model  7: 3 + log(PTAudAllAC)  ----


model_compare <- run_clmm(model_no = 7,
                          PTAud = 'lg10.PTAudAllAC',
                          use_survey = NULL,
                          med_vars = c('ImpHistCult_VorMore','ImpNQ_VorMore','SiteFirstVisit', 'DurVisitMinutes'),
                          GeoVar = 'Site')

#	Model  8: 4 + log(PTAudAllAC)  ---- 

model_compare <- run_clmm(model_no = 8,
                          PTAud = 'lg10.PTAudAllAC',
                          use_survey = 'Survey',
                          med_vars = c('ImpHistCult_VorMore','ImpNQ_VorMore','SiteFirstVisit', 'DurVisitMinutes'),
                          GeoVar = 'Site')

# Model 9: 1 with Park instead of Site ----

model_compare <- run_clmm(model_no = 9,
                          PTAud = 'PTAudAllAC',
                          med_vars = NULL,
                          GeoVar = 'Park')

# Model 10: 2 with Park instead of Site	----

model_compare <- run_clmm(model_no = 10,
                          PTAud = 'PTAudAllAC',
                          use_survey = 'Survey',
                          med_vars = NULL,
                          GeoVar = 'Park')


# Model 11: 3 with Park instead of Site ----

model_compare <- run_clmm(model_no = 11,
                          PTAud = 'PTAudAllAC',
                          use_survey = NULL,
                          med_vars = c('ImpHistCult_VorMore','ImpNQ_VorMore','SiteFirstVisit', 'DurVisitMinutes'),
                          GeoVar = 'Park')


# Model 12: Mediators + Survey with Park ---- 

model_compare <- run_clmm(model_no = 12,
                          PTAud = 'PTAudAllAC',
                          use_survey = 'Survey',
                          med_vars = c('ImpHistCult_VorMore','ImpNQ_VorMore','SiteFirstVisit', 'DurVisitMinutes'),
                          GeoVar = 'Park')


# Run model selection script ----

model_compare$response = unlist(lapply(strsplit(model_compare$model_name, "_"), function(x) x[[1]]))
model_compare$model_no = unlist(lapply(strsplit(as.character(model_compare$model_name), "_"), function(x) x[[2]]))


model_compare$model_name <- as.factor(model_compare$model_name)
model_compare$model_no <- as.factor(model_compare$model_no)
model_compare$response <- as.factor(model_compare$response)
model_compare$AIC <- as.numeric(model_compare$AIC)


write.csv(model_compare, file = file.path(output, 'Model_Compare.csv'), row.names = F)

library(tidyverse)

ggplot(model_compare, aes(x = model_no, y = AIC)) +
  geom_point() + 
  facet_wrap(~response + GeoVar + PTAud + use_survey, scales = 'free_y')

# source("Model_Selection_Overnight.R")

# Summary: 
# Park instead of Site is worse, stick with site but consider Park/Site nested
# log10(PTAud) can't compare by default, since different number of observations (0's omitted), need to compare with same data, but appears to be much worse
# Including survey improves every model
# Including mediators improves models a tiny bit
# Including both survey and mediators is better than just including survey or mediators
# Next steps: move backwards on indivdiaul mediators to simplify