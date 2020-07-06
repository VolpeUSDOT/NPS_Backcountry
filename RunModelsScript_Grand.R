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

run_clmm <- function(model_no, survey = NULL,
                     PTAud = c('PTAudAllAC', 'lg10.PTAudAllAC'),
                     med_vars = c('ImpHistCult_VorMore','ImpNQ_VorMore','SiteFirstVisit', 'DurVisitMinutes'),
                     addl_vars = NULL,
                     GeoVar = c('Site', 'Park'),
                     res_vars = c('Annoy3', 'IntWithNQ3')) {

  # Set up variables
  
  # Add survey and additional variables to mediator variables if they exist
  if(!is.null(med_vars)) {
    if(!is.null(survey)) med_vars = c(med_vars, survey)
    if(!is.null(addl_vars)) med_vars = c(med_vars, addl_vars)
  
    formula_strings = paste(paste0(res_vars, ' ~ SELAllAC + PEnProps + PEnHelos + Dataset + SiteType'),
                         PTAud,
                         paste(med_vars, collapse = ' + '),
                         paste0('(1|', GeoVar,')'),
                         sep = ' + ')   
  } 
  
  if (is.null(med_vars) & !is.null(survey)) {
    # Mediators are null, add survey and addl_variables to formula string
    if(!is.null(survey)) sur_vars = survey
    if(!is.null(addl_vars)) sur_vars = c(sur_vars, addl_vars)
    
    formula_strings = paste(paste0(res_vars, ' ~ SELAllAC + PEnProps + PEnHelos + Dataset + SiteType'),
                            PTAud,
                            paste(sur_vars, collapse = ' + '),
                            paste0('(1|', GeoVar,')'),
                            sep = ' + ')  
    
  } 
  if(is.null(med_vars) & is.null(survey)) {

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
                         data.frame(model_name, summary(get(model_name))$info[,c('nobs','AIC')]))
  
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
                         data.frame(model_name, summary(get(model_name))$info[,c('nobs','AIC')]))
  
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
                          survey = 'Survey',
                          med_vars = NULL,
                          GeoVar = 'Site')


# Model 3: All Mediators ----

model_compare <- run_clmm(model_no = 3,
                          PTAud = 'PTAudAllAC',
                          survey = NULL,
                          med_vars = c('ImpHistCult_VorMore','ImpNQ_VorMore','SiteFirstVisit', 'DurVisitMinutes'),
                          GeoVar = 'Site')


# Model 4: Mediators + Survey  ---- 

model_compare <- run_clmm(model_no = 4,
                          PTAud = 'PTAudAllAC',
                          survey = 'Survey',
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
                          survey = 'Survey',
                          med_vars = NULL,
                          GeoVar = 'Site')

#	Model  7: 3 + log(PTAudAllAC)  ----


model_compare <- run_clmm(model_no = 7,
                          PTAud = 'lg10.PTAudAllAC',
                          survey = NULL,
                          med_vars = c('ImpHistCult_VorMore','ImpNQ_VorMore','SiteFirstVisit', 'DurVisitMinutes'),
                          GeoVar = 'Site')

#	Model  8: 4 + log(PTAudAllAC)  ---- 

model_compare <- run_clmm(model_no = 8,
                          PTAud = 'lg10.PTAudAllAC',
                          survey = 'Survey',
                          med_vars = c('ImpHistCult_VorMore','ImpNQ_VorMore','SiteFirstVisit', 'DurVisitMinutes'),
                          GeoVar = 'Site')


# Run model selection script ----
source("Model_Selection_Overnight.R")