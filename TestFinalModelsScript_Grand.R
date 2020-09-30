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

source("Plot_Predicted_Curves_CLMM.R")

library(scales) # for alpha() and muted()
library(ordinal) # for clmm

project_shared_drive = "//vntscex/DFS/Projects/PROJ-VXK600/MLB48"

output = file.path(project_shared_drive,
                   '2020 Grand Analysis',
                   'Output')

if(!dir.exists(output)){ dir.create(output) }

# For dAll data frame, 4850 observations and 32 variables
load(file.path(project_shared_drive,
          '2020 Grand Analysis',
          'GrandAnalysis_CompleteDoseVars.RData'))

## Variables to test ####
dos_vars = c('LeqAllAC', 'SELAllAC', 'PEnProps','PEnHelos', 'PTAudAllAC', 'lg10.PTAudAllAC')
dat_vars = c('Site', 'SiteType', 'Park', 'Survey')
med_vars = c('ImpHistCult_VorMore','ImpNQ_VorMore','SiteFirstVisit', 'DurVisitMinutes', 'AdultsOnly')
res_vars = c('Annoy3', 'IntWithNQ3')

##Get complete cases for AIC values

dC = dAll[complete.cases(dAll[,c('Annoy3',
                                 'LeqAllAC', #missing ~580: mostly from overlook and short hike (Fairyland, LipPt, PimaPt, PtImpl...)
                                 'SELAllAC',  #missing ~580; Same as Leq (correct - these should be NA)
                                 'PTAudAllAC',  #none missing 
                                 'lg10.PTAudAllAC', #missing ~90
                                 'PEnProps', #missing ~12
                                 'PEnHelos',  #missing ~12
                                 'ImpHistCult_VorMore', #missing ~50 #a few from several sites
                                 'ImpNQ_VorMore', #missing ~40
                                 'SiteFirstVisit', #missing ~15 
                                 'DurVisitMinutes', 
                                 'AdultsOnly',  #none missing
                                 'Dataset', #none missing
                                 'SiteType', #none missing
                                 'Site')]),]

dim(dC)

#which sites are missing which variables?
table(dC$Park, dC$Site)
table(dC$SiteType, dC$Site)
table(dAll$SiteType, dAll$Site)
table(dC$Site)

table(dAll$SiteType, dAll$Site)

RnBowPt <- dAll[which(dAll$Site == "RnBowPt"),]

##Summary tables

VarMeans <- dC %>%
  group_by(Park, Site, SiteType) %>%
  summarize(n_Site = n(),
            mean_duration_hrs = round(mean(DurVisitMinutes/60, na.rm=T),2),
           mean_SELAllAC = round(mean(SELAllAC, na.rm=T),2)
  )

write.csv(VarMeans, file.path(output, 'ParkSiteSound_Summary.csv'), row.names = F)

#### Manual model runs to compare features ####
## Annoy analysis
# Site as random - random effect for clmm (can use clm to run as not random, but makes sense for site to be random)
#Use complete dataset

##
# Base (Site random - need random effect in model)
m1 <- clmm(Annoy3 ~ SELAllAC + PEnHelos + PEnProps + (1|Site),
           data = dC,
           Hess = T,
           link = "logit") # for proportional odds mixed model

# m1 summary
summary(m1)
confint(m1)
m1$Theta
m1$info[,c('nobs','AIC')] #nobs 4117 AIC 7197.1 

plot_curves('m1')

plot_curves('m1', plot_se = F)

##
# m2(site type - fixed)
m2 <- clmm(Annoy3 ~ SELAllAC + PEnHelos + PEnProps + SiteType + (1|Site),
           data = dC,
           Hess = T,
           link = "logit") # for proportional odds mixed model

# m2 summary
summary(m2)
confint(m2)
m2$Theta
m2$info[,c('nobs','AIC')] #nobs 4117 AIC 7180.8 
## Adding site type improves model

plot_curves('m2')

plot_curves('m2', plot_se = F)

## 
# m3(park instead of site)
m3 <- clmm(Annoy3 ~ SELAllAC + PEnHelos + PEnProps + SiteType + (1|Park),
           data = dC,
           Hess = T,
           link = "logit") # for proportional odds mixed model

# m3 summary
summary(m3)
confint(m3)
m3$Theta
m3$info[,c('nobs','AIC')] #nobs 4117 AIC 7287.6 
##m2 is best - Site RE, plus site type fixed

plot_curves('m3')

plot_curves('m3', plot_se = F)

## 
# m4(add PTAuddAllAC)
m4 <- clmm(Annoy3 ~ SELAllAC + PTAudAllAC + PEnHelos + PEnProps + SiteType + (1|Site),
           data = dC,
           Hess = T,
           link = "logit") # for proportional odds mixed model

# m4 summary
summary(m4)
confint(m4)
m4$Theta
m4$info[,c('nobs','AIC')] #nobs 4117 AIC 7167.02
##m4 is best - Site RE, plus site type fixed plus PTAudAllAC

plot_curves('m4')
plot_curves('m4', plot_se = F)


## 
# m5(LeqAllAC instead of PTAuddAllAC + SELAllAC)
m5 <- clmm(Annoy3 ~ LeqAllAC + PEnHelos + PEnProps + SiteType + (1|Site),
           data = dC,
           Hess = T,
           link = "logit") # for proportional odds mixed model

# m5 summary
summary(m5)
confint(m5)
m5$Theta
m5$info[,c('nobs','AIC')] #nobs 4117 AIC 7169.74
##m4 is best by a small amount (SELAllAC + PTAudAllAC + PEnHelos + PEnProps + SiteType + (1|Site)) 

plot_curves('m5')
plot_curves('m5', plot_se = F)


## 
# m6(log(PTAudAllAC) instead of PTAuddAllAC)
m6 <- clmm(Annoy3 ~ SELAllAC + lg10.PTAudAllAC + PEnHelos + PEnProps + SiteType + (1|Site),
           data = dC,
           Hess = T,
           link = "logit") # for proportional odds mixed model

# m6 summary
summary(m6)
confint(m6)
m6$Theta
m6$info[,c('nobs','AIC')] #nobs 4117 AIC 7169.53
##m4 is best by a small amount (SELAllAC + PTAudAllAC + PEnHelos + PEnProps + SiteType + (1|Site)) 

plot_curves('m6')
plot_curves('m6', plot_se = F)


## 
# m7(Add durvisit - does not converge, do not use)
m7 <- clmm(Annoy3 ~ SELAllAC + PTAudAllAC + PEnHelos + PEnProps 
           #+ DurVisitMinutes 
           + SiteType + (1|Site),
           data = dC,
           Hess = T,
           link = "logit") # for proportional odds mixed model

# m7 summary
summary(m7)
confint(m7)
m7$Theta
m7$info[,c('nobs','AIC')] #nobs 4117 AIC 7164.92
##Keep m4

plot_curves('m7')
plot_curves('m7', plot_se = F)


## 
# m7(+ Adults only or + SiteFirstVisit)
m8 <- clmm(Annoy3 ~ SELAllAC + PTAudAllAC + PEnHelos + PEnProps 
           + SiteType 
           #+ DurVisitMinutes 
           + AdultsOnly 
           #+ SiteFirstVisit
           + (1|Site),
           data = dC,
           Hess = T,
           link = "logit") # for proportional odds mixed model

# m8 summary
summary(m8)
confint(m8)
m8$Theta
m8$info[,c('nobs','AIC')] #nobs 4117 AIC 7162.34
##m8 with AdultsOnly is better than m7 (slightly)

# m8.1 now with SiteFirstVisit 
m8.1 <- clmm(Annoy3 ~ SELAllAC + PTAudAllAC + PEnHelos + PEnProps 
           + SiteType 
           #+ DurVisitMinutes 
           #+ AdultsOnly 
           + SiteFirstVisit
           + (1|Site),
           data = dC,
           Hess = T,
           link = "logit") # for proportional odds mixed model

# m8 summary
summary(m8.1)
confint(m8.1)
m8.1$Theta
m8.1$info[,c('nobs','AIC')] #nobs 4117 AIC 7141.14
##m8 with AdultsOnly is better than m7 (slightly)

AIC(m7, m8, m8.1) # New m8.1 better than m7

## 
# m8 with Adults Only (+ ImpHist or + ImpNQ)
m9 <- clmm(Annoy3 ~ SELAllAC + PTAudAllAC + PEnHelos + PEnProps 
           + SiteType 
           #+ DurVisitMinutes 
           + AdultsOnly 
           #+ ImpHistCult_VorMore 
           + ImpNQ_VorMore
           + (1|Site),
           data = dC,
           Hess = T,
           link = "logit") # for proportional odds mixed model

# m9 summary
summary(m9)
confint(m9)
m9$Theta
m9$info[,c('nobs','AIC')] #nobs 4117 AIC  7135.48
##m9 with ImpNQ_VorMore is better than m8 by ~25 units. Best model includes Adults only and ImpNQ

# With SiteFirstVisit 
m9.1 <- clmm(Annoy3 ~ SELAllAC + PTAudAllAC + PEnHelos + PEnProps 
           + SiteType 
           #+ DurVisitMinutes 
           + AdultsOnly 
           + SiteFirstVisit 
           + ImpNQ_VorMore
           + (1|Site),
           data = dC,
           Hess = T,
           link = "logit") # for proportional odds mixed model

# m9 summary
summary(m9.1)
confint(m9.1)
m9.1$Theta
m9.1$info[,c('nobs','AIC')] #nobs 4117 AIC  7111.80

AIC(m7, m9, m9.1) # m9.1 is new best model


# model 9 summary
coef9 <- data.frame('Value' = exp(coef(m9)), 'CI' = exp(confint(m9)), 'Pval' = summary(m9)$coefficients[,4])
coef9

write.csv(coef9,
          file.path(output, 'Annoy_M9_Coef.csv'), row.names = F)

# Compile coefs; compile AIC and N ----
# Two tables to output

# Look in the environment for R objects which are of class 'clmm', these are the model objects to use
# Search by pattern 'm' followed by one or two digits. Then confirm these are class clmm
mod_list <- ls()[grep('^m\\d{1,2}', ls())] 

coef_table <- aic_table <- vector()

for(m in mod_list){
  # m = mod_list[1]
  mx <- get(m)
  
  stopifnot(class(mx) == 'clmm')
  
  # Coefficient table
  
  coefx <- data.frame('Value' = exp(coef(mx)))
  coefx$Variable = rownames(coefx)
  coefx$Model = m
  rownames(coefx) = 1:nrow(coefx)
  
  coef_table <- rbind(coef_table, coefx[c('Model', 'Variable', 'Value')])
  
  # AIC table
  
  model_formula = paste(as.character(mx$formula)[2], as.character(mx$formula)[3], sep = " ~ ")
  
  aicx <- data.frame(ModelNo = m,
                     'AIC' = AIC(mx),
                     'N' = mx$info[,c('nobs')],
                     'Model' = model_formula)
  
  aic_table <- rbind(aic_table, aicx)
  }

write.csv(aic_table,
          file.path(output, 'Annoy_AIC.csv'), row.names = F)
write.csv(coef_table,
          file.path(output, 'Annoy_Coef.csv'), row.names = F)


#### Interfere analysis ####

#### Manual model runs to compare features ####
## Interfere analysis
# Site as random
#Use complete dataset

##
# Base (m11) (Site random - need random effect in model)
m11 <- clmm(IntWithNQ3 ~ SELAllAC + PEnHelos + PEnProps + (1|Site),
           data = dC,
           Hess = T,
           link = "logit") # for proportional odds mixed model

# m11 summary
summary(m11)
confint(m11)
m11$Theta
m11$info[,c('nobs','AIC')] #nobs 4117 AIC 8100.76 

plot_curves('m11')


##
# m12(site type - fixed)
m12 <- clmm(IntWithNQ3 ~ SELAllAC + PEnHelos + PEnProps + SiteType + (1|Site),
           data = dC,
           Hess = T,
           link = "logit") # for proportional odds mixed model

# m12 summary
summary(m12)
confint(m12)
m12$Theta
m12$info[,c('nobs','AIC')] #nobs 4117 AIC 8090.27 
## Adding site type improves model

## 
# m13(park instead of site)
m13 <- clmm(IntWithNQ3 ~ SELAllAC + PEnHelos + PEnProps + SiteType + (1|Park),
           data = dC,
           Hess = T,
           link = "logit") # for proportional odds mixed model

# m13 summary
summary(m13)
confint(m13)
m13$Theta
m13$info[,c('nobs','AIC')] #nobs 4117 AIC 8209.13 
##m12 is best - Site RE, plus site type fixed

plot_curves('m13')


## 
# m14(add PTAuddAllAC)
m14 <- clmm(IntWithNQ3 ~ SELAllAC + PTAudAllAC + PEnHelos + PEnProps 
            + SiteType + (1|Site),
           data = dC,
           Hess = T,
           link = "logit") # for proportional odds mixed model

# m14 summary
summary(m14)
confint(m14)
m14$Theta
m14$info[,c('nobs','AIC')] #nobs 4117 AIC 8063.62
##m14 is best - Site RE, plus site type fixed plus PTAudAllAC

## 
# m15(LeqAllAC instead of PTAuddAllAC + SELAllAC)
m15 <- clmm(IntWithNQ3 ~ LeqAllAC + PEnHelos + PEnProps 
            + SiteType + (1|Site),
           data = dC,
           Hess = T,
           link = "logit") # for proportional odds mixed model

# m15 summary
summary(m15)
confint(m15)
m15$Theta
m15$info[,c('nobs','AIC')] #nobs 4117 AIC 8085.82
##m14 is best (SELAllAC + PTAudAllAC + PEnHelos + PEnProps + SiteType + (1|Site)) 

plot_curves('m15')

## 
# m16(log(PTAudAllAC) instead of PTAuddAllAC)
m16 <- clmm(IntWithNQ3 ~ SELAllAC + lg10.PTAudAllAC + PEnHelos + PEnProps 
            + SiteType + (1|Site),
           data = dC,
           Hess = T,
           link = "logit") # for proportional odds mixed model

# m16 summary
summary(m16)
confint(m16)
m16$Theta
m16$info[,c('nobs','AIC')] #nobs 4117 AIC 8071.44
##m14 is best by a small amount (SELAllAC + PTAudAllAC + PEnHelos + PEnProps + SiteType + (1|Site)) 

## 
# m17(Add durvisit)
m17 <- clmm(IntWithNQ3 ~ SELAllAC + PTAudAllAC + PEnHelos + PEnProps 
            + DurVisitMinutes 
            + SiteType + (1|Site),
           data = dC,
           Hess = T,
           link = "logit") # for proportional odds mixed model

# m17 summary
summary(m17)
confint(m17)
m17$Theta
m17$info[,c('nobs','AIC')] #nobs 4117 AIC 8065.32
##m14 is still best by ~ 2 units, but perhaps keep durvisit for consistency with annoy3 models

## 
# m14(+ Adults only or + SiteFirstVisit)
m18 <- clmm(IntWithNQ3 ~ SELAllAC + PTAudAllAC + PEnHelos + PEnProps 
           + SiteType 
           #+ DurVisitMinutes 
           + AdultsOnly 
           #+ SiteFirstVisit
           + (1|Site),
           data = dC,
           Hess = T,
           link = "logit") # for proportional odds mixed model

# m18 summary
summary(m18)
confint(m18)
m18$Theta
m18$info[,c('nobs','AIC')] #nobs 4117 AIC 8064.32
##m14 is still best by ~ .5 units; m18 with AdultsOnly is much worse if DurVisitMinutes is included

## 
# m14 (+ ImpHist or + ImpNQ)
m19 <- clmm(IntWithNQ3 ~ SELAllAC + PTAudAllAC + PEnHelos + PEnProps 
           + SiteType 
           #+ DurVisitMinutes 
           #+ AdultsOnly 
           #+ ImpHistCult_VorMore 
           + ImpNQ_VorMore
           + (1|Site),
           data = dC,
           Hess = T,
           link = "logit") # for proportional odds mixed model

# m19 summary
summary(m19)
confint(m19)
m19$Theta
m19$info[,c('nobs','AIC')] #nobs 4117 AIC  8034.26
##m19 with ImpNQ_VorMore is better than m14 by ~25 units. Best model includes dose plus ImpNQ

#model 19 summary
coef19 <- data.frame('Value' = exp(coef(m19)), 'CI' = exp(confint(m19)), 'Pval' = summary(m19)$coefficients[,4])
coef19

write.csv(coef19,
          file.path(output, 'Interfere_M19_Coef.csv'), row.names = F)


# Compile coefs; compile AIC and N ----
# Two tables to output

# Look in the environment for R objects which are of class 'clmm', these are the model objects to use
# Search by pattern 'm' followed by one or two digits. Then confirm these are class clmm
mod_list <- ls()[grep('^m\\d{1,2}', ls())] 

coef_table <- aic_table <- vector()

for(m in mod_list){
  # m = mod_list[1]
  mx <- get(m)
  
  stopifnot(class(mx) == 'clmm')
  
  # Coefficient table
  
  coefx <- data.frame('Value' = exp(coef(mx)))
  coefx$Variable = rownames(coefx)
  coefx$Model = m
  rownames(coefx) = 1:nrow(coefx)
  
  coef_table <- rbind(coef_table, coefx[c('Model', 'Variable', 'Value')])
  
  # AIC table
  
  model_formula = paste(as.character(mx$formula)[2], as.character(mx$formula)[3], sep = " ~ ")
  
  aicx <- data.frame(ModelNo = m,
                     'AIC' = AIC(mx),
                     'N' = mx$info[,c('nobs')],
                     'Model' = model_formula)
  
  aic_table <- rbind(aic_table, aicx)
}

write.csv(aic_table,
          file.path(output, 'Interfere_AIC.csv'), row.names = F)
write.csv(coef_table,
          file.path(output, 'Interfere_Coef.csv'), row.names = F)


# Make all plots -----

# pdf(file.path(output, 'All_CLMM_Curves.pdf'), width = 8, height = 8)
# for(m in mod_list){
#   plot_curves(m)
#   plot_curves(m, plot_se = F)
# }
# dev.off()

pdf(file.path(output, 'Final_CLMM_Curves.pdf'), width = 8, height = 8)
for(m in c('m1', 'm9', 'm11', 'm19')){
  plot_curves_95(m, plot_Not_at_all = F)
  plot_curves_95(m)
  plot_curves_95(m, plot_CI = F)
}
dev.off()
