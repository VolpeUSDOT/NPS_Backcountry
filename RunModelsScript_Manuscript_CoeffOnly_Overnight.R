# Model select procedure for ATMP 2011 backcountry survey data:
# Started 11_12_2103 | EA Sudderth
# Modified for overnight only, 2016 | Dan Flynn
# Adapted for standalone code, 2020 | Dan Flynn

#1) Compare alternate dose models and importance of natural quiet vs calm/peace. Include single and compound doses, plus Survey and ImpNQ_VorMore OR ImpCP_VorMore.

#2) Test binary mediators (coded as 0 or 1). Add individually and in combination. Accept new mediator if average of AIC values for the three Annoy or Interfer models (SorMore, MoreMore, VorMore) drops and the regression coefficient for the mediator is significant in one more or of the models.  

# Explanation of response variables ---- 
# Axis labels for figures for each dose variable
# SELAllAC = LAE (dBA)
# LeqTresp = LAeq,Tresp (dBA)
# LeqTAC = L Aeq,Tac (dBA)
# LmaxAllAC = LASmx (dBA)
# PTAudAllAC = TAud (%)
# DprimeLSELAllAC  = D'Lcum (dB)
# DprimeLLeqTAC = D'Leq,TAC (dB)
# DprimeLeqTresp = D'Leq,Tresp (dB)
# DurAbvDprime7Minutes = Time Above D'L = 7 ( minutes)
# DprimeL90 = D'L90 (dB)
# DprimeL50 = D'L50 (dB)
# DprimeL10= D'L10 (dB)
# DurAbvDprime17Minutes = Time Above D'L = 17 ( minutes)
# %TAboveDprimeL17  = %Time Above D'L = 17

# Dayhike models applied to overnight

# Ensure required packaged are present

source("get_packages.R")

# <<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>>
# Model 1 - Annoy Dayhike final model ----
rm(list = ls())        	### Clears all variables

source("Model_Setup_Overnight.R")

STypeList = c("BCOvernight")
dose.name= "LAE (dBA)"
dose.var = "SELAllAC"
AddDose = "PTAudAllAC" #Use "none" if additional doses are not included in the model.
vars.dos = c("SELAllAC", "PTAudAllAC", "PEnHelos", "PEnProps")  ###For PEn:
vars.mit = c("Survey", "ImpCP_VorMore", "SiteVisitBefore",  "AdultsOnly", "WatchBirds", "SiteType") 
#vars.interact =	""#"I(PEnHelos * PEnProps)"	###For PEn: "I(PEnHelos * PEnProps)"
PEnRegress = TRUE
DesiredPlotX = vars.dos[1]

#Process data for Annoy response
source("ATMP_2011Overnight_LogRegPEn_Annoy_ACHR1HR2_CoeffOnly.R")

#Process data for Interfere response
source("ATMP_2011Overnight_LogRegPEn_Interfere_ACHR1HR2_CoeffOnly.R")

# <<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>>
# Model 2 - Interfere Dayhike final model ----

rm(list = ls())        	### Clears all variables
source("Model_Setup_Overnight.R")

STypeList=c("BCOvernight")
dose.name= "LAE (dBA)"
dose.var = "SELAllAC"
AddDose = "PTAudAllAC" #Use "none" if additional doses are not included in the model.
vars.dos = c("SELAllAC", "PTAudAllAC", "PEnHelos", "PEnProps")  ###For PEn:
vars.mit = c("ImpCP_VorMore", "AdultsOnly", "WatchBirds", "SiteType") #Use "none" if additional doses are not included in the model.
PEnRegress = TRUE
DesiredPlotX = vars.dos[1]

# Process data for Annoy response
source("ATMP_2011Overnight_LogRegPEn_Annoy_ACHR1HR2_CoeffOnly-nosurvey.R")

# Process data for Interfere response
source("ATMP_2011Overnight_LogRegPEn_Interfere_ACHR1HR2_CoeffOnly-nosurvey.R")

# Now alterative models, but all keeping core four variables

# <<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>>
# Model 2 ----
# SELAllAC = LAE (dBA)

  rm(list = ls())          ### Clears all variables
  source("Model_Setup_Overnight.R")
  
   dose.name= "LAE (dBA)"
   dose.var = "SELAllAC"
   AddDose = "none" 
   vars.dos = c("SELAllAC", "PEnHelos", "PEnProps","PTAudAllAC")
   vars.mit = c("Survey", "ImpNQ_VorMore", "SiteType")#, "SiteVisitBefore",  "AdultsOnly")    ###For OnlyPrior: "SiteType"
PEnRegress = TRUE
DesiredPlotX = vars.dos[1]

#Process data for Annoy response
source("ATMP_2011Overnight_LogRegPEn_Annoy_ACHR1HR2_CoeffOnly.R")

#Process data for Interfere response
source("ATMP_2011Overnight_LogRegPEn_Interfere_ACHR1HR2_CoeffOnly.R")

# <<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>>
# Model 2b ----
# SELAllAC = LAE (dBA)

rm(list = ls())          ### Clears all variables
source("Model_Setup_Overnight.R")
dose.name= "LAE (dBA)"
dose.var = "SELAllAC"
AddDose = "none" 
vars.dos = c("SELAllAC", "PEnHelos", "PEnProps","PTAudAllAC")
vars.mit = c("Survey", "ImpCP_VorMore", "SiteType")#, "SiteVisitBefore",  "AdultsOnly")   

PEnRegress = TRUE
DesiredPlotX = vars.dos[1]

#Process data for Annoy response
source("ATMP_2011Overnight_LogRegPEn_Annoy_ACHR1HR2_CoeffOnly.R")

#Process data for Interfere response
source("ATMP_2011Overnight_LogRegPEn_Interfere_ACHR1HR2_CoeffOnly.R")

# <<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>>
# Model 14 ----
# SELAllAC = LAE (dBA)

rm(list = ls())          ### Clears all variables
source("Model_Setup_Overnight.R")
dose.name= "LAE (dBA)"
dose.var = "SELAllAC"
AddDose = "none" 
vars.dos = c("SELAllAC", "PEnHelos", "PEnProps","PTAudAllAC")

vars.mit = c("Survey", "ImpNQ_VorMore", "SiteType")#, "SiteVisitBefore",  "AdultsOnly")    ###For OnlyPrior: "SiteType"

PEnRegress = TRUE
DesiredPlotX = vars.dos[1]

#Process data for Annoy response
source("ATMP_2011Overnight_LogRegPEn_Annoy_ACHR1HR2_CoeffOnly.R")

#Process data for Interfere response
source("ATMP_2011Overnight_LogRegPEn_Interfere_ACHR1HR2_CoeffOnly.R")

# <<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>>
# Model 14b ----
# SELAllAC = LAE (dBA)

rm(list = ls())          ### Clears all variables
source("Model_Setup_Overnight.R")
dose.name= "LAE (dBA)"
dose.var = "SELAllAC"
AddDose = "none" 
vars.dos = c("SELAllAC", "PEnHelos", "PEnProps","PTAudAllAC")
vars.mit = c("Survey", "ImpCP_VorMore")#, "SiteVisitBefore",  "AdultsOnly")    ###For OnlyPrior: "SiteType"
PEnRegress = TRUE
DesiredPlotX = vars.dos[1]

#Process data for Annoy response
source("ATMP_2011Overnight_LogRegPEn_Annoy_ACHR1HR2_CoeffOnly.R")

#Process data for Interfere response
source("ATMP_2011Overnight_LogRegPEn_Interfere_ACHR1HR2_CoeffOnly.R")

# <<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>>
# Model 22 ----
# SELAllAC = LAE (dBA)

rm(list = ls())          ### Clears all variables
source("Model_Setup_Overnight.R")
dose.name= "LAE (dBA)"
dose.var = "SELAllAC"
AddDose = "lg10.PTAudAllAC" #Use "none" if additional doses are not included in the model.
vars.dos = c("SELAllAC", "lg10.PTAudAllAC", "PEnHelos", "PEnProps")  ###For PEn:
vars.mit = c("Survey", "ImpNQ_VorMore", "SiteVisitBefore",  "AdultsOnly")    ###For OnlyPrior: "SiteType"
PEnRegress = TRUE
DesiredPlotX = vars.dos[1]

#Process data for Annoy response 
source("ATMP_2011Overnight_LogRegPEn_Annoy_ACHR1HR2_CoeffOnly.R")  

#Process data for Interfere response 
source("ATMP_2011Overnight_LogRegPEn_Interfere_ACHR1HR2_CoeffOnly.R")

# <<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>>
# Model 22b ----
# SELAllAC = LAE (dBA)

rm(list = ls())          ### Clears all variables
source("Model_Setup_Overnight.R")
dose.name= "LAE (dBA)"
dose.var = "SELAllAC"
AddDose = "lg10.PTAudAllAC" #Use "none" if additional doses are not included in the model.
vars.dos = c("SELAllAC", "lg10.PTAudAllAC", "PEnHelos", "PEnProps")  ###For PEn:
vars.mit = c("Survey", "ImpCP_VorMore", "SiteVisitBefore",  "AdultsOnly")    ###For OnlyPrior: "SiteType"
PEnRegress = TRUE
DesiredPlotX = vars.dos[1]

#Process data for Annoy response 
source("ATMP_2011Overnight_LogRegPEn_Annoy_ACHR1HR2_CoeffOnly.R")  

#Process data for Interfere response 
source("ATMP_2011Overnight_LogRegPEn_Interfere_ACHR1HR2_CoeffOnly.R")


# Summary ----
# Use best model from above to Test fewer/additional mediators
# Best model = SELAllAC + PTAudAllAC + PEnHelos + PEnProps + Survey +ImpCPVorMore
# Notes: 
#  ImpNQ_VorMore also among best models: test both Importance variables in additional models. 
#  Omit PEn interaction: not significant in any of the best dose models
#  Omit the survey variable for the Interfere models: not significant for any of the best dose models.

# <<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>>
# Best models with ImpNQ and ImpCP, PEn interaction removed, and Survey removed for Interfere responses
# Annoy and Interfere models run separately from here forward.

# Model 30 (skip 23-29 in case more need to be added earlier)
# SELAllAC = LAE (dBA)
# Annoy only

# <<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>>
# Model 31: interfere only ----

rm(list = ls())          ### Clears all variables
source("Model_Setup_Overnight.R")
dose.name= "LAE (dBA)"
dose.var = "SELAllAC"
AddDose = "PTAudAllAC" #Use "none" if additional doses are not included in the model.
vars.dos = c("SELAllAC", "PTAudAllAC", "PEnHelos", "PEnProps")  ###For PEn:
vars.mit = c("ImpNQ_VorMore")    ###For OnlyPrior: "SiteType"
vars.interact =  NULL #c("I(PEnHelos * PEnProps)")  ###For PEn: "I(PEnHelos * PEnProps)"
PEnRegress = TRUE
DesiredPlotX = vars.dos[1]
source("ATMP_2011Overnight_LogRegPEn_Interfere_CoeffOnly.R")

# <<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>>
# Model 31b: interfere only ----

rm(list = ls())          ### Clears all variables
source("Model_Setup_Overnight.R")
dose.name= "LAE (dBA)"
dose.var = "SELAllAC"
AddDose = "PTAudAllAC" #Use "none" if additional doses are not included in the model.
vars.dos = c("SELAllAC", "PTAudAllAC", "PEnHelos", "PEnProps")  ###For PEn:
vars.mit = c("ImpCP_VorMore")    ###For OnlyPrior: "SiteType"
vars.interact =  NULL #c("I(PEnHelos * PEnProps)")  ###For PEn: "I(PEnHelos * PEnProps)"
PEnRegress = TRUE
DesiredPlotX = vars.dos[1]
source("ATMP_2011Overnight_LogRegPEn_Interfere_CoeffOnly.R")

# <<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>>
# Try using ImpViewScenery instead of ImpNQ or ImpCP:
# Model 32 ----
# SELAllAC = LAE (dBA)
#Annoy only

rm(list = ls())          ### Clears all variables
source("Model_Setup_Overnight.R")
dose.name= "LAE (dBA)"
dose.var = "SELAllAC"
AddDose = "PTAudAllAC" #Use "none" if additional doses are not included in the model.
vars.dos = c("SELAllAC", "PTAudAllAC", "PEnHelos", "PEnProps")  ###For PEn:
vars.mit = c("Survey", "ImpVS_VorMore")    ###For OnlyPrior: "SiteType"
vars.interact =  NULL #c("I(PEnHelos * PEnProps)")  ###For PEn: "I(PEnHelos * PEnProps)"
PEnRegress = TRUE
DesiredPlotX = vars.dos[1]
source("ATMP_2011Overnight_LogRegPEn_Annoy_ACHR1HR2_CoeffOnly.R")

# <<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>>
# Model 33: interfere only ----

rm(list = ls())          ### Clears all variables
source("Model_Setup_Overnight.R")
dose.name= "LAE (dBA)"
dose.var = "SELAllAC"
AddDose = "PTAudAllAC" #Use "none" if additional doses are not included in the model.
vars.dos = c("SELAllAC", "PTAudAllAC", "PEnHelos", "PEnProps")  ###For PEn:
vars.mit = c("ImpVS_VorMore")    ###For OnlyPrior: "SiteType"
vars.interact =  NULL #c("I(PEnHelos * PEnProps)")  ###For PEn: "I(PEnHelos * PEnProps)"
PEnRegress = TRUE
DesiredPlotX = vars.dos[1]
source("ATMP_2011Overnight_LogRegPEn_Interfere_CoeffOnly.R")

# <<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>>
# Reject ImpVS - AIC values are higher for all responses compared to ImpNQ and ImpCP.
# Note: ImpCP has the lowest AIC values for all models except AnnoySorMore and Interfere SorMore - keep both for now.  

# Test EarlyStart ----
rm(list = ls())          ### Clears all variables
source("Model_Setup_Overnight.R")
dose.name= "LAE (dBA)"
dose.var = "SELAllAC"
AddDose = "PTAudAllAC" #Use "none" if additional doses are not included in the model.
vars.dos = c("SELAllAC", "PTAudAllAC", "PEnHelos", "PEnProps")  ###For PEn:
vars.mit = c("Survey", "ImpNQ_VorMore", "EarlyStart")    ###For OnlyPrior: "SiteType"
vars.interact =  NULL #c("I(PEnHelos * PEnProps)")  ###For PEn: "I(PEnHelos * PEnProps)"
PEnRegress = TRUE
DesiredPlotX = vars.dos[1]
source("ATMP_2011Overnight_LogRegPEn_Annoy_ACHR1HR2_CoeffOnly.R")

rm(list = ls())          ### Clears all variables
source("Model_Setup_Overnight.R")
dose.name= "LAE (dBA)"
dose.var = "SELAllAC"
AddDose = "PTAudAllAC" #Use "none" if additional doses are not included in the model.
vars.dos = c("SELAllAC", "PTAudAllAC", "PEnHelos", "PEnProps")  ###For PEn:
vars.mit = c("ImpNQ_VorMore", "EarlyStart")    ###For OnlyPrior: "SiteType"
vars.interact =  NULL #c("I(PEnHelos * PEnProps)")  ###For PEn: "I(PEnHelos * PEnProps)"
PEnRegress = TRUE
DesiredPlotX = vars.dos[1]
source("ATMP_2011Overnight_LogRegPEn_Interfere_CoeffOnly.R")

rm(list = ls())          ### Clears all variables
source("Model_Setup_Overnight.R")
dose.name= "LAE (dBA)"
dose.var = "SELAllAC"
AddDose = "PTAudAllAC" #Use "none" if additional doses are not included in the model.
vars.dos = c("SELAllAC", "PTAudAllAC", "PEnHelos", "PEnProps")  ###For PEn:
vars.mit = c("ImpCP_VorMore", "EarlyStart")    ###For OnlyPrior: "SiteType"
vars.interact =  NULL #c("I(PEnHelos * PEnProps)")  ###For PEn: "I(PEnHelos * PEnProps)"
PEnRegress = TRUE
DesiredPlotX = vars.dos[1]
source("ATMP_2011Overnight_LogRegPEn_Interfere_CoeffOnly.R")

## REJECT EarlyStart: AIC only in/decreases by 1 for all and the regression coefficients are not significant for any response.

# <<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>>
# Test additional mediators in the following order: 
# SiteVisitBefore, AdultsOnly, AirTour, WatchBirds, ViewSunRiseSet, PicnicMeal, Talk
# Accept if average AIC values decline significantly and regression coefficient is significant for one or more responses. 

# Model 34: Annoy only ----
# SELAllAC = LAE (dBA)
#Best Model: vars.dos = c("SELAllAC", "PTAudAllAC", "PEnHelos", "PEnProps"), vars.mit = c("Survey", "ImpCP_VorMore" OR "ImpNQ_VorMore")  
#Add SiteVisit?

rm(list = ls())          ### Clears all variables
source("Model_Setup_Overnight.R")
dose.name= "LAE (dBA)"
dose.var = "SELAllAC"
AddDose = "PTAudAllAC" #Use "none" if additional doses are not included in the model.
vars.dos = c("SELAllAC", "PTAudAllAC", "PEnHelos", "PEnProps")  ###For PEn:
vars.mit = c("Survey", "ImpNQ_VorMore","SiteVisitBefore")    ###For OnlyPrior: "SiteType"
vars.interact =  NULL #c("I(PEnHelos * PEnProps)")  ###For PEn: "I(PEnHelos * PEnProps)"
PEnRegress = TRUE
DesiredPlotX = vars.dos[1]
source("ATMP_2011Overnight_LogRegPEn_Annoy_ACHR1HR2_CoeffOnly.R")

# <<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>>
# Model 34b: Annoy only ----
# SELAllAC = LAE (dBA)
#Best Model: vars.dos = c("SELAllAC", "PTAudAllAC", "PEnHelos", "PEnProps"), vars.mit = c("Survey", "ImpCP_VorMore" OR "ImpNQ_VorMore")  
#Add SiteVisit?

rm(list = ls())          ### Clears all variables
source("Model_Setup_Overnight.R")
dose.name= "LAE (dBA)"
dose.var = "SELAllAC"
AddDose = "PTAudAllAC" #Use "none" if additional doses are not included in the model.
vars.dos = c("SELAllAC", "PTAudAllAC", "PEnHelos", "PEnProps")  ###For PEn:
vars.mit = c("Survey", "ImpCP_VorMore","SiteVisitBefore")    ###For OnlyPrior: "SiteType"
vars.interact =  NULL #c("I(PEnHelos * PEnProps)")  ###For PEn: "I(PEnHelos * PEnProps)"
PEnRegress = TRUE
DesiredPlotX = vars.dos[1]
source("ATMP_2011Overnight_LogRegPEn_Annoy_ACHR1HR2_CoeffOnly.R")

# <<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>>
# Model 35: interfere only ----
# SELAllAC = LAE (dBA)
#Best Model: vars.dos = c("SELAllAC", "PTAudAllAC", "PEnHelos", "PEnProps"), vars.mit = c("Survey", "ImpCP_VorMore"OR "ImpNQ_VorMore")  
#Add SiteVisit?

rm(list = ls())          ### Clears all variables
source("Model_Setup_Overnight.R")
dose.name= "LAE (dBA)"
dose.var = "SELAllAC"
AddDose = "PTAudAllAC" #Use "none" if additional doses are not included in the model.
vars.dos = c("SELAllAC", "PTAudAllAC", "PEnHelos", "PEnProps")  ###For PEn:
vars.mit = c("ImpNQ_VorMore", "SiteVisitBefore")    ###For OnlyPrior: "SiteType"
vars.interact =  NULL #c("I(PEnHelos * PEnProps)")  ###For PEn: "I(PEnHelos * PEnProps)"
PEnRegress = TRUE
DesiredPlotX = vars.dos[1]
source("ATMP_2011Overnight_LogRegPEn_Interfere_CoeffOnly.R")

# <<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>>
# Model 35b: interfere only ----
# SELAllAC = LAE (dBA)
#Best Model: vars.dos = c("SELAllAC", "PTAudAllAC", "PEnHelos", "PEnProps"), vars.mit = c("Survey", "ImpCP_VorMore"OR "ImpNQ_VorMore")  
#Add SiteVisit?

rm(list = ls())          ### Clears all variables
source("Model_Setup_Overnight.R")
dose.name= "LAE (dBA)"
dose.var = "SELAllAC"
AddDose = "PTAudAllAC" #Use "none" if additional doses are not included in the model.
vars.dos = c("SELAllAC", "PTAudAllAC", "PEnHelos", "PEnProps")  ###For PEn:
vars.mit = c("ImpCP_VorMore", "SiteVisitBefore")    ###For OnlyPrior: "SiteType"
vars.interact =  NULL #c("I(PEnHelos * PEnProps)")  ###For PEn: "I(PEnHelos * PEnProps)"
PEnRegress = TRUE
DesiredPlotX = vars.dos[1]
source("ATMP_2011Overnight_LogRegPEn_Interfere_CoeffOnly.R")

# <<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>>
# For Annoy: ACCEPT SiteVisitBefore AND Use ImpCP for remainder of models: AIC value is the same for SorMore but 5-6 units lower for MoreMore and VorMore.
# For Interfere REJECT SiteVisitBefore but continue testing ImpNQ AND ImpCP 
# NOTE: Test Annoy and Interfere models separately from here forward.

# <<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>>
# Test mediators for Annoy models

# Model 36: Annoy only ----
#Test AdultsOnly
rm(list = ls())          ### Clears all variables
source("Model_Setup_Overnight.R")
dose.name= "LAE (dBA)"
dose.var = "SELAllAC"
AddDose = "PTAudAllAC" #Use "none" if additional doses are not included in the model.
vars.dos = c("SELAllAC", "PTAudAllAC", "PEnHelos", "PEnProps")  ###For PEn:
vars.mit = c("Survey", "ImpCP_VorMore","SiteVisitBefore", "AdultsOnly")    ###For OnlyPrior: "SiteType"
vars.interact =  NULL #c("I(PEnHelos * PEnProps)")  ###For PEn: "I(PEnHelos * PEnProps)"
PEnRegress = TRUE
DesiredPlotX = vars.dos[1]
source("ATMP_2011Overnight_LogRegPEn_Annoy_ACHR1HR2_CoeffOnly.R")

##Accept AdultsOnly: AIC drops by 9 for SorMore, Increases by 2 for Mm and Vm and the regression coefficient is significant for SorMore.

# <<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>>
# Model 36: Annoy only ----
#Test AirTour
rm(list = ls())          ### Clears all variables
source("Model_Setup_Overnight.R")
dose.name= "LAE (dBA)"
dose.var = "SELAllAC"
AddDose = "PTAudAllAC" #Use "none" if additional doses are not included in the model.
vars.dos = c("SELAllAC", "PTAudAllAC", "PEnHelos", "PEnProps")  ###For PEn:
vars.mit = c("Survey", "ImpCP_VorMore","SiteVisitBefore", "AdultsOnly", "AirTour")    ###For OnlyPrior: "SiteType"
vars.interact =  NULL #c("I(PEnHelos * PEnProps)")  ###For PEn: "I(PEnHelos * PEnProps)"
PEnRegress = TRUE
DesiredPlotX = vars.dos[1]
source("ATMP_2011Overnight_LogRegPEn_Annoy_ACHR1HR2_CoeffOnly.R")

##Accept AirTour: AIC increases by 2 for Sm, drops by 10 for Mm and by 5 for Vm. The regression coefficients are significant for Mm and Vm.

# <<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>>
# Model 37: Annoy only ----
#Test WatchBirds
rm(list = ls())          ### Clears all variables
source("Model_Setup_Overnight.R")
dose.name= "LAE (dBA)"
dose.var = "SELAllAC"
AddDose = "PTAudAllAC" #Use "none" if additional doses are not included in the model.
vars.dos = c("SELAllAC", "PTAudAllAC", "PEnHelos", "PEnProps")  ###For PEn:
vars.mit = c("Survey", "ImpCP_VorMore","SiteVisitBefore", "AdultsOnly", "AirTour", "WatchBirds")    ###For OnlyPrior: "SiteType"
vars.interact =  NULL #c("I(PEnHelos * PEnProps)")  ###For PEn: "I(PEnHelos * PEnProps)"
PEnRegress = TRUE
DesiredPlotX = vars.dos[1]
source("ATMP_2011Overnight_LogRegPEn_Annoy_ACHR1HR2_CoeffOnly.R")

## Accept WatchBirds: AIC drops by 6 for Sm, stays same for Mm and Vm and the regression coefficient is significant for SorMore.

# <<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>>
# Model 38: Annoy only ----
#Test ViewSunRiseSet
rm(list = ls())          ### Clears all variables
source("Model_Setup_Overnight.R")
dose.name= "LAE (dBA)"
dose.var = "SELAllAC"
AddDose = "PTAudAllAC" #Use "none" if additional doses are not included in the model.
vars.dos = c("SELAllAC", "PTAudAllAC", "PEnHelos", "PEnProps")  ###For PEn:
vars.mit = c("Survey", "ImpCP_VorMore","SiteVisitBefore", "AdultsOnly", "AirTour", "WatchBirds", "ViewSunRiseSet")    ###For OnlyPrior: "SiteType"
vars.interact =  NULL #c("I(PEnHelos * PEnProps)")  ###For PEn: "I(PEnHelos * PEnProps)"
PEnRegress = TRUE
DesiredPlotX = vars.dos[1]
source("ATMP_2011Overnight_LogRegPEn_Annoy_ACHR1HR2_CoeffOnly.R")

## REJECT ViewSunRiseSet: AIC increases by 1-2 for all.

# <<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>>
# Model 39: Annoy only ----
# Test PicnicMeal
rm(list = ls())          ### Clears all variables
source("Model_Setup_Overnight.R")
dose.name= "LAE (dBA)"
dose.var = "SELAllAC"
AddDose = "PTAudAllAC" #Use "none" if additional doses are not included in the model.
vars.dos = c("SELAllAC", "PTAudAllAC", "PEnHelos", "PEnProps")  ###For PEn:
vars.mit = c("Survey", "ImpCP_VorMore","SiteVisitBefore", "AdultsOnly", "AirTour", "WatchBirds", "PicnicMeal")    ###For OnlyPrior: "SiteType"
vars.interact =  NULL #c("I(PEnHelos * PEnProps)")  ###For PEn: "I(PEnHelos * PEnProps)"
PEnRegress = TRUE
DesiredPlotX = vars.dos[1]
source("ATMP_2011Overnight_LogRegPEn_Annoy_ACHR1HR2_CoeffOnly.R")

##REJECT PicnicMeal: AIC only decreases by 1 for all and the regression coefficients are not significant for any response.

# <<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>>
# Model 40: Annoy only ----
#Test Talk
rm(list = ls())          ### Clears all variables
source("Model_Setup_Overnight.R")
dose.name= "LAE (dBA)"
dose.var = "SELAllAC"
AddDose = "PTAudAllAC" #Use "none" if additional doses are not included in the model.
vars.dos = c("SELAllAC", "PTAudAllAC", "PEnHelos", "PEnProps")  ###For PEn:
vars.mit = c("Survey", "ImpCP_VorMore","SiteVisitBefore", "AdultsOnly", "AirTour", "WatchBirds", "Talk")    ###For OnlyPrior: "SiteType"
vars.interact =  NULL #c("I(PEnHelos * PEnProps)")  ###For PEn: "I(PEnHelos * PEnProps)"
PEnRegress = TRUE
DesiredPlotX = vars.dos[1]
source("ATMP_2011Overnight_LogRegPEn_Annoy_ACHR1HR2_CoeffOnly.R")

##REJECT Talk: AIC only decreases or increases by 1 for all and the regression coefficients are not significant for any response.

# <<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>>
# Model 41: Annoy only ----
#Test ViewWildlife
rm(list = ls())          ### Clears all variables
source("Model_Setup_Overnight.R")
dose.name= "LAE (dBA)"
dose.var = "SELAllAC"
AddDose = "PTAudAllAC" #Use "none" if additional doses are not included in the model.
vars.dos = c("SELAllAC", "PTAudAllAC", "PEnHelos", "PEnProps")  ###For PEn:
vars.mit = c("Survey", "ImpCP_VorMore","SiteVisitBefore", "AdultsOnly", "AirTour", "WatchBirds", "ViewWildlife")    ###For OnlyPrior: "SiteType"
vars.interact =  NULL #c("I(PEnHelos * PEnProps)")  ###For PEn: "I(PEnHelos * PEnProps)"
PEnRegress = TRUE
DesiredPlotX = vars.dos[1]
source("ATMP_2011Overnight_LogRegPEn_Annoy_ACHR1HR2_CoeffOnly.R")

##REJECT ViewWildlife: AIC only decreases or increases by 1 for all and the regression coefficients are not significant for any response.

# <<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>>
# Model 41: Annoy only ----
#Test log10(DurVisitMinutes)
rm(list = ls())          ### Clears all variables
source("Model_Setup_Overnight.R")
dose.name= "LAE (dBA)"
dose.var = "SELAllAC"
AddDose = "PTAudAllAC" #Use "none" if additional doses are not included in the model.
vars.dos = c("SELAllAC", "PTAudAllAC", "PEnHelos", "PEnProps")  ###For PEn:
vars.mit = c("Survey", "ImpCP_VorMore","SiteVisitBefore", "AdultsOnly", "AirTour", "WatchBirds", "lg10.DurVisitMinutes")    ###For OnlyPrior: "SiteType"
vars.interact =  NULL #c("I(PEnHelos * PEnProps)")  ###For PEn: "I(PEnHelos * PEnProps)"
PEnRegress = TRUE
DesiredPlotX = vars.dos[1]
source("ATMP_2011Overnight_LogRegPEn_Annoy_ACHR1HR2_CoeffOnly.R")

##REJECT lg10.DurVisitMinutes: AIC decreases by 1-4 but the regression coefficients are not significant for any response.

# <<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>>
# Annoy final model: Include SEL, PTAud, PEnHelos, PEnProps, Survey, SIteVisitBefore, AdultsOnly, AirTour, WatchBirds

# <<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>>
# Test Interfere responses: Include ImpNQ and ImpCP
# Test AdultsOnly

# Model 42: interfere only ----
rm(list = ls())          ### Clears all variables
source("Model_Setup_Overnight.R")
dose.name= "LAE (dBA)"
dose.var = "SELAllAC"
AddDose = "PTAudAllAC" #Use "none" if additional doses are not included in the model.
vars.dos = c("SELAllAC", "PTAudAllAC", "PEnHelos", "PEnProps")  ###For PEn:
vars.mit = c("ImpNQ_VorMore", "AdultsOnly")    ###For OnlyPrior: "SiteType"
vars.interact =  NULL #c("I(PEnHelos * PEnProps)")  ###For PEn: "I(PEnHelos * PEnProps)"
PEnRegress = TRUE
DesiredPlotX = vars.dos[1]
source("ATMP_2011Overnight_LogRegPEn_Interfere_CoeffOnly.R")

# Model 42b: interfere only ----
rm(list = ls())          ### Clears all variables
source("Model_Setup_Overnight.R")
dose.name= "LAE (dBA)"
dose.var = "SELAllAC"
AddDose = "PTAudAllAC" #Use "none" if additional doses are not included in the model.
vars.dos = c("SELAllAC", "PTAudAllAC", "PEnHelos", "PEnProps")  ###For PEn:
vars.mit = c("ImpCP_VorMore", "AdultsOnly")    ###For OnlyPrior: "SiteType"
vars.interact =  NULL #c("I(PEnHelos * PEnProps)")  ###For PEn: "I(PEnHelos * PEnProps)"
PEnRegress = TRUE
DesiredPlotX = vars.dos[1]
source("ATMP_2011Overnight_LogRegPEn_Interfere_CoeffOnly.R")

##Accept AdultsOnly: AIC drops by 2-3 for SorMore, Increases by 1 for Mm and Vm and the regression coefficient is significant for SorMore.

#Note: ImpNQ has lower AIC for Sm (1747 vs 1752) while ImpCP has lower AIC for Mm and Vm (1426 and 945 vs 1432 and 950)


# <<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>>
#Test AirTour

# Model 43: interfere only ----
rm(list = ls())          ### Clears all variables
source("Model_Setup_Overnight.R")
dose.name= "LAE (dBA)"
dose.var = "SELAllAC"
AddDose = "PTAudAllAC" #Use "none" if additional doses are not included in the model.
vars.dos = c("SELAllAC", "PTAudAllAC", "PEnHelos", "PEnProps")  ###For PEn:
vars.mit = c("ImpNQ_VorMore", "AdultsOnly", "AirTour")    ###For OnlyPrior: "SiteType"
vars.interact =  NULL #c("I(PEnHelos * PEnProps)")  ###For PEn: "I(PEnHelos * PEnProps)"
PEnRegress = TRUE
DesiredPlotX = vars.dos[1]
source("ATMP_2011Overnight_LogRegPEn_Interfere_CoeffOnly.R")

# Model 43b: interfere only ----
rm(list = ls())          ### Clears all variables
source("Model_Setup_Overnight.R")
dose.name= "LAE (dBA)"
dose.var = "SELAllAC"
AddDose = "PTAudAllAC" #Use "none" if additional doses are not included in the model.
vars.dos = c("SELAllAC", "PTAudAllAC", "PEnHelos", "PEnProps")  ###For PEn:
vars.mit = c("ImpCP_VorMore", "AdultsOnly", "AirTour")    ###For OnlyPrior: "SiteType"
vars.interact =  NULL #c("I(PEnHelos * PEnProps)")  ###For PEn: "I(PEnHelos * PEnProps)"
PEnRegress = TRUE
DesiredPlotX = vars.dos[1]
source("ATMP_2011Overnight_LogRegPEn_Interfere_CoeffOnly.R")

##Accept AirTour: AIC drops by 1 for all responses and the regression coefficient is significant for VorMore (and close to significant for other responses.

# <<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>>
# Test WatchBirds

# Model 44: interfere only ----
rm(list = ls())          ### Clears all variables
source("Model_Setup_Overnight.R")
dose.name= "LAE (dBA)"
dose.var = "SELAllAC"
AddDose = "PTAudAllAC" #Use "none" if additional doses are not included in the model.
vars.dos = c("SELAllAC", "PTAudAllAC", "PEnHelos", "PEnProps")  ###For PEn:
vars.mit = c("ImpNQ_VorMore", "AdultsOnly", "AirTour", "WatchBirds")    ###For OnlyPrior: "SiteType"
vars.interact =  NULL #c("I(PEnHelos * PEnProps)")  ###For PEn: "I(PEnHelos * PEnProps)"
PEnRegress = TRUE
DesiredPlotX = vars.dos[1]
source("ATMP_2011Overnight_LogRegPEn_Interfere_CoeffOnly.R")

# Model 44b: interfere only ----
rm(list = ls())          ### Clears all variables
source("Model_Setup_Overnight.R")
dose.name= "LAE (dBA)"
dose.var = "SELAllAC"
AddDose = "PTAudAllAC" #Use "none" if additional doses are not included in the model.
vars.dos = c("SELAllAC", "PTAudAllAC", "PEnHelos", "PEnProps")  ###For PEn:
vars.mit = c("ImpCP_VorMore", "AdultsOnly", "AirTour", "WatchBirds")    ###For OnlyPrior: "SiteType"
vars.interact =  NULL #c("I(PEnHelos * PEnProps)")  ###For PEn: "I(PEnHelos * PEnProps)"
PEnRegress = TRUE
DesiredPlotX = vars.dos[1]
source("ATMP_2011Overnight_LogRegPEn_Interfere_CoeffOnly.R")

## Reject WatchBirds: AIC drops by ~2 for Sm but increases for Mm and Vm. The regression coefficient is significant for SorMore, but significance of other coefficients is reduced.

# <<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>>
#Test ViewSunRiseSet

# Model 45: interfere only ----
rm(list = ls())          ### Clears all variables
source("Model_Setup_Overnight.R")
dose.name= "LAE (dBA)"
dose.var = "SELAllAC"
AddDose = "PTAudAllAC" #Use "none" if additional doses are not included in the model.
vars.dos = c("SELAllAC", "PTAudAllAC", "PEnHelos", "PEnProps")  ###For PEn:
vars.mit = c("ImpNQ_VorMore", "AdultsOnly", "AirTour", "ViewSunRiseSet")    ###For OnlyPrior: "SiteType"
vars.interact =  NULL #c("I(PEnHelos * PEnProps)")  ###For PEn: "I(PEnHelos * PEnProps)"
PEnRegress = TRUE
DesiredPlotX = vars.dos[1]
source("ATMP_2011Overnight_LogRegPEn_Interfere_CoeffOnly.R")

# Model 45b: interfere only ----
rm(list = ls())          ### Clears all variables
source("Model_Setup_Overnight.R")
dose.name= "LAE (dBA)"
dose.var = "SELAllAC"
AddDose = "PTAudAllAC" #Use "none" if additional doses are not included in the model.
vars.dos = c("SELAllAC", "PTAudAllAC", "PEnHelos", "PEnProps")  ###For PEn:
vars.mit = c("ImpCP_VorMore", "AdultsOnly", "AirTour", "ViewSunRiseSet")    ###For OnlyPrior: "SiteType"
vars.interact =  NULL #c("I(PEnHelos * PEnProps)")  ###For PEn: "I(PEnHelos * PEnProps)"
PEnRegress = TRUE
DesiredPlotX = vars.dos[1]
source("ATMP_2011Overnight_LogRegPEn_Interfere_CoeffOnly.R")

##Reject ViewSunRiseSet: AIC increases by ~2 for Sm and Vm, decreases for Mm. The regression coefficients not significant.

# <<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>>
#Test PicnicMeal

# Model 46: interfere only ----
rm(list = ls())          ### Clears all variables
source("Model_Setup_Overnight.R")
dose.name= "LAE (dBA)"
dose.var = "SELAllAC"
AddDose = "PTAudAllAC" #Use "none" if additional doses are not included in the model.
vars.dos = c("SELAllAC", "PTAudAllAC", "PEnHelos", "PEnProps")  ###For PEn:
vars.mit = c("ImpNQ_VorMore", "AdultsOnly", "AirTour", "PicnicMeal")    ###For OnlyPrior: "SiteType"
vars.interact =  NULL #c("I(PEnHelos * PEnProps)")  ###For PEn: "I(PEnHelos * PEnProps)"
PEnRegress = TRUE
DesiredPlotX = vars.dos[1]
source("ATMP_2011Overnight_LogRegPEn_Interfere_CoeffOnly.R")

# Model 46b: interfere only ----
rm(list = ls())          ### Clears all variables
source("Model_Setup_Overnight.R")
dose.name= "LAE (dBA)"
dose.var = "SELAllAC"
AddDose = "PTAudAllAC" #Use "none" if additional doses are not included in the model.
vars.dos = c("SELAllAC", "PTAudAllAC", "PEnHelos", "PEnProps")  ###For PEn:
vars.mit = c("ImpCP_VorMore", "AdultsOnly", "AirTour", "PicnicMeal")    ###For OnlyPrior: "SiteType"
vars.interact =  NULL #c("I(PEnHelos * PEnProps)")  ###For PEn: "I(PEnHelos * PEnProps)"
PEnRegress = TRUE
DesiredPlotX = vars.dos[1]
source("ATMP_2011Overnight_LogRegPEn_Interfere_CoeffOnly.R")

##Reject PicnicMeal: AIC increases by ~2 for all.

# <<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>>
#Test Talk

# Model 47: interfere only ----
rm(list = ls())          ### Clears all variables
source("Model_Setup_Overnight.R")
dose.name= "LAE (dBA)"
dose.var = "SELAllAC"
AddDose = "PTAudAllAC" #Use "none" if additional doses are not included in the model.
vars.dos = c("SELAllAC", "PTAudAllAC", "PEnHelos", "PEnProps")  ###For PEn:
vars.mit = c("ImpNQ_VorMore", "AdultsOnly", "AirTour", "Talk")    ###For OnlyPrior: "SiteType"
vars.interact =  NULL #c("I(PEnHelos * PEnProps)")  ###For PEn: "I(PEnHelos * PEnProps)"
PEnRegress = TRUE
DesiredPlotX = vars.dos[1]
source("ATMP_2011Overnight_LogRegPEn_Interfere_CoeffOnly.R")


# Model 47b: interfere only ----
rm(list = ls())          ### Clears all variables
source("Model_Setup_Overnight.R")
dose.name= "LAE (dBA)"
dose.var = "SELAllAC"
AddDose = "PTAudAllAC" #Use "none" if additional doses are not included in the model.
vars.dos = c("SELAllAC", "PTAudAllAC", "PEnHelos", "PEnProps")  ###For PEn:
vars.mit = c("ImpCP_VorMore", "AdultsOnly", "AirTour", "Talk")    ###For OnlyPrior: "SiteType"
vars.interact =  NULL #c("I(PEnHelos * PEnProps)")  ###For PEn: "I(PEnHelos * PEnProps)"
PEnRegress = TRUE
DesiredPlotX = vars.dos[1]
source("ATMP_2011Overnight_LogRegPEn_Interfere_CoeffOnly.R")

## KEEP Talk: AIC decreases by ~5 for Vm and 1-2 for Sm and Mm. Regression coefficients significant for Vm and close for Sm.

# Make plots for the best models

source("ATMP_2011Overnight_LogRegPEn_Interfere_CoeffOnly.R")

# <<>><<>><<>><<>><<>><<>><<>><<>><<>>
# Test ViewWildlife

# Model 48: interfere only ----
rm(list = ls())          ### Clears all variables
source("Model_Setup_Overnight.R")
dose.name= "LAE (dBA)"
dose.var = "SELAllAC"
AddDose = "PTAudAllAC" #Use "none" if additional doses are not included in the model.
vars.dos = c("SELAllAC", "PTAudAllAC", "PEnHelos", "PEnProps")  ###For PEn:
vars.mit = c("ImpNQ_VorMore", "AdultsOnly", "AirTour", "Talk", "ViewWildlife")    ###For OnlyPrior: "SiteType"
vars.interact =  NULL #c("I(PEnHelos * PEnProps)")  ###For PEn: "I(PEnHelos * PEnProps)"
PEnRegress = TRUE
DesiredPlotX = vars.dos[1]
source("ATMP_2011Overnight_LogRegPEn_Interfere_CoeffOnly.R")

# Model 48b: interfere only ----
rm(list = ls())          ### Clears all variables
source("Model_Setup_Overnight.R")
dose.name= "LAE (dBA)"
dose.var = "SELAllAC"
AddDose = "PTAudAllAC" #Use "none" if additional doses are not included in the model.
vars.dos = c("SELAllAC", "PTAudAllAC", "PEnHelos", "PEnProps")  ###For PEn:
vars.mit = c("ImpCP_VorMore", "AdultsOnly", "AirTour", "Talk", "ViewWildlife")    ###For OnlyPrior: "SiteType"
vars.interact =  NULL #c("I(PEnHelos * PEnProps)")  ###For PEn: "I(PEnHelos * PEnProps)"
PEnRegress = TRUE
DesiredPlotX = vars.dos[1]
source("ATMP_2011Overnight_LogRegPEn_Interfere_CoeffOnly.R")

##REJECT ViewWildlife: AIC increases by ~2 for all. 

# <<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>>
# Test lg10.DurVisitMinutes

# Model 49: interfere only ----
rm(list = ls())          ### Clears all variables
source("Model_Setup_Overnight.R")
dose.name= "LAE (dBA)"
dose.var = "SELAllAC"
AddDose = "PTAudAllAC" #Use "none" if additional doses are not included in the model.
vars.dos = c("SELAllAC", "PTAudAllAC", "PEnHelos", "PEnProps")  ###For PEn:
vars.mit = c("ImpNQ_VorMore", "AdultsOnly", "AirTour", "Talk", "lg10.DurVisitMinutes")    ###For OnlyPrior: "SiteType"
vars.interact =  NULL #c("I(PEnHelos * PEnProps)")  ###For PEn: "I(PEnHelos * PEnProps)"
PEnRegress = TRUE
DesiredPlotX = vars.dos[1]
source("ATMP_2011Overnight_LogRegPEn_Interfere_CoeffOnly.R")

# Model 49b: interfere only ----
rm(list = ls())          ### Clears all variables
source("Model_Setup_Overnight.R")
dose.name= "LAE (dBA)"
dose.var = "SELAllAC"
AddDose = "PTAudAllAC" #Use "none" if additional doses are not included in the model.
vars.dos = c("SELAllAC", "PTAudAllAC", "PEnHelos", "PEnProps")  ###For PEn:
vars.mit = c("ImpCP_VorMore", "AdultsOnly", "AirTour", "Talk", "lg10.DurVisitMinutes")    ###For OnlyPrior: "SiteType"
vars.interact =  NULL #c("I(PEnHelos * PEnProps)")  ###For PEn: "I(PEnHelos * PEnProps)"
PEnRegress = TRUE
DesiredPlotX = vars.dos[1]
source("ATMP_2011Overnight_LogRegPEn_Interfere_CoeffOnly.R")

##REJECT lg10.DurVisitMinutes: AIC increases for all

# <<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>>
#Interfere final model: Include SEL, PTAud, PEnHelos, PEnProps, ImpNQ_VorMore OR ImpCP_VorMore, AdultsOnly, AirTour, Talk 
# <<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>>


# Run model selection script rather than manual evaluation of AIC as above:
source("Model_Selection_Overnight.R")

# Variables in the dataset
# [1] "X"                      "Site"                   "Survey"                
# [4] "SiteType"               "HikeBeginMinAfterMidnt" "SiteFirstVisit"        
# [7] "ImpNatQuiet"            "ImpViewScenery"         "ImpCalmPeace"          
# [10] "ImpHistCult"            "ImpAdventure"           "ViewSunRiseSet"        
# [13] "PicnicMeal"             "WatchBirds"             "ViewWildlife"          
# [16] "RangerTalk"             "OtherTalkDemonst"       "AirTour"          
# [19] "country"                "CommEducGroup"          "NumChild"              
# [22] "LmaxAllAC"              "AudDurAllACMinutes"     "DurVisitMinutes"       
# [25] "SELAllAC"               "HierSELHelos"           "AudDurHelosMinutes"    
# [28] "HierSELProps"           "HierAudDurPropsMinutes" "HierSELJets"           
# [31] "HierAudDurJetsMinutes"  "L50NatQuiet"            "DprimeLSELAllAC"       
# [34] "DprimeLLeqTAC"          "DprimeLeqTresp"         "DurAbvDprime7Minutes"  
# [37] "DprimeL90"              "DprimeL50"              "DprimeL10"             
# [40] "DurAbvDprime17Minutes"  "X.TAboveDprimeL17"      "InterfereNatQuiet"     
# [43] "AircraftAnnoy"          "HearAircraft"           "Talk"                  
# [46] "LeqTresp"               "LeqTAC"                 "PTAudAllAC"            
# [52] "LeqProps"               "PTAudProps"             "LeqJets"               
# [55] "PTAudJets"              "PEnHelos"               "PEnProps"              
# [58] "lg10.DurVisitMinutes"   "ImpNQ_VorMore"          "ImpCP_VorMore"         
# [61] "AdultsOnly"             "SiteVisitBefore"        "IntWithNQ_SorMore"     
# [64] "IntWithNQ_MorMore"      "IntWithNQ_VorMore"      "Annoy_SorMore"         
# [67] "Annoy_MorMore"          "Annoy_VorMore"          "Dataset"               
# [70] "SeqAll"                