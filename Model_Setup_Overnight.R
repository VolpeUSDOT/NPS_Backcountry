#ModelSetup

##  DO NOT CHANGE THESE
rm(list = ls())      		### Clears all variables
library("arm")					### Required library
library("lme4")					### Required library
library("stats")

###########################################################################
#Set the variables used in the Plot3ModsScript for the simulations
DesiredRegrType = "Dichot"        	#Dichot or Ordinal
OrdMultiYesNo = "No"
num.curves =  150 #number of curves to show on the plots
num.sims   = 1000 #number of simulations to run

HeaderEq = T # Include equation in header if T
EquationCurve = T
GrayCurves = F # show grey curves on plot if T
ConfLimits = T # Show confidence limits on plot if T
RandomParts = F

STypeList=c("Dayhike")
