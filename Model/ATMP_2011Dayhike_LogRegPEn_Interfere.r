### Script to test alternative dose variables (published model uses LmaxAllAC) using the published model fit to new data (DRMerged2011subset.csv)
### Erika A Sudderth March 14, 2013. Modified from scripts by Grant Anderson ()

### The code below produces the model fits to the new (2011) survey data that using the published models that were determined to have the best fit to the older survey data (Noise Control Eng. J. 59(5):519-540). Additional mediator and dose variables were tested following the published model selection procedures.
				
###########################################################################
#### MAKE THESE MODIFICATIONS/CHOICES EACH TIME SCRIPT IS RUN
###########################################################################

# ##	DO NOT CHANGE THESE
# 			rm(list = ls())					### Clears all variables
#       library("arm")					### Required library
#    		library("lme4")					### Required library
#       library("stats")

#################
##  EAS: Load the csv file generated in the "ATMP_2011DataProcess.r" script
#Data = DR2011
  setwd("X:/Overnight") #change to directory location of files
  Data <- read.csv("ATMP2011_CompleteDoseVars_dprime.csv") 
  names(Data) 
  dim(Data)
  DataType = "AllCorrectedOnlyPrior"

####################################################
##List of alternative Dose models fit for report:
# 1.  LeqTresp + Pens + Mediators (reference model) 
# 2.  SEL + Pens + Mediators 
# 3.	LeqTAC + Pens + Mediators 
# 4.	PTAUDAllAC + Mediators 
# 5.	LmaxAllAC + Mediators 
# 6.	LeqTresp+PTAudAllAC + PENs + Mediators 
# 7.	SELAllAC + PTAudAllAC + Pens + Mediators 
# 8.	LeqTresp + L50NatQuiet + Pens + Mediators 
# 9.	LeqTac + L50NatQuiet + Pens + Mediators 
# 10.	LmaxAllAC +L50NatQuiet + Mediators 

#####################################################
# Axis labels for figures for each dose variable
# SELAllAC = LAE (dBA)
# LeqTresp = LAeq,Tresp (dBA)
# LeqTAC = L Aeq,Tac (dBA)
# LmaxAllAC = LASmx (dBA)
# PTAudAllAC = TAud (%)

#####################################################
##  DESIGNATE ALL THE REGRESSION AND FILTER VARIABLES
##  IF DATA is "Corrected," then need LmaxAllAC instead of LeqAll.
##	Remaining variables (format for logged variables: log10(... + 0.001):

#         dose.name= "LAeq,Tresp (dBA)"
#         dose.var = "LeqTresp"
#         AddDose = "none" #Use "none" if additional doses are not included in the model.
#         vars.dos = c("LeqTresp", "PEnHelos", "PEnProps")  ###For PEn:
# 				vars.mit = c("Survey", "ImpCP_VorMore", "SiteVisitBefore",  "AdultsOnly")		###For OnlyPrior: "SiteType"
# 				vars.interact =	c("I(PEnHelos * PEnProps)")	###For PEn: "I(PEnHelos * PEnProps)"
#         DesiredPlotX = vars.dos[1]

#################################################################
##Code to check that the requested variables are in the datafile for debugging
#   vars.dos%in%names(Data) #TRUE
#   vars.mit%in%names(Data) #TRUE
#   intNames <- c("PTAudHelos", "PTAudProps")
#   intNames%in%names(Data) #TRUE
#   c("Annoy_SorMore", "LmaxAllAC")%in%names(Data) #TRUE
################################################################

#Pre-process datafile
#SiteType: remove ShortHike data from 2011 dataset 
Data <- subset(Data,Data$SiteType == "DayHike") #Removes ~300 rows
Data$SiteType <- factor(Data$SiteType) 
Data <- subset(Data,Data$Survey != "AC") #Removes ~878 rows
Data$Survey <- factor(Data$Survey) 

#######################
##  DURATION RESTRICTIONS FPR DayHikes
##  Fill in this value (minimum minutes for DurVisit)
minDurVisit = 60							## Only for DayHikes
maxDurVisit = 900 						## Use 9000 for Overnight hikes

##  ADDITIONAL DESIRED DATA (must have SeqAll in it)
AddData = c("SeqAll", "DurVisitMinutes", "Survey")		# Plus others, if desired during simulation

##  CORRECTED VS NOT
		DSet = c("Dataset")			#  FOR CORRECTED, ENTER "Dataset"; otherwise leave empty.

##  FILTERING: HR1 and HR2, plus Site
		DSet.filterOff = ""				#  FOR ALL DATA, CHANGE THIS TO ""; otherwise "HR1" or "HR2"
		SiteFilterYesNo = "No"
		SiteFilterOn = c()
		
###########################################################################	
####	VARIABLE DEFINITIONS	(written by Grant Anderson)
###########################################################################	
##
##	Input string variables
##		Outfile: The CSV file for output	
##		SeqNo: Numbering for CSV output file
##	
##		vars.dos: All the non-test, non-interaction dose variables
##		vars.mit: Same, mitigation variables
##		vars.interact: All the non-test, interaction variables (dose, or mitigation, or both)
##	
#####	Dichotomized regressions
##	Variables slightly modified
##		vars.dos.logpre: Same, for non-test doses
##		vars.mit.logpre: Same, for non-test mitigators
##		vars.dos.nolog: Same, for non-test doses
##		vars.mit.nolog: Same, for non-test mitigators
##  			logpre variables are shorthand for the header equation of simulation plots 
##  			nolog variables are for ...
##	
##	Resulting equation variables
##	results [6,5]: 6 responses, response name and three equation components and full equation]
##		
##	Response-loop variables
##		r: Loop index over the 6 responses
##		res: The response for this loop (pulled from response[,]
##		varnames.na: Names of all variables in this loop's two regressions---to accomplish na.omit
##		vars.all.data: Data frame with those variables, with NA's omitted---for all regressions
##	
####	Reference part of loop
##					Names, equations
##						varnames.ref: Variables needed for just the reference equation
##						eq.ref: The reference equation for this loop, built from varnames.ref
##					Results
##						fit.ref: The reference fit from this loop, fit with lmer()
##						AIC.ref: The reference AIC for this loop
##						Dev.ref: The reference Deviance for this loop
##						SDSts.ref: The Standard Deviation of the Sites for this loop	
	
###########################################################################
#### COMPUTATIONS
###########################################################################

##	MODIFY VARIABLE NAMES AS NEEDED BELOW
##	For logged dose variable, get rid of "log"	
				temp						= gsub("10(", "", vars.dos, fixed = T)
				vars.dos.logpre	= gsub(" + 0.001)", "", temp, fixed = T)
				vars.dos.nolog 	= gsub("log", "", vars.dos.logpre, fixed = T)
##	Same for mitigators
				temp						= gsub("10(", "", vars.mit, fixed = T)
				vars.mit.logpre	= gsub(" + 0.001)", "", temp, fixed = T)
				vars.mit.nolog 	= gsub("log", "", vars.mit.logpre, fixed = T)

##	SET UP RESULTS DATAFRAME FOR EVERYTHING EXCEPT fit
	num.col <- length(c("Response", "Int", vars.dos, vars.interact, vars.mit, "AIC", "BIC", "logLike", "Deviance", "n.obs"))
  results.mat = rep(NA,3*num.col)
	dim(results.mat) = c(3, num.col)
	results = as.data.frame(results.mat)
	colnames(results) = c("Response", "Int", vars.dos, vars.interact, vars.mit, "AIC", "BIC", "logLike", "Deviance", "n.obs")
	rownames(results) = c("IS", "IM", "IV")
	rm(results.mat)
  results

##EAS: 6 different responses fit - 
	#results[1,1] = "Annoy_SorMore"
	#results[2,1] = "Annoy_MorMore"
	#results[3,1] = "Annoy_VorMore"
	results[1,1] = "IntWithNQ_SorMore"
	results[2,1] = "IntWithNQ_MorMore"
	results[3,1] = "IntWithNQ_VorMore"
	results

###### RESPONSE: LOOP OVER 6 RESPONSES
 r = 2 					# Uncomment, to test
#detach(vars.all.data)

	for (r in 1 : 3) {
		res = results[r,1]
	
		###### Assemble variables and data for both regressions (this r), using na.omit:
		varnames.na = c(res, "Site", vars.dos.nolog, vars.mit.nolog, DSet, AddData)
		vars.all.data = Data[varnames.na]													### Grab from proper data set
		vars.all.data = subset(vars.all.data, Dataset != DSet.filterOff)		### Subset re Dataset
		if (DataType == "AllCorrectedOnlyPrior") { ##Changed from AllCorrectedNoPrior to filter short visits
		  vars.all.data = subset(vars.all.data, vars.all.data$DurVisitMinutes > minDurVisit)
		  vars.all.data = subset(vars.all.data, vars.all.data$DurVisitMinutes < maxDurVisit) #Use for Overnight data
		}
		vars.all.data = na.omit(vars.all.data)
		if (SiteFilterYesNo == "Yes") {
		  vars.all.data = vars.all.data[vars.all.data$Site %in% SiteFilterOn, ]
		}
		vars.all.data = na.omit(vars.all.data)
		
		varnames.ref = c(res, vars.dos, vars.interact, vars.mit)	# Reference case
		varnames.ref		
		
###### REFERENCE REGRESSION
			## Equation
				n.vars.ref = length(varnames.ref)
					eq.ref = paste(res, " ~ (1|Site) + 1", sep="")
					if (n.vars.ref > 1) {
						for (n in 2:n.vars.ref) {
							eq.ref = paste(eq.ref, " + ", varnames.ref[n], sep="")
						}
					}
					
      ## Regression
				attach(vars.all.data)
				fit.ref = glmer(noquote(eq.ref), family=binomial(link="logit"), verbose=FALSE)
		    print(fit.ref)		
				fit.ref
		    betas = fixef(fit.ref)
		    coeff.cols <- length(c("Response", "Int", vars.dos, vars.interact, vars.mit))
		    results[r,2:coeff.cols] = round(betas,5)
		    results  
    
    ## Collect required baseline parameters
        fits <- round(summary(fit.ref)$AICtab,1)
				AIC.ref = fits[1]
		    BIC.ref = fits[2]
  		  logLik.ref = fits[3]
				Dev.ref = fits[4]
				SDSts.ref = sigma.hat(fit.ref)$sigma$Site[1]
        n.ref = dim(fit.ref@frame)[1]
    
    ## Add AIC, BIC, deviance to results table
		    results[r,coeff.cols+1] = AIC.ref
		    results[r,coeff.cols+2] = BIC.ref
		    results[r,coeff.cols+3] = logLik.ref
		    results[r,coeff.cols+4] = Dev.ref
		    results[r,coeff.cols+5] = n.ref
		
		## Save equation for simulation
			#results$eq[r]  = eq.ref
		
    ## Save fit for simulation
		if (r == 1) {fit.1 = fit.ref}
		if (r == 2) {fit.2 = fit.ref}
		if (r == 3) {fit.3 = fit.ref}
		#if (r == 4) {fit.4 = fit.ref}
		#if (r == 5) {fit.5 = fit.ref}
		#if (r == 6) {fit.6 = fit.ref}
    
    ## Save database for residual analysis
		if (r == 1) {vars.all.data.1 = vars.all.data}
		if (r == 2) {vars.all.data.2 = vars.all.data}
		if (r == 3) {vars.all.data.3 = vars.all.data}
		#if (r == 4) {vars.all.data.4 = vars.all.data}
		#if (r == 5) {vars.all.data.5 = vars.all.data}
		#if (r == 6) {vars.all.data.6 = vars.all.data}
		
      ## Detach datafile
		  detach(vars.all.data)
}

#check results - print to console
print(results)

#Save model results (coefficient estimates) to file
write.csv(results,file=paste("ATMP_2011Dayhike_",paste(vars.dos,collapse=""),paste(vars.mit,collapse=""),"_Interfere_CoeffAIC.csv",sep=""))   #SiteTypeOnly

## Save fits as .csv files
fit.summary <- list(results$eq[1],fit.1,results$eq[2],fit.2,results$eq[3],fit.3)
fit.summary
sink(file=paste("ATMP_2011Dayhike_",paste(vars.dos,collapse=""),paste(vars.mit,collapse=""),"_Interfere_FitsAll.csv",sep=""))
fit.summary
sink()

fit.table <- rbind(results$Response[1],coef(summary(fit.1)),results$Response[2],coef(summary(fit.2)),results$Response[3],coef(summary(fit.3)))
fit.table
write.csv(fit.table,file=paste("ATMP_2011Dayhike_",paste(vars.dos,collapse=""),paste(vars.mit,collapse=""),"_Interfere_CoeffProbs.csv",sep=""))

################################################
#Set mediator variable values used for the results plots

#Mediator variable tables for each hike type
ImpCPCounts <- table(vars.all.data$ImpCP_VorMore)
ImpCPCounts
ImpCPCounts[2]/sum(ImpCPCounts) #86%

VisitBefore <- table(vars.all.data$SiteVisitBefore)
VisitBefore
VisitBefore[2]/sum(VisitBefore) #12.9%

AdultsOnly <- table(vars.all.data$AdultsOnly)
AdultsOnly
AdultsOnly[2]/sum(AdultsOnly) #80.6%

Perct.ImpCP = round(ImpCPCounts[2]/sum(ImpCPCounts),2)*100  			
Perct.Before = round(VisitBefore[2]/sum(VisitBefore),2)*100				
Perct.Adults = round(AdultsOnly[2]/sum(AdultsOnly),2)*100		

###########################################################################
#Linear regressions of dose variables
#Note: These plots show the linear regressions that were used to relate the non-plotted dose variables to the plotted dose variables (LeqTresp) in Grant Anderson's code compared to the logisit fits used by EA Sudderth. The logistic fits prevent predicted values for the doses beyond the range of the data (e.g. no negative doses are predicted).

if (PEnRegress == "TRUE"){
#names(vars.all.data)
DoseVar <- vars.all.data[,which(names(vars.all.data)==dose.var)]

#Linear regression used in the plotting function (written by Grant)
m1 <- lm(vars.all.data$PEnHelos~DoseVar)
m1

#Median and Mean of PEnHelos
median(vars.all.data$PEnHelos, na.rm=TRUE)
mean(vars.all.data$PEnHelos, na.rm=TRUE)

#Linear regression used in the plotting function (written by Grant)
m3 <- lm(vars.all.data$PEnProps~DoseVar)
m3

#Mean and median of PEnProps
mean(vars.all.data$PEnProps)
median(vars.all.data$PEnProps)

##############################################################################
#Threshold data and apply standard logistic regression. 
#Try logistic regression function: PEnHelos
PEnHelos.Binary <- vars.all.data$PEnHelos
PEnHelos.Binary[PEnHelos.Binary<50] <- 0
PEnHelos.Binary[PEnHelos.Binary>50] <- 1
H.log = glm(PEnHelos.Binary~DoseVar, family=binomial(link="logit"))
summary(H.log)

# #xplot values
# xnumb = 50
# xmin = min(DoseVar)
# xmax = max(DoseVar)
# xplot = seq(xmin, xmax, (xmax - xmin)/(xnumb - 1))
# 
# #Plot curve
# jitter.binary <- function(a, jitt=.05){ a + (1-2*a)*runif(length(a),0,jitt) } 
# Res.jit = 100*jitter.binary(as.numeric(PEnHelos.Binary))
# plot(Res.jit ~ DoseVar, main="PEnHelos vs. Dose Variable", xlim=range(DoseVar), xlab=dose.var, ylab="PEnHelos")
# 
# P.log.curve <- paste("100*invlogit(",H.log$coefficients[1],"+",H.log$coefficients[2],"*xplot)", sep="")
# 
# yplot.H = rep(NA,50)
# yplot.H = eval(parse(text = P.log.curve))    
# lines(xplot, yplot.H, col="red",lwd=4)
# 
# #Add raw data points and original regression
# points(vars.all.data$PEnHelos~DoseVar, pch=16, col="light blue", cex=.5)
# abline(m1, col="blue", lty=2, lwd=2)

#Logistic regression function: PEnProps
PEnProps.Binary <- vars.all.data$PEnProps
PEnProps.Binary[PEnProps.Binary<50] <- 0
PEnProps.Binary[PEnProps.Binary>50] <- 1
P.log = glm(PEnProps.Binary~DoseVar, family=binomial(link="logit"))
summary(P.log)
#str(P.log)
P.log$coefficients[1]
}
# #xplot values
# xnumb = 50
# xmin = min(DoseVar)
# xmax = max(DoseVar)
# xplot = seq(xmin, xmax, (xmax - xmin)/(xnumb - 1))
# 
# #Plot data points
# jitter.binary <- function(a, jitt=.05){ a + (1-2*a)*runif(length(a),0,jitt) } 
# Res.jit = 100*jitter.binary(as.numeric(PEnProps.Binary))
# plot(Res.jit ~ DoseVar, main="PEnProps vs. Dose Variable", xlim=range(DoseVar), xlab=dose.var, ylab="PEnProps")
# 
# #Plot logistic curve
# P.log.curve <- paste("100*invlogit(",P.log$coefficients[1],"+",P.log$coefficients[2],"*xplot)", sep="")
# yplot.P = rep(NA,50)
# yplot.P = eval(parse(text = P.log.curve))  	
# lines(xplot, yplot.P, col="red",lwd=4)
# 
# #Add raw data points and original regression
# points(vars.all.data$PEnProps~DoseVar, pch=16, col="light blue", cex=.5)
# abline(m3, col="blue", lty=2, lwd=2)

# ###########################################################################
# #Set the variables used in the Plot3ModsScript for the simulations
# DesiredRegrType = "Dichot"      		#Dichot or Ordinal
# OrdMultiYesNo = "No"
# num.curves =150 #number of curves to show on the plots
# num.sims = 1000 #number of simulations to run
 xmin = min(vars.all.data[DesiredPlotX]) #x-axis minimum
 xmax = max(vars.all.data[DesiredPlotX]) #x-axis max
 xlimits = c(xmin-2, xmax+2)					#Set x-axis limits

# 
#   HeaderEq = T #Include equation in header if T
# 	EquationCurve = T
# 	GrayCurves = F #show grey curves on plot if T
# 	ConfLimits = T #Show confidence limits on plot if T
# 	RandomParts = F
# 
# STypeList=c("Dayhike")
# 
# ####################################
# #3 model plots: save to pdf file
# #The file name includes the dose variables included in the model above 
# pdf(file = paste("~/Documents/Documents/Admin/Volpe/Rwork/DprimeScripts/ATMP_2011DataPlot3Mods_Dayhike_LogRegPEn_",paste(vars.dos,collapse="_"),"Int_Survey.pdf",sep=""), width = 18, height = 8, onefile = TRUE, family = "Helvetica")
# 
# #List of desired dichotomizations to include in the figures
# #DesiredPlotYListAnnoy = c("Annoy_SorMore","Annoy_MorMore","Annoy_VorMore")
# DesiredPlotYListIntNQ = c("IntWithNQ_SorMore", "IntWithNQ_MorMore", "IntWithNQ_VorMore")
# 
# STypeList=c("DayHike")
# 
# #####################################
# ##Generate figures showing the dose-response curves for the 3 dichotomizations (somewhat, moderately, and very or more) for Annoy and Interfere responses
# 
# #Adjust the margins
# par(mfrow=c(1,1))
# par(mar=c(5.1, 5.1, 4.1, 2))
# 
# #Annoy response plot
# SType <- STypeList
# #   for (i in 1:length(DesiredPlotYListAnnoy)){
# #     DesiredPlotY <- DesiredPlotYListAnnoy[i]
# #     xlabel=dose.name
# #     ylabel="Percent Annoy"
# #     Title="Annoy:"
# #     source("~/Documents/Documents/Admin/Volpe/Rwork/DprimeScripts/ATMP_LogRegPEn_Plot3ModsScript.r")
# #   }
# 
# #Interfere response plot
#   SType <- STypeList
#   for (i in 1:length(DesiredPlotYListIntNQ)){
#     DesiredPlotY <- DesiredPlotYListIntNQ[i]
#     xlabel=dose.name
#     ylabel="Percent Interfere with Natural Quiet"
#     Title="Interfere With Natural Quiet:" 
#     source("~/Documents/Documents/Admin/Volpe/Rwork/DprimeScripts/ATMP_LogRegPEn_Plot3ModsScript_Int_Survey.r")
#   }
# 
# dev.off()

