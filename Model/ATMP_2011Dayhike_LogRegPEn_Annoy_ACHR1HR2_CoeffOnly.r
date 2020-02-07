### Script to fit the dose response model using the variables specified in the RunModelsScript_Manuscript_CoeffOnly.R script.
### Produces a table of the estimated coefficients for each predictor included in the model
### Erika A Sudderth March 14, 2013. Modified from scripts by Grant Anderson ()


#################
##  EAS: Load the csv file generated in the "ATMP_2011DataProcess.r" script
#Data = DR2011
#  setwd("F:/Volpe/Rwork/DprimeScripts") #change to directory location of files
  Data <- read.csv("ATMP2011_CompleteDoseVars_dprime.csv")
  names(Data) 
  dim(Data)
  DataType = "AllCorrectedOnlyPrior"


################################################################

#Pre-process datafile
#SiteType: remove ShortHike data from 2011 dataset 
# dim(Data)
Data <- subset(Data,Data$SiteType == "DayHike") #Removes ~300 rows
Data$SiteType <- factor(Data$SiteType) 
# dim(Data)
# table(Data$SiteType)
# table(Data$Survey)

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

##  SET UP RESULTS DATAFRAME FOR EVERYTHING EXCEPT fit
num.col <- length(c("Response", "Int", vars.dos, vars.interact, "SurveyHR1","SurveyHR2", vars.mit[2:length(vars.mit)], "AIC", "BIC", "logLike", "Deviance", "n.obs", "SigmaSite"))
results.mat = rep(NA,3*num.col)
dim(results.mat) = c(3, num.col)
results = as.data.frame(results.mat)
colnames(results) = c("Response", "Int", vars.dos, vars.interact, "SurveyHR1","SurveyHR2", vars.mit[2:length(vars.mit)], "AIC", "BIC", "logLike", "Deviance", "n.obs", "SigmaSite")
rownames(results) = c("AS", "AM", "AV") #, "IS", "IM", "IV")
rm(results.mat)
#results

##EAS: 3 different responses fit for Annoy - 
	results[1,1] = "Annoy_SorMore"
	results[2,1] = "Annoy_MorMore"
	results[3,1] = "Annoy_VorMore"
	#results[4,1] = "IntWithNQ_SorMore"
	#results[5,1] = "IntWithNQ_MorMore"
	#results[6,1] = "IntWithNQ_VorMore"
	#results

###### RESPONSE: LOOP OVER 3 RESPONSES
# r = 2 					# Uncomment, to test

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
				fit.ref = with(vars.all.data,glmer(noquote(eq.ref), family=binomial(link="logit"), verbose=FALSE))
		    #print(fit.ref)		
				#fit.ref
		    betas = fixef(fit.ref)
		    coeff.cols <- length(c("Response", "Int", vars.dos, vars.interact, "SurveyHR1","SurveyHR2", vars.mit[2:length(vars.mit)]))
		    results[r,2:coeff.cols] = round(betas,5)
		    #results  
    
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
		    results[r,coeff.cols+6] = SDSts.ref
    
		
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
		
}

#check results - print to console
print(results)

#Save model results (coefficient estimates) to file
write.csv(results,file=paste("ATMP_2011Dayhike_",paste(vars.dos,collapse=""),paste(vars.mit,collapse=""),"_Annoy_CoeffAIC.csv",sep=""))   #SiteTypeOnly

## Save fits as .csv files
#fit.summary <- list(results$eq[1],fit.1,results$eq[2],fit.2,results$eq[3],fit.3)
#fit.summary

# sink(file=paste("ATMP_2011Dayhike_",paste(vars.dos,collapse=""),paste(vars.mit,collapse=""),"_Annoy_FitsAll.csv",sep=""))
# fit.summary
# sink()

fit.table <- rbind(results$Response[1],coef(summary(fit.1)),results$Response[2],coef(summary(fit.2)),results$Response[3],coef(summary(fit.3)))
#fit.table
write.csv(fit.table,file=paste("ATMP_2011Dayhike_",paste(vars.dos,collapse=""),paste(vars.mit,collapse=""),"_Annoy_CoeffProbs.csv",sep=""))

