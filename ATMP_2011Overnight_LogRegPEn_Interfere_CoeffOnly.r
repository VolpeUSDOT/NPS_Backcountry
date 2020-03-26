### Script to test alternative dose variables (published model uses LmaxAllAC) using the published model fit to new data (DRMerged2011subset.csv)
### Erika A Sudderth March 14, 2013. Modified from scripts by Grant Anderson ()

### The code below produces the model fits to the new (2011) survey data that using the published models that were determined to have the best fit to the older survey data (Noise Control Eng. J. 59(5):519-540). Additional mediator and dose variables were tested following the published model selection procedures.
				
#################

  Data <- read.csv("Data/ATMP2011_CompleteDoseVars_dprime.csv") 
  names(Data) 
  dim(Data)
  DataType = "AllCorrectedOnlyPrior"

#Pre-process datafile
#SiteType: remove ShortHike data from 2011 dataset 
dim(Data)
Data <- subset(Data, Data$SiteType != "ShortHike") # Removes ~251 rows
Data$SiteType <- factor(Data$SiteType) 
Data <- subset(Data, Data$Survey != "AC") #Removes ~878 rows
Data$Survey <- factor(Data$Survey) 
dim(Data)
table(Data$SiteType)
table(Data$Survey)

#######################
##  DURATION RESTRICTIONS FPR BCOvernights
##  Fill in this value (minimum minutes for DurVisit)
minDurVisit = 60							## Only for BCOvernights
maxDurVisit = 9000 						## Use 9000 for Overnight hikes

##  ADDITIONAL DESIRED DATA (must have SeqAll in it)
AddData = c("SeqAll", "DurVisitMinutes", "Survey")		# Plus others, if desired during simulation

##  CORRECTED VS NOT
		DSet = c("Dataset")			#  FOR CORRECTED, ENTER "Dataset"; otherwise leave empty.

##  FILTERING: HR1 and HR2, plus Site
		DSet.filterOff = ""				#  FOR ALL DATA, CHANGE THIS TO ""; otherwise "HR1" or "HR2"
		SiteFilterYesNo = "No"
		SiteFilterOn = c()
		

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
				colnames_results = c("Response", "Int",
				                     levels(Data$Site),
				                     levels(Data$SiteType),
				                     vars.dos, 
				                     vars.mit, 
				                     "AIC", "BIC", "logLike", "Deviance", "n.obs", "Sigma")
				rownames_results = c("IS", "IM", "IV")
				
				results = matrix(data = NA,
				                 nrow = length(rownames_results),
				                 ncol = length(colnames_results))
				
				dimnames(results) = list(rownames_results,
				                         colnames_results)
				
				results = as.data.frame(results)
				

##EAS: 6 different responses fit - 
	#results[1,1] = "Annoy_SorMore"
	#results[2,1] = "Annoy_MorMore"
	#results[3,1] = "Annoy_VorMore"
	results[1,1] = "IntWithNQ_SorMore"
	results[2,1] = "IntWithNQ_MorMore"
	results[3,1] = "IntWithNQ_VorMore"
	#results

###### RESPONSE: LOOP OVER 6 RESPONSES
 r = 2 					# Uncomment, to test
#detach(vars.all.data)

	for (r in 1 : 3) {
		res = results[r,1]
	
		###### Assemble variables and data for both regressions (this r), using na.omit:
		varnames.na = c(res, "Site", "SiteType", vars.dos.nolog, vars.mit.nolog, DSet, AddData)
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
					eq.ref = paste(res, " ~ Site + SiteType + 1", sep="")
					if (n.vars.ref > 1) {
						for (n in 2:n.vars.ref) {
							eq.ref = paste(eq.ref, " + ", varnames.ref[n], sep="")
						}
					}
					
      ## Regression
				fit.ref = glm(noquote(eq.ref),
				              family = binomial(link="logit"),
				              data = vars.all.data)
				
				#print(fit.ref)		
				#fit.ref
				
				betas = coef(fit.ref)
				
				# Need column for each predictor. Site and SiteType are now in the fixed effects, so need to add to the coefficient results.
				coeff.cols <- length(betas)
				
				coeffs_for_res <- round(betas,5)
				names(coeffs_for_res)[names(coeffs_for_res) == "(Intercept)"] = "Int"
				names(coeffs_for_res) <- sub("^Site", "", names(coeffs_for_res))
				names(coeffs_for_res) <- sub("Yes$", "", names(coeffs_for_res))
				names(coeffs_for_res) <- sub("^Type", "", names(coeffs_for_res))
				
				## Collect required baseline parameters
				
				coefs_and_params_for_res = c(coeffs_for_res,
				                             AIC = AIC(fit.ref),
				                             BIC = BIC(fit.ref),
				                             logLike = logLik(fit.ref),
				                             Deviance = deviance(fit.ref),
				                             n.obs = length(fit.ref$fitted.values),
				                             Sigma = sd(resid(fit.ref))
				)
				
				
				results[r, 
				        na.omit(match(names(coefs_and_params_for_res), names(results)))] =
				  
				  coefs_and_params_for_res[na.omit(match(names(results), names(coefs_and_params_for_res)))]  
				
		
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
write.csv(results,file = file.path("Output",
                                  paste0("Fixed_ATMP_2011Overnight_",
                                         paste(vars.dos, collapse = ""),
                                         paste(vars.mit, collapse = ""), "_Interfere_CoeffAIC.csv")
                                  )
          )   #SiteTypeOnly


fit.table <- rbind(results$Response[1], coef(summary(fit.1)),
                   results$Response[2], coef(summary(fit.2)),
                   results$Response[3], coef(summary(fit.3)))

fit.table

write.csv(fit.table, file = file.path("Output",
                                     paste0("Fixed_ATMP_2011Overnight_",
                                            paste(vars.dos, collapse = ""),
                                            paste(vars.mit, collapse = ""), 
                                  "_Interfere_CoeffProbs.csv")
                                  )
          )


