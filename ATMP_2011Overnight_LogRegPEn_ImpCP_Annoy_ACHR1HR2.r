### Script to test alternative dose variables (published model uses LmaxAllAC) using the published model fit to new data (DRMerged2011subset.csv)
### Erika A Sudderth March 14, 2013. Modified from scripts by Grant Anderson ()

### The code below produces the model fits to the new (2011) survey data that using the published models that were determined to have the best fit to the older survey data (Noise Control Eng. J. 59(5):519-540). Additional mediator and dose variables were tested following the published model selection procedures.
				
#################


  Data <- read.csv("Data/ATMP2011_CompleteDoseVars_dprime.csv")
  names(Data) 
  dim(Data)
  DataType = "AllCorrectedOnlyPrior"

#####################################################
# Axis labels for figures for each dose variable
# SELAllAC = LAE (dBA)
# LeqTresp = LAeq,Tresp (dBA)
# LeqTAC = L Aeq,Tac (dBA)
# LmaxAllAC = LASmx (dBA)
# PTAudAllAC = TAud (%)

#Pre-process datafile
#SiteType: remove ShortHike data from 2011 dataset 

  Data <- subset(Data, Data$SiteType != "ShortHike") #Removes ~251 rows
  
  Data$SiteType <- factor(as.character(Data$SiteType))
  levels(Data$SiteType) = c('2_BCOvernight', '1_DayHike')
  Data$SiteType <- factor(as.character(Data$SiteType))
  

#######################
##  DURATION RESTRICTIONS FPR DayHikes
##  Fill in this value (minimum minutes for DurVisit)
minDurVisit = 60							## Only for DayHikes
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

##  SET UP RESULTS DATAFRAME FOR EVERYTHING EXCEPT fit
num.col <- length(c("Response", "Int", vars.dos, "SurveyHR1","SurveyHR2", vars.mit[2:length(vars.mit)], "AIC", "BIC", "logLike", "Deviance", "n.obs"))
results.mat = rep(NA,3*num.col)
dim(results.mat) = c(3, num.col)
results = as.data.frame(results.mat)
colnames(results) = c("Response", "Int", vars.dos, "SurveyHR1","SurveyHR2", vars.mit[2:length(vars.mit)], "AIC", "BIC", "logLike", "Deviance", "n.obs")
rownames(results) = c("AS", "AM", "AV") #, "IS", "IM", "IV")
rm(results.mat)
results

##EAS: 6 different responses fit - 
	results[1,1] = "Annoy_SorMore"
	results[2,1] = "Annoy_MorMore"
	results[3,1] = "Annoy_VorMore"
	#results[4,1] = "IntWithNQ_SorMore"
	#results[5,1] = "IntWithNQ_MorMore"
	#results[6,1] = "IntWithNQ_VorMore"
	results

###### RESPONSE: LOOP OVER 6 RESPONSES
# r = 1 					# Uncomment, to test
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
		
		# Scale numeric variables
		vars.all.data2 <- vars.all.data
		is_numeric = sapply(vars.all.data2, class) == 'numeric'
		vars.all.data2[is_numeric] <- as.numeric(scale(vars.all.data2[is_numeric]))
		
		varnames.ref = c(res, vars.dos, vars.mit)	# Reference case
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
					fit.ref = glmer(noquote(eq.ref), 
					                family = binomial(link="logit"),
					                verbose = FALSE,
					                data = vars.all.data2,
					                glmerControl(optimizer = "optimx", optCtrl = list(method = 'nlminb'))
					)
					
		    betas = fixef(fit.ref)
		    coeff.cols <- length(c("Response", "Int", vars.dos, "SurveyHR1","SurveyHR2", vars.mit[2:length(vars.mit)]))
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
}

#check results - print to console
print(results)

#Save model results (coefficient estimates) to file
write.csv(results,file=paste0("Output/ATMP_2011Overnight_",
                             paste(vars.dos,collapse=""),
                             paste(vars.mit,collapse=""),"_Annoy_CoeffAIC.csv"))   #SiteTypeOnly


fit.table <- rbind(results$Response[1],coef(summary(fit.1)),results$Response[2],coef(summary(fit.2)),results$Response[3],coef(summary(fit.3)))
fit.table
write.csv(fit.table,file=paste0("Output/ATMP_2011Overnight_",
                                paste(vars.dos,collapse=""),
                                paste(vars.mit,collapse=""),
                                "_Annoy_CoeffProbs.csv"))


################################################
#Set mediator variable values used for the results plots

#Mediator variable tables for each hike type
ImpCPCounts <- table(vars.all.data$ImpCP_VorMore)
ImpCPCounts
ImpCPCounts[2]/sum(ImpCPCounts) #86%

VisitBefore <- table(vars.all.data$SiteVisitBefore)
VisitBefore
VisitBefore[2]/sum(VisitBefore) #12.9%

AdultsOnlyCounts <- table(vars.all.data$AdultsOnly)
AdultsOnlyCounts
AdultsOnlyCounts[2]/sum(AdultsOnlyCounts) #80.6%

Perct.ImpCP = round(ImpCPCounts[2]/sum(ImpCPCounts),2)*100  			
Perct.Before = round(VisitBefore[2]/sum(VisitBefore),2)*100				
Perct.Adults = round(AdultsOnlyCounts[2]/sum(AdultsOnlyCounts),2)*100		

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


#Logistic regression function: PEnProps
PEnProps.Binary <- vars.all.data$PEnProps
PEnProps.Binary[PEnProps.Binary<50] <- 0
PEnProps.Binary[PEnProps.Binary>50] <- 1
P.log = glm(PEnProps.Binary~DoseVar, family=binomial(link="logit"))
summary(P.log)
#str(P.log)
P.log$coefficients[1]
}


 xmin = min(vars.all.data[DesiredPlotX]) #x-axis minimum
 xmax = max(vars.all.data[DesiredPlotX]) #x-axis max
 xlimits = c(xmin-2, xmax+2)					#Set x-axis limits

#####################################
# new for 2016! Plot in here. 
if(!exists("PLOT")) PLOT = FALSE
 
if(PLOT){
  

  varnames.plot = c(DesiredPlotY, vars.dos, "SurveyHR1","SurveyHR2", vars.mit[2:length(vars.mit)])
  
  if (DesiredPlotY == "Annoy_SorMore") {
    fit.plot = fit.1
    #vars.all.data = vars.all.data.1
    eq.plot = results[1,"eq"]
    add.plot=FALSE
    col.line="darkolivegreen"
  }
  if (DesiredPlotY == "Annoy_MorMore") {
    fit.plot = fit.2
    #vars.all.data = vars.all.data.2
    eq.plot = results[2,"eq"]
    add.plot=FALSE
    col.line="darkolivegreen3"
  }	
  if (DesiredPlotY == "Annoy_VorMore") {
    fit.plot = fit.3
    #vars.all.data = vars.all.data.3
    eq.plot = results[3,"eq"]
    add.plot=TRUE
    col.line="honeydew3"
  }
  if (DesiredPlotY == "IntWithNQ_SorMore") {
    fit.plot = fit.4
    #vars.all.data = vars.all.data.4
    eq.plot = results[4,"eq"]
    add.plot=FALSE
    col.line="darkolivegreen"
  }
  if (DesiredPlotY == "IntWithNQ_MorMore") {
    fit.plot = fit.5
    #vars.all.data = vars.all.data.5
    eq.plot = results[5,"eq"]
    add.plot=TRUE
    col.line="darkolivegreen3"
  }
  if (DesiredPlotY == "IntWithNQ_VorMore") {
    fit.plot = fit.6
    #vars.all.data = vars.all.data.6
    eq.plot = results[6,"eq"]
    add.plot=TRUE
    col.line="honeydew3"
  }
  
  
  
  
  
  }
 
 
####################################
#Remove variables
remove(Data)


