##Script to import and merge individual csv files from different surveys and sites
##Imports all files and extracts the columns used in the logistic regression models
##Applies transformations and dichotomizations to variables
##Author: EA Sudderth 
##Last modified 2016-10-19 Dan Flynn, to fit overnight data


#NOTE: Same as ATMP_2011DataProcess_Dp.r but adds "HierLmaxHelos", "HierLmaxProps", "HierLmaxJets" variables.

#####################################################################################
rm(list = ls())					### Clears all variables

library(lattice)
#Working directory with list of csv files exported from Excel

setwd("X:/Overnight") # Map server to local computer

# setwd("F:/Volpe/Rwork/DprimeScripts/2011_DR_csv_dprime") #change to location of csv files saved from Excel datasheets. Each excel sheet is saved as a csv file using Excel macros to batch process the worksheets.   


dir() #check list of files in the directory: there should be one file per site and survey (24 files for current 2011 survey data).

#Create list of files to be merged
FileNames <- list.files()
FileNames
length(FileNames)

###################################################################################
#Merge data for columns present in all surveys

#List of the variables (columns) to include in the merged dataset. We're not merging all the columns because not all columns are present in all the datasets.
MergeVars <- c("site","Survey","SiteType","HikeBeginMinAfterMidnt", "SiteFirstVisit","ImpNatQuiet", "ImpViewScenery", "ImpCalmPeace", "ImpHistCult", "ImpAdventure", "ViewSunRiseSet", "PicnicMeal", "WatchBirds", "ViewWildlife", "RangerTalk", "OtherTalkDemonst", "NeverAirTour", "country", "CommEducGroup","NumChild", "LmaxAllAC","AudDurAllACMinutes","DurVisitMinutes","SELAllAC", "HierSELHelos", "AudDurHelosMinutes", "HierSELProps", "HierAudDurPropsMinutes", "HierSELJets", "HierLmaxHelos", "HierLmaxProps", "HierLmaxJets", "HierAudDurJetsMinutes", "L50NatQuiet", "DprimeLSELAllAC", "DprimeLLeqTAC", "DprimeLeqTresp", "DurAbvDprime7Minutes", "DprimeL90", "DprimeL50", "DprimeL10", "DurAbvDprime17Minutes", "X.TAboveDprimeL17")

         
#Import the first file and subset the columns to merge using the MergeVars variable
DRMerged2011 <- read.csv(FileNames[1])
names(DRMerged2011)
DRMerged2011sub <- DRMerged2011[ ,names(DRMerged2011) %in% MergeVars]
dim(DRMerged2011sub)

#Add NA for the InterfereNatQuiet variable for the AC (audio clip) surveys: this survey did not include "InterfereNatQuiet"
Survey <- c(strsplit(FileNames, split="_"))
if (Survey[[1]][3] == "AC.csv") InterfereNatQuiet <- rep(NA,nrow(DRMerged2011sub)) else InterfereNatQuiet <- DRMerged2011$InterefereNatQuiet
length(InterfereNatQuiet)

#Set the AircraftAnnoy variable for each survey type: different column headings were used.
if (Survey[[1]][3] == "AC.csv") AircraftAnnoy <- DRMerged2011$AicraftSoundAnnoy else if (Survey[[1]][3] == "HR1.csv") AircraftAnnoy <- DRMerged2011$AicraftSoundAnnoy else if (Survey[[1]][3] == "HR2.csv") AircraftAnnoy <- DRMerged2011$AnnoyAircraft
length(AircraftAnnoy)

#Add the HearAircraft variable for the HR2 surveys: this survey used "Aircraft"
if (Survey[[1]][3] == "AC.csv") HearAircraft <- DRMerged2011$HearAircraft else if (Survey[[1]][3] == "HR1.csv") HearAircraft <- DRMerged2011$HearAircraft else if (Survey[[1]][3] == "HR2.csv") HearAircraft <- DRMerged2011$Aircraft
length(HearAircraft)

#Merge the variables created with NA for AC surveys with the first datafile
DRMerged2011sub <- data.frame(DRMerged2011sub,InterfereNatQuiet,AircraftAnnoy,HearAircraft)
dim(DRMerged2011sub)
names(DRMerged2011sub)

#Merge the rest of the csv files with the first csv file imported above.
#HearAircraft <- NULL
for (i in c(2:21)){
  TempName <- FileNames[i]
  print(paste(i,TempName))
  temp <- read.csv(TempName)
  names(temp)[which(names(temp)=="ACIntrfrNatQuiet")] <- "InterefereNatQuiet"
  names(temp)[which(names(temp)=="ImpViewScenary")] <- "ImpViewScenery"
  print(dim(temp))
  
  #subset the columns of temp to merge
  tempsub <- temp[ ,names(temp) %in% MergeVars]
  
#Add the variables only found in some surveys, replace with NA if that variable is not found in a given survey
  Survey <- c(strsplit(FileNames, split="_"))
  if (Survey[[i]][3] == "AC.csv") InterfereNatQuiet <- rep(NA,nrow(tempsub)) else InterfereNatQuiet <- temp$InterefereNatQuiet
  #print(length(InterfereNatQuiet))
  
  if (Survey[[i]][3] == "AC.csv") AircraftAnnoy <- temp$AicraftSoundAnnoy else if (Survey[[i]][3] == "HR1.csv") AircraftAnnoy <- temp$AicraftSoundAnnoy else if (Survey[[i]][3] == "HR2.csv") AircraftAnnoy <- temp$AnnoyAircraft

  if (Survey[[i]][3] == "AC.csv") HearAircraft <- temp$HearAircraft else if (Survey[[i]][3] == "HR1.csv") HearAircraft <- temp$HearAircraft else if (Survey[[i]][3] == "HR2.csv") HearAircraft <- ifelse(is.na(temp$Aircraft), "No", "Yes")

  tempsub <- data.frame(tempsub,InterfereNatQuiet,AircraftAnnoy,HearAircraft)
  
  remove("InterfereNatQuiet","AircraftAnnoy","HearAircraft")
  
  #check the subset dimensions
  print(dim(tempsub))
  
  #Merge the tempsub file with the DRMerged2011sub file 
  DRMerged2011sub <- merge(DRMerged2011sub,tempsub,by=c(MergeVars,"InterfereNatQuiet","AircraftAnnoy","HearAircraft"),all=TRUE)
  
  #Print dimensions of the merged file
  print(dim(DRMerged2011sub))
}

remove("Survey")
print(names(DRMerged2011sub))
table(DRMerged2011sub$Survey,DRMerged2011sub$HearAircraft)
table(DRMerged2011sub$site)
levels(DRMerged2011sub$site)

DRMerged2011sub <- subset(DRMerged2011sub, DRMerged2011sub$site!="")
DRMerged2011sub$site <- factor(DRMerged2011sub$site)
levels(DRMerged2011sub$site)

###################################################################################
#Check file dimensions and names: use to trouble shoot if there is an error while merging files. Commented out if not needed.
# rows <- NULL
# for (i in 1:length(FileNames)){
#   TempName <-FileNames[i]
#   print(TempName)
#   temp <- read.csv(TempName)
#   print(dim(temp))
#   print(MergeVars %in% names(temp))
#   print(MergeVars[MergeVars %in% names(temp)=="FALSE"])
#   rows <- sum(rows,dim(temp)[1])
# }
# rows
######################################################################################

#Check the dimensions of the merged file: should equal the length of MergeVars+1 
dim(DRMerged2011sub)

#Check the names and structure of the merged file
names(DRMerged2011sub)
str(DRMerged2011sub)

#Edit the site factor to remove duplicate spellings
levels(DRMerged2011sub$site)
DRMerged2011sub$site[DRMerged2011sub$site=="Tylrcrk"] <- "TylrCrk"
DRMerged2011sub$site[DRMerged2011sub$site==""] <- NA
DRMerged2011sub$site <- factor(DRMerged2011sub$site)
levels(DRMerged2011sub$site)
table(DRMerged2011sub$site)

#Convert variables to a character then back to numeric to get blank values replaced with NA or 0
DRMerged2011sub$NumChild <- as.character(DRMerged2011sub$NumChild)
DRMerged2011sub$NumChild[which(is.na(DRMerged2011sub$NumChild)=="TRUE")] <- "0"
DRMerged2011sub$NumChild <- as.numeric(DRMerged2011sub$NumChild)
table(DRMerged2011sub$NumChild)

DRMerged2011sub$ViewSunRiseSet <- as.character(DRMerged2011sub$ViewSunRiseSet)
DRMerged2011sub$ViewSunRiseSet[which(is.na(DRMerged2011sub$ViewSunRiseSet)=="TRUE")] <- "No"
DRMerged2011sub$ViewSunRiseSet[which(DRMerged2011sub$ViewSunRiseSet=="1")] <- "Yes"
table(DRMerged2011sub$ViewSunRiseSet)

DRMerged2011sub$PicnicMeal <- as.character(DRMerged2011sub$PicnicMeal)
DRMerged2011sub$PicnicMeal[which(is.na(DRMerged2011sub$PicnicMeal)=="TRUE")] <- "No"
DRMerged2011sub$PicnicMeal[which(DRMerged2011sub$PicnicMeal=="1")] <- "Yes"
table(DRMerged2011sub$PicnicMeal)

DRMerged2011sub$WatchBirds <- as.character(DRMerged2011sub$WatchBirds)
DRMerged2011sub$WatchBirds[which(is.na(DRMerged2011sub$WatchBirds)=="TRUE")] <- "No"
DRMerged2011sub$WatchBirds[which(DRMerged2011sub$WatchBirds=="1")] <- "Yes"
table(DRMerged2011sub$WatchBirds)

DRMerged2011sub$ViewWildlife <- as.character(DRMerged2011sub$ViewWildlife)
DRMerged2011sub$ViewWildlife[which(is.na(DRMerged2011sub$ViewWildlife)=="TRUE")] <- "No"
DRMerged2011sub$ViewWildlife[which(DRMerged2011sub$ViewWildlife=="1")] <- "Yes"
table(DRMerged2011sub$ViewWildlife)

DRMerged2011sub$RangerTalk <- as.character(DRMerged2011sub$RangerTalk)
DRMerged2011sub$RangerTalk[which(is.na(DRMerged2011sub$RangerTalk)=="TRUE")] <- "No"
DRMerged2011sub$RangerTalk[which(DRMerged2011sub$RangerTalk=="1")] <- "Yes"
DRMerged2011sub$Talk <- DRMerged2011sub$RangerTalk
DRMerged2011sub$Talk[which(DRMerged2011sub$OtherTalkDemonst==1)] <- "Yes"
table(DRMerged2011sub$Talk)

table(DRMerged2011sub$NeverAirTour)
DRMerged2011sub$AirTour <- as.character(DRMerged2011sub$NeverAirTour)
DRMerged2011sub$AirTour[which(DRMerged2011sub$NeverAirTour=="1")] <- "No"
DRMerged2011sub$AirTour[which(is.na(DRMerged2011sub$NeverAirTour)=="TRUE")] <- "Yes"
DRMerged2011sub$AirTour[which(DRMerged2011sub$NeverAirTour=="0")] <- "Yes"
table(DRMerged2011sub$AirTour)

table(DRMerged2011sub$country)
levels(DRMerged2011sub$country)
DRMerged2011sub$country <- as.character(DRMerged2011sub$country)
DRMerged2011sub$country[DRMerged2011sub$country=="USA"] <- "Yes"
DRMerged2011sub$country[DRMerged2011sub$country=="Usa"] <- "Yes"
DRMerged2011sub$country[DRMerged2011sub$country=="Other"|DRMerged2011sub$country==" Other"|DRMerged2011sub$country==""|DRMerged2011sub$country==" "] <- "No"
table(DRMerged2011sub$country)

levels(DRMerged2011sub$CommEducGroup)
DRMerged2011sub$CommEducGroup[DRMerged2011sub$CommEducGroup==""] <- "No"
DRMerged2011sub$CommEducGroup[DRMerged2011sub$CommEducGroup==" "] <- "No"
DRMerged2011sub$CommEducGroup <-factor(DRMerged2011sub$CommEducGroup)
table(DRMerged2011sub$CommEducGroup)

#Convert characters to factors
DRMerged2011sub$InterfereNatQuiet <- as.factor(DRMerged2011sub$InterfereNatQuiet)
DRMerged2011sub$HearAircraft <- as.factor(DRMerged2011sub$HearAircraft)
DRMerged2011sub$LmaxAllAC <- as.numeric(DRMerged2011sub$LmaxAllAC)
DRMerged2011sub$L50NatQuiet <- as.numeric(DRMerged2011sub$L50NatQuiet)

#Convert DurVisitMinutes to numeric and replace negative value with NA
DRMerged2011sub$DurVisitMinutes <- as.numeric(DRMerged2011sub$DurVisitMinutes)
DRMerged2011sub$DurVisitMinutes[DRMerged2011sub$DurVisitMinutes<=0] <- NA

str(DRMerged2011sub)
DRMerged2011sub$SiteType
detach(DRMerged2011sub)
###################################################################################
##Fix levels when duplicated with different spellings

attach(DRMerged2011sub)
dim(DRMerged2011sub)
SiteType <- factor(SiteType,levels=c("FCDay","BCDay","FCSH","DayHike","ShortHike","BCOvernight", "CHOverlook",""))
SiteType[SiteType=="FCDay"|SiteType=="BCDay"] <- "DayHike"
SiteType[SiteType=="FCSH"] <- "ShortHike"
SiteType[SiteType==""] <- NA
DRMerged2011sub$SiteType <- SiteType

levels(HearAircraft)
HearAircraft[HearAircraft==""|HearAircraft==" "] <- NA
HearAircraft <- factor(HearAircraft)
levels(HearAircraft)
DRMerged2011sub$HearAircraft <- HearAircraft
table(DRMerged2011sub$Survey,DRMerged2011sub$HearAircraft)

levels(InterfereNatQuiet)
InterfereNatQuiet[InterfereNatQuiet==" NotAtAll"|InterfereNatQuiet=="Not at All"|InterfereNatQuiet=="Not At All"] <- "NotAtAll"
InterfereNatQuiet[InterfereNatQuiet==""|InterfereNatQuiet==" "] <- NA
InterfereNatQuiet <- factor(InterfereNatQuiet)
levels(InterfereNatQuiet)
DRMerged2011sub$InterfereNatQuiet <- InterfereNatQuiet

levels(ImpCalmPeace)
ImpCalmPeace[ImpCalmPeace=="Not At All"] <- "NotAtAll"
ImpCalmPeace[ImpCalmPeace==""|ImpCalmPeace==" "] <- NA
ImpCalmPeace[ImpCalmPeace=="moderately"] <- "Moderately"
ImpCalmPeace <- factor(ImpCalmPeace)
levels(ImpCalmPeace)
DRMerged2011sub$ImpCalmPeace <- ImpCalmPeace

levels(ImpViewScenery)
ImpViewScenery[ImpViewScenery==""|ImpViewScenery==" "] <- NA
ImpViewScenery <- factor(ImpViewScenery)
levels(ImpViewScenery)
DRMerged2011sub$ImpViewScenery <- ImpViewScenery

levels(ImpHistCult)
ImpHistCult[ImpHistCult=="Not At All"] <- "NotAtAll"
ImpHistCult[ImpHistCult=="Slightly "] <- "Slightly"
ImpHistCult[ImpHistCult==""|ImpHistCult==" "] <- NA
ImpHistCult <- factor(ImpHistCult)
levels(ImpHistCult)
DRMerged2011sub$ImpHistCult <- ImpHistCult

levels(ImpAdventure)
ImpAdventure[ImpAdventure=="Not At All"] <- "NotAtAll"
ImpAdventure[ImpAdventure=="Slightly "] <- "Slightly"
ImpAdventure[ImpAdventure==""|ImpAdventure==" "] <- NA
ImpAdventure <- factor(ImpAdventure)
levels(ImpAdventure)
DRMerged2011sub$ImpAdventure <- ImpAdventure

levels(AircraftAnnoy)
AircraftAnnoy9pt <- AircraftAnnoy
#########
#Aside to check distributions of 9 point scale
AircraftAnnoy9pt[AircraftAnnoy9pt=="Slightly"] <- "Slightly Annoying"
AircraftAnnoy9pt[AircraftAnnoy9pt=="Moderately"] <- "Moderately Annoying"
AircraftAnnoy9pt[AircraftAnnoy9pt=="Very"] <- "Very Annoying"
AircraftAnnoy9pt[AircraftAnnoy9pt=="Extremely"] <- "Extremely Annoying"
AircraftAnnoy9pt[AircraftAnnoy9pt==""|AircraftAnnoy9pt==" "] <- NA
levels(AircraftAnnoy9pt)
Survey <- factor(Survey)
AircraftAnnoy9pt <- factor(AircraftAnnoy9pt, levels=c("Extremely Pleasing", "Very Pleasing", "Moderately Pleasing", "Slightly Pleasing", "Neutral", "NotAtAll", "Slightly Annoying", "Moderately Annoying", "Very Annoying", "Extremely Annoying"))
levels(AircraftAnnoy9pt)

table(AircraftAnnoy9pt,Survey)

library(ggplot2)
par(mfrow=c(1,3))
histogram(AircraftAnnoy9pt[Survey=="AC"])
histogram(AircraftAnnoy9pt[Survey=="HR1"])
histogram(AircraftAnnoy9pt[Survey=="HR2"])

##########
AircraftAnnoy[AircraftAnnoy=="Extremely Pleasing"|AircraftAnnoy=="Very Pleasing"|AircraftAnnoy=="Moderately Pleasing"|AircraftAnnoy=="Slightly Pleasing"|AircraftAnnoy=="Neutral"] <- "NotAtAll"
AircraftAnnoy[AircraftAnnoy=="Slightly Annoying"] <- "Slightly"
AircraftAnnoy[AircraftAnnoy=="Moderately Annoying"] <- "Moderately"
AircraftAnnoy[AircraftAnnoy=="Very Annoying"] <- "Very"
AircraftAnnoy[AircraftAnnoy=="Extremely Annoying"] <- "Extremely"
AircraftAnnoy[AircraftAnnoy==""|AircraftAnnoy==" "] <- NA
AircraftAnnoy <- factor(AircraftAnnoy)
levels(AircraftAnnoy)
DRMerged2011sub$AircraftAnnoy <- AircraftAnnoy

AircraftAnnoy <- factor(AircraftAnnoy, levels=c("NotAtAll", "Slightly", "Moderately", "Very", "Extremely"))

par(mfrow=c(1,3))
histogram(AircraftAnnoy[Survey=="AC"])
histogram(AircraftAnnoy[Survey=="HR1"])
histogram(AircraftAnnoy[Survey=="HR2"])


remove("AircraftAnnoy", "SiteType", "InterfereNatQuiet", "ImpCalmPeace", "ImpViewScenery", "ImpHistCult", "ImpAdventure", "HearAircraft")

detach(DRMerged2011sub)

#Remove data from the Overlook site
DRMerged2011sub <- subset(DRMerged2011sub,DRMerged2011sub$SiteType != "CHOverlook") #Removes 402 rows
DRMerged2011sub$SiteType <- factor(DRMerged2011sub$SiteType)
table(DRMerged2011sub$SiteType)
dim(DRMerged2011sub) 

###################################################################################
#Add transformed dose variables to the merged file

#Attach merged data frame for transformations
attach(DRMerged2011sub)

#Create new derived variables according to formulas in the "Dose Key" from the Excel spreadsheet 
LeqTresp = SELAllAC-10*log10(DurVisitMinutes*60)
PTAudAllAC = 100*(AudDurAllACMinutes/DurVisitMinutes)
LeqHelos = HierSELHelos - 10*log10(DurVisitMinutes*60)
PTAudHelos = 100*(AudDurHelosMinutes/DurVisitMinutes)
LeqProps = HierSELProps - 10*log10(DurVisitMinutes*60)
PTAudProps	=100* (HierAudDurPropsMinutes/DurVisitMinutes)
LeqJets	=HierSELJets - 10*log10(DurVisitMinutes*60)
PTAudJets	=100* (HierAudDurJetsMinutes/DurVisitMinutes)
PEnHelos	=100*((10^(HierSELHelos/10))/(10^(SELAllAC/10)))
PEnHelos[which(is.na(HierSELHelos) & SELAllAC>0)] <- 0
PEnProps	=100*((10^(HierSELProps/10))/(10^(SELAllAC/10)))
PEnProps[which(is.na(HierSELProps) & SELAllAC>0)] <- 0
LeqTAC = SELAllAC-10*log10(AudDurAllACMinutes*60)
lg10.PTAudAllAC = log10(100*(AudDurAllACMinutes/DurVisitMinutes))
lg10.DurVisitMinutes = log10(DurVisitMinutes)

#Merge derived variables with the merged data subsets from all sites
DRMerged2011sub <- data.frame(DRMerged2011sub,LeqTresp,LeqTAC,PTAudAllAC,lg10.PTAudAllAC,LeqHelos,PTAudHelos,LeqProps,PTAudProps,LeqJets,PTAudJets,PEnHelos,PEnProps,lg10.DurVisitMinutes)

#Remove the derived variables from the workspace
remove("LeqTresp","LeqTAC", "PTAudAllAC","lg10.PTAudAllAC", "LeqHelos","PTAudHelos","LeqProps","PTAudProps","LeqJets","PTAudJets","PEnHelos","PEnProps", "lg10.DurVisitMinutes")

#detach the merged data frame
detach(DRMerged2011sub)

#Check the column names
names(DRMerged2011sub)

###################################################################################
#Dichotomizations of response variables used in the dose-response models
attach(DRMerged2011sub)

#Create a new vector to dichotomize ImpNatQuiet.
#Note: "Not Relevant" and "Not relevant" are included as "No". Missing or blank values are included as "NA".
levels(ImpNatQuiet)
ImpNQ_VorMore <- rep(NA,length(ImpNatQuiet))
ImpNQ_VorMore[ImpNatQuiet=="Extremely"|ImpNatQuiet=="Very"] <- "Yes"
ImpNQ_VorMore[ImpNatQuiet=="Moderately"|ImpNatQuiet=="Not relevant"|ImpNatQuiet=="NotAtAll"|ImpNatQuiet=="Slightly"|ImpNatQuiet=="Not Relevant"|ImpNatQuiet=="Not At All"] <- "No"
ImpNQ_VorMore[ImpNatQuiet==""|ImpNatQuiet==" "] <- NA
ImpNQ_VorMore <- as.factor(ImpNQ_VorMore)
table(ImpNQ_VorMore)

#Create a new vector that to dichotomize ImpCalmPeace.
#Note: "Not Relevant" and "Not relevant" are included as "No". Missing or blank values are included as "NA".
levels(ImpCalmPeace)
ImpCP_VorMore <- rep(NA,length(ImpCalmPeace))
ImpCP_VorMore[ImpCalmPeace=="Extremely"|ImpCalmPeace=="Very"] <- "Yes"
ImpCP_VorMore[ImpCalmPeace=="Moderately"|ImpCalmPeace=="Not relevant"|ImpCalmPeace=="NotAtAll"|ImpCalmPeace=="Slightly"|ImpCalmPeace=="Not Relevant"] <- "No"
ImpCP_VorMore <- as.factor(ImpCP_VorMore)
table(ImpCP_VorMore)

#Create a new vector that to dichotomize ImpHistCult.
#Note: "Not Relevant" and "Not relevant" are included as "No". Missing or blank values are included as "NA".
levels(ImpHistCult)
ImpHC_VorMore <- rep(NA,length(ImpHistCult))
ImpHC_VorMore[ImpHistCult=="Extremely"|ImpHistCult=="Very"] <- "Yes"
ImpHC_VorMore[ImpHistCult=="Moderately"|ImpHistCult=="Not relevant"|ImpHistCult=="NotAtAll"|ImpHistCult=="Slightly"|ImpHistCult=="Not Relevant"] <- "No"
ImpHC_VorMore <- as.factor(ImpHC_VorMore)
table(ImpHC_VorMore)

#Create a new vector that to dichotomize ImpAdventure.
#Note: "Not Relevant" and "Not relevant" are included as "No". Missing or blank values are included as "NA".
levels(ImpAdventure)
ImpA_VorMore <- rep(NA,length(ImpHistCult))
ImpA_VorMore[ImpAdventure=="Extremely"|ImpHistCult=="Very"] <- "Yes"
ImpA_VorMore[ImpAdventure=="Moderately"|ImpHistCult=="Not relevant"|ImpAdventure=="NotAtAll"|ImpAdventure=="Slightly"|ImpAdventure=="Not Relevant"] <- "No"
ImpA_VorMore <- as.factor(ImpA_VorMore)
table(ImpA_VorMore)

#Create a new vector that to dichotomize ImpViewScenery.
#Note: "Not Relevant" and "Not relevant" are included as "No". Missing or blank values are included as "NA".
levels(ImpViewScenery)
ImpVS_VorMore <- rep(NA,length(ImpViewScenery))
ImpVS_VorMore[ImpViewScenery=="Extremely"|ImpViewScenery=="Very"] <- "Yes"
ImpVS_VorMore[ImpViewScenery=="Moderately"|ImpViewScenery=="Not relevant"|ImpViewScenery=="NotAtAll"|ImpViewScenery=="Slightly"|ImpViewScenery=="Not Relevant"] <- "No"
ImpVS_VorMore <- as.factor(ImpVS_VorMore)
table(ImpVS_VorMore)


#Create a new vector (AdultsOnly) to dichotomize NumChild.
AdultsOnly <- rep(NA,length(NumChild))
AdultsOnly[NumChild==0|NumChild==NA] <- "Yes"
AdultsOnly[NumChild>0] <- "No"
AdultsOnly <- as.factor(AdultsOnly)

#Create a new vector (EarlyStart) to dichotomize those who began before crowds (9am).
hist(HikeBeginMinAfterMidnt)
EarlyStart <- rep(NA,length(HikeBeginMinAfterMidnt))
EarlyStart[HikeBeginMinAfterMidnt<540|HikeBeginMinAfterMidnt==NA] <- "Yes"
EarlyStart[HikeBeginMinAfterMidnt>=540] <- "No"
EarlyStart <- as.factor(EarlyStart)
table(EarlyStart)

##Create a new vector (SiteVisitBefore) from the variable SiteFirstVisit to match the prior analysis.
SiteVisitBefore <- rep(NA,length(SiteFirstVisit))
levels(SiteFirstVisit)
SiteFirstVisit[SiteFirstVisit==""|SiteFirstVisit==" "] <- NA
SiteVisitBefore[SiteFirstVisit=="No"] <- "Yes"
SiteVisitBefore[SiteFirstVisit=="Yes"] <- "No"
SiteVisitBefore <- factor(SiteVisitBefore)
levels(SiteVisitBefore)

################################
#Create a new vector for InterfereNatQuiet in three ways.
#Transform 1 (HR1, HR2):  Resp. who received a dose (defined as DataDurAllAC>0), but indicated they did not hear aircraft (responded "No" to "HearAircraft") are recoded as "NotAtAll".
InterfereNatQuiet[which(AudDurAllACMinutes > 0 & HearAircraft == "No")] <- "NotAtAll"
DRMerged2011sub$InterfereNatQuiet <- InterfereNatQuiet

#Transform 2:  Dichotomize 3 ways:   slightly or more, moderately or more, very or more.
levels(InterfereNatQuiet)

#Slightly or more
#NOTE: "Not Relevant" is currently coded as "NA"
IntWithNQ_SorMore <- rep(NA,length(InterfereNatQuiet))
IntWithNQ_SorMore[InterfereNatQuiet=="Slightly"|InterfereNatQuiet=="Slightly/Moderately"|InterfereNatQuiet=="Moderately"|InterfereNatQuiet=="Very"|InterfereNatQuiet=="Extremely"] <- "Yes"
IntWithNQ_SorMore[InterfereNatQuiet=="NotAtAll"] <- "No"
IntWithNQ_SorMore <- as.factor(IntWithNQ_SorMore)
levels(IntWithNQ_SorMore)
table(IntWithNQ_SorMore)

#Moderately or more
#NOTE: "Not Relevant" is currently coded as "NA"
IntWithNQ_MorMore <- rep(NA,length(InterfereNatQuiet))
IntWithNQ_MorMore[InterfereNatQuiet=="Moderately"|InterfereNatQuiet=="Very"|InterfereNatQuiet=="Extremely"] <- "Yes"
IntWithNQ_MorMore[InterfereNatQuiet=="NotAtAll"|InterfereNatQuiet=="Slightly"|InterfereNatQuiet=="Slightly/Moderately"] <- "No"
IntWithNQ_MorMore <- as.factor(IntWithNQ_MorMore)
levels(IntWithNQ_MorMore)
table(IntWithNQ_MorMore)

#Very or more
#NOTE: "Not Relevant" is currently coded as "NA"
IntWithNQ_VorMore <- rep(NA,length(InterfereNatQuiet))
IntWithNQ_VorMore[InterfereNatQuiet=="Very"|InterfereNatQuiet=="Extremely"] <- "Yes"
IntWithNQ_VorMore[InterfereNatQuiet=="NotAtAll"|InterfereNatQuiet=="Slightly"|InterfereNatQuiet=="Slightly/Moderately"|InterfereNatQuiet=="Moderately"] <- "No"
IntWithNQ_VorMore <- as.factor(IntWithNQ_VorMore)
levels(IntWithNQ_VorMore)
table(IntWithNQ_VorMore)

################################
#Create a new vector for AicraftSoundAnnoy in three ways.
#Transform 1 (HR1, AC):  Resp. who received a dose (defined as DataDurAllAC>0), but indicated they did not hear aircraft (responded "No" to "HearAircraft") are recoded as "NotAtAll".
AircraftAnnoy[which(AudDurAllACMinutes > 0 & HearAircraft == "No")] <- "NotAtAll"
DRMerged2011sub$AircraftAnnoy <- AircraftAnnoy

#Transform 2:  Dichotomize 3 ways:   slightly or more, moderately or more, very or more.
levels(AircraftAnnoy)

#Slightly or more
#NOTE: "Not Relevant" is currently coded as "NA"
Annoy_SorMore <- rep(NA,length(AircraftAnnoy))
Annoy_SorMore[AircraftAnnoy=="Slightly"|AircraftAnnoy=="Slightly/Moderately"|AircraftAnnoy=="Moderately"|AircraftAnnoy=="Very"|AircraftAnnoy=="Extremely"] <- "Yes"
Annoy_SorMore[AircraftAnnoy=="NotAtAll"] <- "No"
Annoy_SorMore <- as.factor(Annoy_SorMore)
levels(Annoy_SorMore)
table(Annoy_SorMore,Survey)

#Moderately or more
#NOTE: "Not Relevant" is currently coded as "NA"
Annoy_MorMore <- rep(NA,length(AircraftAnnoy))
Annoy_MorMore[AircraftAnnoy=="Moderately"|AircraftAnnoy=="Very"|AircraftAnnoy=="Extremely"] <- "Yes"
Annoy_MorMore[AircraftAnnoy=="NotAtAll"|AircraftAnnoy=="Slightly"|AircraftAnnoy=="Slightly/Moderately"] <- "No"
Annoy_MorMore <- as.factor(Annoy_MorMore)
levels(Annoy_MorMore)
table(Annoy_MorMore)

#Very or more
#NOTE: "Not Relevant" is currently coded as "NA"
Annoy_VorMore <- rep(NA,length(AircraftAnnoy))
Annoy_VorMore[AircraftAnnoy=="Very"|AircraftAnnoy=="Extremely"] <- "Yes"
Annoy_VorMore[AircraftAnnoy=="NotAtAll"|AircraftAnnoy=="Slightly"|AircraftAnnoy=="Slightly/Moderately"|AircraftAnnoy=="Moderately"] <- "No"
Annoy_VorMore <- as.factor(Annoy_VorMore)
levels(Annoy_VorMore)
table(Annoy_VorMore)

#Merge transformed variables with DRMerged2011sub
DRMerged2011sub <- data.frame(DRMerged2011sub,ImpNQ_VorMore,ImpCP_VorMore, ImpVS_VorMore, ImpA_VorMore, ImpHC_VorMore, AdultsOnly, EarlyStart, SiteVisitBefore,IntWithNQ_SorMore, IntWithNQ_MorMore, IntWithNQ_VorMore, Annoy_SorMore, Annoy_MorMore, Annoy_VorMore)

#Remove variables from workspace
remove("ImpNQ_VorMore","ImpCP_VorMore", "ImpVS_VorMore", "ImpA_VorMore", "ImpHC_VorMore", "AdultsOnly","SiteVisitBefore", "EarlyStart", "IntWithNQ_SorMore", "IntWithNQ_MorMore", "IntWithNQ_VorMore", "Annoy_SorMore", "Annoy_MorMore", "Annoy_VorMore")

detach(DRMerged2011sub)
str(DRMerged2011sub)

#######################################################################
#Filter d prime data to limit range
#We should include only DprimeLSELAllAC 0-80 and both DprimeLLeqTAC and DprimeLeqTresp 0-50.
dim(DRMerged2011sub)
DRMerged2011sub <- subset(DRMerged2011sub,DRMerged2011sub$DprimeLSELAllAC <= 80)
DRMerged2011sub <- subset(DRMerged2011sub,DRMerged2011sub$DprimeLLeqTAC <= 50)
DRMerged2011sub <- subset(DRMerged2011sub, DprimeLeqTresp <= 50)
dim(DRMerged2011sub)

#######################################################################

#Change names in dataset to match code for model fitting
names(DRMerged2011sub)[names(DRMerged2011sub)=="site"] <- "Site"
Dataset <- rep("Prior",nrow(DRMerged2011sub))
SeqAll <- c(1:nrow(DRMerged2011sub))

DRMerged2011sub <- data.frame(DRMerged2011sub,Dataset,SeqAll)
names(DRMerged2011sub)

DRMerged2011sub <- subset(DRMerged2011sub,DRMerged2011sub$Survey!="")
DRMerged2011sub <- droplevels(DRMerged2011sub)
levels(DRMerged2011sub$Survey)

#Save full datafile to use for model fitting
write.csv(DRMerged2011sub, file="~/Documents/Documents/Admin/Volpe/Rwork/DprimeScripts/DRMerged2011subset_dprime.csv")
names(DRMerged2011sub)
dim(DRMerged2011sub)
str(DRMerged2011sub)
table(DRMerged2011sub$Survey,DRMerged2011sub$Annoy_SorMore)
table(DRMerged2011sub$Survey,DRMerged2011sub$HearAircraft)

#######################################################################
#Subset to include complete cases of dose variables
complete.dose <- c("Site", "LmaxAllAC", "SELAllAC", "PTAudAllAC", "LeqTresp", "LeqTAC", "L50NatQuiet", "PEnHelos", "PEnProps", "SiteType", "ImpNQ_VorMore", "ImpHC_VorMore", "ImpVS_VorMore", "ImpCP_VorMore",  "SiteVisitBefore", "AdultsOnly", "Survey", "Annoy_SorMore", "Annoy_MorMore", "Annoy_VorMore", "EarlyStart", "Talk")

dose.cols <- DRMerged2011sub[,complete.dose]
dose.rows <- complete.cases(dose.cols)
length(dose.rows)

DRMerged2011subComplete <- DRMerged2011sub[dose.rows==TRUE,]
dim(DRMerged2011subComplete)
table(DRMerged2011subComplete$Survey)
table(DRMerged2011subComplete$Survey,DRMerged2011subComplete$HearAircraft)

write.csv(DRMerged2011subComplete, file="~/Documents/Documents/Admin/Volpe/Rwork/DprimeScripts/ATMP2011_CompleteDoseVars_dprime.csv")

#######################################################################
#check HearAircraft variable for comapring survey types
table(DRMerged2011subComplete$Annoy_SorMore)#,DRMerged2011subComplete$Survey)
