### Script to test alternative dose variables (published model uses LmaxAllAC) using the published model fit to new data (DRMerged2011subset.csv)
### Erika A Sudderth March 14, 2013. Modified from scripts by Grant Anderson ()

### The code below produces the model fits to the new (2011) survey data that using the published models that were determined to have the best fit to the older survey data (Noise Control Eng. J. 59(5):519-540). Additional mediator and dose variables will be added and tested following the published model selection procedures.
				

###########################################################################
#### MAKE THESE MODIFICATIONS/CHOICES EACH TIME SCRIPT IS RUN
###########################################################################

##	DO NOT CHANGE THESE
			rm(list = ls())					### Clears all variables
      require(ggplot2)
      library("arm")					### Required library
   		library("lme4")					### Required library
      library("stats")


#################
##  EAS: Load the csv file generated in the "ATMP_DR2011_NewDataProcessV2.r" script
#Data = DR2011
  setwd("F:/Volpe/Rwork/DprimeScripts")
  Data <- read.csv("ATMP2011_CompleteDoseVars_dprime.csv")
  names(Data) 
  dim(Data)
  DataType = "AllCorrectedOnlyPrior"

#######################
#SiteType: remove ShortHike data from new dataset for now
dim(Data)
#Data <- subset(Data,Data$SiteType != "ShortHike") #Removes ~300 rows
Data$SiteType <- factor(Data$SiteType) 
dim(Data)
table(Data$SiteType)
table(Data$Site)


##List of dose variables, mitigator variables, interactions, and dichotomizations used in all models
vars.dos <- c("LmaxAllAC","SELAllAC", "PTAudAllAC","LeqTresp", "LeqTAC", "L50NatQuiet", "PEnHelos", "PEnProps", "PTAudHelos", "PTAudProps", "PTAudJets", "LeqHelos", "LeqProps", "LeqJets", "HierSELHelos", "HierSELProps", "HierSELJets", "HierLmaxHelos", "HierLmaxProps", "HierLmaxJets", "DprimeLSELAllAC", "DprimeLLeqTAC", "DprimeLeqTresp", "X.TAboveDprimeL17", "DprimeL90", "DprimeL50", "DprimeL10")

# 

vars.mit = 			c("SiteType", "ImpNQ_VorMore", "SiteVisitBefore",  "AdultsOnly")		###For OnlyPrior: "SiteType"
vars.interact =	c("I(PEnHelos * PEnProps)", "I(PTAudHelos * PTAudProps)")		###For PEn: "I(PEnHelos * PEnProps)"

vars.dicot =c("Annoy_SorMore", "Annoy_MorMore", "Annoy_VorMore")
##  ADDITIONAL DESIRED DATA (must have SeqAll in it)
AddData = c("Dataset", "SeqAll", "DurVisitMinutes", "Survey", "AircraftAnnoy")  				# Plus others, if desired during simulation

###### Assemble all variables and data used for all regressions:
varnames.na = c("Site", vars.dos, vars.mit, AddData, vars.dicot)
vars.all = Data[varnames.na]													
#vars.all = na.omit(vars.all) #omit rows with missign data for any variable
vars.all = vars.all[is.na(vars.all$Site)=="FALSE",]
dim(vars.all)
table(vars.all$Site)

##  DURATION RESTRICTIONS FPR DayHikes
##  Fill in this value (minimum minutes for DurVisit)
minDurVisit = 0    					## Only for DayHikes
maxDurVisit = 9000  						## For Overnight hikes

vars.all = subset(vars.all, vars.all$DurVisitMinutes > minDurVisit)
vars.all = subset(vars.all, vars.all$DurVisitMinutes < maxDurVisit) #Use for Overnight data
dim(vars.all)
table(vars.all$Site)

##################
#Correlations among dose variables
vars.dos.cor <- c("LmaxAllAC","SELAllAC", "PTAudAllAC","LeqTresp", "LeqTAC", "DprimeLSELAllAC", "DprimeLLeqTAC", "DprimeLeqTresp", "X.TAboveDprimeL17")

dose.cor <- cor(vars.all[,vars.dos.cor])
write.csv(dose.cor, file="DoseCors.csv")


#Figures for Report vol2: Analysis

#Create new dataframe to make plots by aircraft type
#SEL in 3 groups: SELHelos, SELProps, SELJets - NEED TO ADD
#Leq in 3 groups: LeqHelos, LeqProps, LeqJets  
#PTaud in 3 groups: PTAudHelos, PTAudProps, PTAudJets.
names(vars.all)
dim(vars.all)

Leq <- c(vars.all$LeqHelos, vars.all$LeqProps, vars.all$LeqJets)
length(Leq)

PTAud <- c(vars.all$PTAudHelos, vars.all$PTAudProps, vars.all$PTAudJets)
length(PTAud)

SEL <- c(vars.all$HierSELHelos, vars.all$HierSELProps, vars.all$HierSELJets)
length(SEL)

Lmax <- c(vars.all$HierLmaxHelos, vars.all$HierLmaxProps, vars.all$HierLmaxJets)
length(Lmax)

Aircraft <- as.factor(gl(3,nrow(vars.all),labels=c("Helos","Props","Jets")))
length(Aircraft)
Aircraft[1:100]

SiteType <-as.factor(rep(vars.all$SiteType,3))
length(SiteType)

Site <- as.factor(rep(vars.all$Site,3))
length(Site)

DoseVars <- data.frame(Site,SiteType,Aircraft,Leq, PTAud, SEL)
dim(DoseVars)

#Add factor for full site names

vars.all$SiteLong <- factor(vars.all$Site, levels=c("Fairyland", "Hlake", "Sperry", "Grandview", "Hermit", "TylrCrk", "Wrim"), labels=c("Fairyland (Bryce Canyon)","Hidden Lake (Glacier)", "Sperry (Glacier)","Grandview (Grand Canyon)", "Hermit (Grand Canyon)", "Taylor Creek (Zion)", "West Rim (Zion)"))

#########################
#Function to relabel Sites
mf_labeller <- function(var, value){
  value <- as.character(value)
  if (var=="Site") { 
    value[value=="Fairyland"] <- "Fairyland (Bryce Canyon)"
    value[value=="Grandview"]   <- "Grandview (Grand Canyon)"
    value[value=="Hermit"]   <- "Hermit (Grand Canyon)"
    value[value=="Hlake"]   <- "Hidden Lake (Glacier)"
    value[value=="Sperry"]   <- "Sperry (Glacier)"
    value[value=="TylrCrk"]   <- "Taylor Creek (Zion)"
    value[value=="Wrim"]   <- "West Rim (Zion)"
  }
  return(value)
}

#Axis labels to use
# SELAllAC = LAE (dBA)
# LeqTresp = LAeq,Tresp (dBA)
# LeqTac = L Aeq,Tac (dBA)
# LMaxAllAC = LASmx (dBA)
#PTAudAllAC = TAud (%)

#EPS file of Figures for JASA article (revisions submitted February 2015)

postscript("LAEboxplots.eps", width = 8, height = 8, 
           horizontal = FALSE, onefile = FALSE, paper = "special", 
           colormodel = "cmyk", family = "Arial")
par(mar = c(3.5, 3.5, 1, 1), mgp = c(2, 0.7, 0), tck = -0.01)

SELbox <- ggplot(vars.all, aes(factor(SiteLong), DprimeLSELAllAC)) #+ xlim(20,100) 
SELbox + geom_boxplot() + ylab("A-weighted Sound Exposure Level (dB)")  + xlab("Site") + theme_bw(20) + theme(strip.background=element_rect(fill="white"), panel.grid.minor=element_blank(), panel.grid.major=element_blank(), strip.text.x = element_text(size=16, face="bold"),strip.text.y = element_text(size=16, face="bold"), axis.title = element_text(face="bold", size=18), axis.text  = element_text(size=14, color="black"), axis.text.x  = element_text(angle=90, vjust=.5, size=16), axis.text.y  = element_text(vjust=.5)) + scale_color_manual(values=colors.Site) + scale_fill_manual(values=colors.Site) + theme(legend.position = "none") 

dev.off()

#PDF file of Histograms of dose variables with all respondents grouped together(?)

pdf(file = paste("ReportVol2Figs_01132015",".pdf",sep=""), width = 12, height = 12, onefile = TRUE, family = "Helvetica")

#histogram plots: SEL, LeqTresp, Lmax, PTAud, and L50Nat, plus D' metrics

colors.Site <- c("Fairyland"= "darkolivegreen3", "Grandview"="deepskyblue3", "Hermit"="deepskyblue3", "Hlake"="darkolivegreen3", "Sperry"="deepskyblue3", "Tusayan"="mediumpurple3", "TylrCrk"="darkolivegreen3", "Wrim"="deepskyblue3")

hist_cut <- ggplot(vars.all, aes(x=SELAllAC)) #+ xlim(20,100)
hist_cut + geom_histogram(aes(y= ..count.., color=Site, fill = Site)) + facet_grid(Site~., labeller=mf_labeller) + xlab("LAE (dBA)") + theme_bw(20) + theme(strip.background=element_rect(fill="white"), strip.text.x = element_text(size=16, face="bold"),strip.text.y = element_text(size=18, face="bold"), axis.title = element_text(face="bold", size=18), axis.text  = element_text(size=16, color="black")) + scale_color_manual(values=colors.Site) + scale_fill_manual(values=colors.Site) + theme(legend.position = "none") 

hist_cut <- ggplot(vars.all, aes(x=LeqTresp)) #+ xlim(20,100)
hist_cut + geom_histogram(aes(y= ..count.., color=Site, fill = Site)) + facet_grid(Site~., labeller=mf_labeller) + xlab("LAeq,Tresp (dBA)") + theme_bw(20) + theme(strip.background=element_rect(fill="white"), strip.text.x = element_text(size=16, face="bold"),strip.text.y = element_text(size=18, face="bold"), axis.title = element_text(face="bold", size=18), axis.text  = element_text(size=16, color="black")) + scale_color_manual(values=colors.Site) + scale_fill_manual(values=colors.Site) + theme(legend.position = "none") 

hist_cut <- ggplot(vars.all, aes(x=LmaxAllAC)) #+ xlim(20,100)
hist_cut + geom_histogram(aes(y= ..count.., color=Site, fill = Site)) + facet_grid(Site~., labeller=mf_labeller) + xlab("LASmx (dBA)")  + theme_bw(20) + theme(strip.background=element_rect(fill="white"), strip.text.x = element_text(size=16, face="bold"),strip.text.y = element_text(size=18, face="bold"), axis.title = element_text(face="bold", size=18), axis.text  = element_text(size=16, color="black")) + scale_color_manual(values=colors.Site) + scale_fill_manual(values=colors.Site) + theme(legend.position = "none") 

hist_cut <- ggplot(vars.all, aes(x=PTAudAllAC)) #+ xlim(20,100)
hist_cut + geom_histogram(aes(y= ..count.., color=Site, fill = Site)) + facet_grid(Site~., labeller=mf_labeller) + xlab("TAud (%)")  + theme_bw(20) + theme(strip.background=element_rect(fill="white"), strip.text.x = element_text(size=16, face="bold"),strip.text.y = element_text(size=18, face="bold"), axis.title = element_text(face="bold", size=18), axis.text  = element_text(size=16, color="black")) + scale_color_manual(values=colors.Site) + scale_fill_manual(values=colors.Site) + theme(legend.position = "none") 

hist_cut <- ggplot(vars.all, aes(x=L50NatQuiet)) #+ xlim(20,100)
hist_cut + geom_histogram(aes(y= ..count.., color=Site, fill = Site)) + facet_grid(Site~., labeller=mf_labeller) + xlab("L50NatQuiet")  + theme_bw(20) + theme(strip.background=element_rect(fill="white"), strip.text.x = element_text(size=16, face="bold"),strip.text.y = element_text(size=18, face="bold"), axis.title = element_text(face="bold", size=18), axis.text  = element_text(size=16, color="black")) + scale_color_manual(values=colors.Site) + scale_fill_manual(values=colors.Site) + theme(legend.position = "none") 

# DâL abbreviations:
#   
#   DprimeLSELAllAC      DâLE  (dB)
# DprimeLLeqTAC         DâLeq,Tac (dB)
# DprimeLeqTresp         DâLeq,Tresp (dB)
# DprimeL90                  DâL90 (dB)
# DprimeL50                  DâL50 (dB)
# Percent time DL>=17            %TN (short for percent time noticeable)       

hist_cut <- ggplot(vars.all, aes(x=DprimeLSELAllAC)) #+ xlim(20,100)
hist_cut + geom_histogram(aes(y= ..count.., color=Site, fill = Site)) + facet_grid(Site~., labeller=mf_labeller) + xlab("D'LE  (dB)")  + theme_bw(20) + theme(strip.background=element_rect(fill="white"), strip.text.x = element_text(size=16, face="bold"),strip.text.y = element_text(size=18, face="bold"), axis.title = element_text(face="bold", size=18), axis.text  = element_text(size=16, color="black")) + scale_color_manual(values=colors.Site) + scale_fill_manual(values=colors.Site) + theme(legend.position = "none") 

hist_cut <- ggplot(vars.all, aes(x=DprimeLLeqTAC)) #+ xlim(20,100)
hist_cut + geom_histogram(aes(y= ..count.., color=Site, fill = Site)) + facet_grid(Site~., labeller=mf_labeller) + xlab("D'Leq,Tac (dB)")  + theme_bw(20) + theme(strip.background=element_rect(fill="white"), strip.text.x = element_text(size=16, face="bold"),strip.text.y = element_text(size=18, face="bold"), axis.title = element_text(face="bold", size=18), axis.text  = element_text(size=16, color="black")) + scale_color_manual(values=colors.Site) + scale_fill_manual(values=colors.Site) + theme(legend.position = "none") 

hist_cut <- ggplot(vars.all, aes(x=DprimeLeqTresp)) #+ xlim(20,100)
hist_cut + geom_histogram(aes(y= ..count.., color=Site, fill = Site)) + facet_grid(Site~., labeller=mf_labeller) + xlab("D'Leq,Tresp (dB)")  + theme_bw(20) + theme(strip.background=element_rect(fill="white"), strip.text.x = element_text(size=16, face="bold"),strip.text.y = element_text(size=18, face="bold"), axis.title = element_text(face="bold", size=18), axis.text  = element_text(size=16, color="black")) + scale_color_manual(values=colors.Site) + scale_fill_manual(values=colors.Site) + theme(legend.position = "none") 

hist_cut <- ggplot(vars.all, aes(x=DprimeL90)) #+ xlim(20,100)
hist_cut + geom_histogram(aes(y= ..count.., color=Site, fill = Site)) + facet_grid(Site~., labeller=mf_labeller) + xlab("D'L90 (dB)")  + theme_bw(20) + theme(strip.background=element_rect(fill="white"), strip.text.x = element_text(size=16, face="bold"),strip.text.y = element_text(size=18, face="bold"), axis.title = element_text(face="bold", size=18), axis.text  = element_text(size=16, color="black")) + scale_color_manual(values=colors.Site) + scale_fill_manual(values=colors.Site) + theme(legend.position = "none") 

hist_cut <- ggplot(vars.all, aes(x=DprimeL50)) #+ xlim(20,100)
hist_cut + geom_histogram(aes(y= ..count.., color=Site, fill = Site)) + facet_grid(Site~., labeller=mf_labeller) + xlab("D'L50 (dB)")  + theme_bw(20) + theme(strip.background=element_rect(fill="white"), strip.text.x = element_text(size=16, face="bold"),strip.text.y = element_text(size=18, face="bold"), axis.title = element_text(face="bold", size=18), axis.text  = element_text(size=16, color="black")) + scale_color_manual(values=colors.Site) + scale_fill_manual(values=colors.Site) + theme(legend.position = "none") 

hist_cut <- ggplot(vars.all, aes(x=X.TAboveDprimeL17)) #+ xlim(20,100)
hist_cut + geom_histogram(aes(y= ..count.., color=Site, fill = Site)) + facet_grid(Site~., labeller=mf_labeller) + xlab("%TN")  + theme_bw(20) + theme(strip.background=element_rect(fill="white"), strip.text.x = element_text(size=16, face="bold"),strip.text.y = element_text(size=18, face="bold"), axis.title = element_text(face="bold", size=18), axis.text  = element_text(size=16, color="black")) + scale_color_manual(values=colors.Site) + scale_fill_manual(values=colors.Site) + theme(legend.position = "none") 

hist_cut <- ggplot(vars.all, aes(x=DurVisitMinutes)) + xlim(0,5000)
hist_cut + geom_histogram(aes(y= ..count.., color=Site, fill = Site)) + facet_grid(Site~., labeller=mf_labeller) + xlab("Visit Duration (minutes)")  + theme_bw(20) + theme(strip.background=element_rect(fill="white"), strip.text.x = element_text(size=16, face="bold"),strip.text.y = element_text(size=18, face="bold"), axis.title = element_text(face="bold", size=18), axis.text  = element_text(size=16, color="black")) + scale_color_manual(values=colors.Site) + scale_fill_manual(values=colors.Site) + theme(legend.position = "none") 


#Aircraft type plots
#Separate aircraft type and site
# On the histograms:
# Iâm on the fence about the scale of the y-axis.  A lot of the information for some of the sites gets lost because of the number of observations we have at Fairyland.  What's your preference on this?

colors <- c("BCOvernight"= "deepskyblue3", "CHOverlook"="mediumpurple3", "DayHike"="darkolivegreen3", "ShortHike"="indianred3")

hist_cut <- ggplot(DoseVars, aes(x=SEL)) + xlim(20,100)
hist_cut + geom_histogram(aes(y= ..count.., color=SiteType, fill = SiteType)) + facet_grid(Site ~ Aircraft, labeller=mf_labeller) + xlab("LAE (dBA)")  + theme_bw(20) + theme(strip.background=element_rect(fill="white"), strip.text.x = element_text(size=16, face="bold"),strip.text.y = element_text(size=18, face="bold"), axis.title = element_text(face="bold", size=18), axis.text  = element_text(size=16, color="black")) + scale_color_manual(values=colors) + scale_fill_manual(values=colors) + theme(legend.text=theme_text(size=16)) + theme(legend.title=theme_text(size=18))

hist_cut <- ggplot(DoseVars, aes(x=Leq)) + xlim(0,100)
hist_cut + geom_histogram(aes(y= ..count.., color=SiteType, fill = SiteType)) + facet_grid(Site ~ Aircraft, labeller=mf_labeller) + xlab("LAeq,Tresp (dBA)")  + theme_bw(20) + theme(strip.background=element_rect(fill="white"), strip.text.x = element_text(size=16, face="bold"),strip.text.y = element_text(size=18, face="bold"), axis.title = element_text(face="bold", size=18), axis.text  = element_text(size=16, color="black")) + scale_color_manual(values=colors) + scale_fill_manual(values=colors) + theme(legend.text=theme_text(size=12)) + theme(legend.title=theme_text(size=14))

hist_cut <- ggplot(DoseVars, aes(x=PTAud)) + xlim(0,100)
hist_cut + geom_histogram(aes(y= ..count.., color=SiteType, fill = SiteType)) + facet_grid(Site ~ Aircraft, labeller=mf_labeller) + xlab("TAud (%)") + theme_bw(20)  + theme(strip.background=element_rect(fill="white"), strip.text.x = element_text(size=16, face="bold"),strip.text.y = element_text(size=18, face="bold"), axis.title = element_text(face="bold", size=18), axis.text  = element_text(size=16, color="black")) + scale_color_manual(values=colors) + scale_fill_manual(values=colors) + theme(legend.text=theme_text(size=12)) + theme(legend.title=theme_text(size=14))

hist_cut <- ggplot(DoseVars, aes(x=Lmax)) + xlim(20,100)
hist_cut + geom_histogram(aes(y= ..count.., color=SiteType, fill = SiteType)) + facet_grid(Site ~ Aircraft, labeller=mf_labeller) + xlab("LASmx (dBA)")  + theme_bw(20) + theme(strip.background=element_rect(fill="white"), strip.text.x = element_text(size=16, face="bold"),strip.text.y = element_text(size=18, face="bold"), axis.title = element_text(face="bold", size=18), axis.text  = element_text(size=16, color="black")) + scale_color_manual(values=colors) + scale_fill_manual(values=colors) + theme(legend.text=theme_text(size=12)) + theme(legend.title=theme_text(size=14))

dev.off()

##########################################################################################
##########################################################################################
##########################################################################################

