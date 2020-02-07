# Dose-response work 2016
# Fitting models of response to aviation noise for overnight, backcountry visitors to 4 national parks
# Dan Flynn | daniel.flynn.ctr@dot.gov

# Set up working directory, read in data and necessary packages

setwd("X:/Overnight") # Map server to local computer

library(lme4)
library(rstan)
library(arm)
library(plyr)
library(ggplot2)

Data <- read.csv("ATMP2011_CompleteDoseVars_dprime.csv")
DataType = "AllCorrectedOnlyPrior"
Data <- subset(Data,Data$SiteType == "BCOvernight") #Removes ~300 rows
Data$SiteType <- factor(Data$SiteType) 

minDurVisit = 60							## Only for DayHikes
maxDurVisit = 9000 						## Use 9000 for Overnight hikes

##  ADDITIONAL DESIRED DATA (must have SeqAll in it)
AddData = c("SeqAll", "DurVisitMinutes", "Survey")		# Plus others, if desired during simulation

##  FILTERING: HR1 and HR2, plus Site
SiteFilterYesNo = "No"
SiteFilterOn = c()


########### Plot

pdf("Best Models Overnight.pdf", width = 6, height = 11)

par(mfrow=c(3,1))

# Annoy and Interfere, overnight hike
# first plot S or more, then M or more and V or more together

colz = c("deepskyblue3",
         "dodgerblue3",
         "midnightblue")

### Annoy_SorMore
# Best model: SELAllAC + PTAudAllAC + PEnHelos + PEnProps + Survey + ImpCP_VorMore

dose.var = "SELAllAC"
vars.dos = c("SELAllAC", "PTAudAllAC", "PEnHelos", "PEnProps")  ###For PEn:
vars.mit = c("Survey", "ImpCP_VorMore", "SiteVisitBefore", "AdultsOnly", "AirTour", "WatchBirds", "lg10.DurVisitMinutes", "Site")

varnames.na = c("Annoy_SorMore", vars.dos, vars.mit, "Dataset", AddData)
vars.all.data = Data[varnames.na]													### Grab from proper data set
vars.all.data = subset(vars.all.data, vars.all.data$DurVisitMinutes > minDurVisit)
vars.all.data = subset(vars.all.data, vars.all.data$DurVisitMinutes < maxDurVisit)
vars.all.data = na.omit(vars.all.data)

# Fit model
annS = glmer(Annoy_SorMore ~ SELAllAC + PTAudAllAC + PEnHelos + PEnProps + Survey + ImpCP_VorMore + AirTour + WatchBirds + lg10.DurVisitMinutes + (1|Site), family=binomial(link="logit"), data = vars.all.data)

summary(annS) # copy in to final resultsfile

annS.curve = glm(Annoy_SorMore ~  SELAllAC + PTAudAllAC + PEnHelos + PEnProps + Survey + ImpCP_VorMore + AirTour + WatchBirds, family=binomial(link="logit"), data = vars.all.data)


plot(vars.all.data$SELAllAC, jitter(as.numeric(vars.all.data$Annoy_SorMore)-1, factor = 0.1), 
     pch = 16, cex = 1.1, 
     #xlim = c(0, 25),
     col = alpha(colz[1], 0.75),
     xlab = "LAE",
     ylab = "Annoyance")

title(main = "Overnight Hikes: Percent Annoy")

xresp = as.numeric(vars.all.data[,"Annoy_SorMore"])-1

cor(xresp, fitted(annS))

curve(predict(annS.curve, data.frame(SELAllAC = x, 
                               PEnHelos = mean(vars.all.data$PEnHelos),
                               PEnProps = mean(vars.all.data$PEnProps),
                               PTAudAllAC = mean(vars.all.data$PTAudAllAC),
                               Survey= vars.all.data$Survey[1],
                               ImpCP_VorMore = vars.all.data$ImpCP_VorMore[1],
                               AirTour = vars.all.data$AirTour[1],
                               WatchBirds = vars.all.data$WatchBirds[1]
                               )
              , type = "resp"), add = TRUE,
      col = colz[1], lwd = 2)

# Add error bars

px <- predict(annS.curve, data.frame(SELAllAC = seq(40, 90, by = 0.1), 
                                     PEnHelos = mean(vars.all.data$PEnHelos),
                                     PEnProps = mean(vars.all.data$PEnProps),
                                     PTAudAllAC = mean(vars.all.data$PTAudAllAC),
                                     Survey= vars.all.data$Survey[1],
                                     ImpCP_VorMore = vars.all.data$ImpCP_VorMore[1],
                                     AirTour = vars.all.data$AirTour[1],
                                     WatchBirds = vars.all.data$WatchBirds[1]),
        type = "resp",
        se.fit = TRUE)

lines(seq(40, 90, by = 0.1),
      px$fit+px$se.fit, 
      col=colz[1], lty = 2)

lines(seq(40, 90, by = 0.1),
      px$fit-px$se.fit, 
      col=colz[1], lty = 2)


legend("topleft",
       lwd = 2,
       col = colz,
       cex = 0.7, 
       inset = 0.06,
       bty = "n",
       legend = c("Slightly or more",
                  "Moderately or more",
                  "Very or more")
        )

#########################################
### Annoy_MorMore
# Best model: SELAllAC + PEnHelos + PEnProps + Survey + ImpCP_VorMore

dose.var = "SELAllAC"
vars.dos = c("SELAllAC", "PEnHelos", "PEnProps", "PTAudAllAC")  ###For PEn:
vars.mit = c("ImpCP_VorMore","AdultsOnly","WatchBirds", "Site")

varnames.na = c("Annoy_MorMore", vars.dos, vars.mit, "Dataset", AddData)
vars.all.data = Data[varnames.na]													### Grab from proper data set
vars.all.data = subset(vars.all.data, vars.all.data$DurVisitMinutes > minDurVisit)
vars.all.data = subset(vars.all.data, vars.all.data$DurVisitMinutes < maxDurVisit)
vars.all.data = na.omit(vars.all.data)

# Fit model
annM = glmer(Annoy_MorMore ~ SELAllAC + PEnHelos + PEnProps + ImpCP_VorMore + AdultsOnly + WatchBirds + (1|Site), family=binomial(link="logit"), data = vars.all.data)

summary(annM) # copy in to final resultsfile

annM.curve = glm(Annoy_MorMore ~ SELAllAC + PEnHelos + PEnProps + ImpCP_VorMore + AdultsOnly + WatchBirds, family=binomial(link="logit"), data = vars.all.data)

plot(vars.all.data$SELAllAC, jitter(as.numeric(vars.all.data$Annoy_MorMore)-1, factor = 0.1), 
     pch = 16, cex = 1.1, 
     xlim = c(40, 90),
     col = alpha(colz[2], 0.75),
     xlab = "SELAllAC",
     ylab = "Annoyance")

xresp = as.numeric(vars.all.data[,"Annoy_MorMore"])-1

cor(xresp, fitted(annM))

curve(predict(annM.curve, data.frame(SELAllAC = x, 
                                     PEnHelos = mean(vars.all.data$PEnHelos),
                                     PEnProps = mean(vars.all.data$PEnProps),
                                     ImpCP_VorMore = vars.all.data$ImpCP_VorMore[1],
                                     WatchBirds = vars.all.data$WatchBirds[1],
                                     AdultsOnly = vars.all.data$AdultsOnly[1]
                                     ), type = "resp"), add = TRUE,
      col = colz[2], lwd = 2)

# Add error bars

px <- predict(annM.curve, data.frame(SELAllAC = seq(40, 90, by = 0.1), 
                                     PEnHelos = mean(vars.all.data$PEnHelos),
                                     PEnProps = mean(vars.all.data$PEnProps),
                                     ImpCP_VorMore = vars.all.data$ImpCP_VorMore[1],
                                     WatchBirds = vars.all.data$WatchBirds[1],
                                     AdultsOnly = vars.all.data$AdultsOnly[1]),
              type = "resp",
              se.fit = TRUE)

lines(seq(40, 90, by = 0.1),
      px$fit+px$se.fit, 
      col=colz[2], lty = 2)

lines(seq(40, 90, by = 0.1),
      px$fit-px$se.fit, 
      col=colz[2], lty = 2)

#########################################
### Annoy_VorMore
# Best model: SELAllAC + PTAudAllAC + PEnHelos + PEnProps + Survey + ImpCP_VorMore + SiteVisitBefore + AdultsOnly + AirTour + WatchBirds + ViewSunRiseSet

dose.var = "SELAllAC"
vars.dos = c("SELAllAC", "PEnHelos", "PEnProps", "PTAudAllAC")  
vars.mit = c("ImpCP_VorMore", "AdultsOnly","WatchBirds", "Site")

varnames.na = c("Annoy_VorMore", vars.dos, vars.mit, "Dataset", AddData)
vars.all.data = Data[varnames.na]													### Grab from proper data set
vars.all.data = subset(vars.all.data, vars.all.data$DurVisitMinutes > minDurVisit)
vars.all.data = subset(vars.all.data, vars.all.data$DurVisitMinutes < maxDurVisit)
vars.all.data = na.omit(vars.all.data)

# Fit model
annV = glmer(Annoy_VorMore ~ SELAllAC + PEnHelos + PEnProps + PTAudAllAC + ImpCP_VorMore +  AdultsOnly  + WatchBirds + (1|Site), family=binomial(link="logit"), data = vars.all.data)

summary(annV) # copy in to final resultsfile

annV.curve = glm(Annoy_VorMore ~ SELAllAC + PEnHelos + PEnProps + PTAudAllAC + ImpCP_VorMore + AdultsOnly + WatchBirds, family=binomial(link="logit"), data = vars.all.data)

plot(vars.all.data$SELAllAC, jitter(as.numeric(vars.all.data$Annoy_VorMore)-1, factor = 0.1), 
     pch = 16, cex = 1.1, 
     xlim = c(40, 100),
     col = alpha(colz[3], 0.75),
     xlab = "SELAllAC",
     ylab = "Annoyance")

xresp = as.numeric(vars.all.data[,"Annoy_VorMore"])-1
cor(xresp, fitted(intV))

curve(predict(annV.curve, data.frame(SELAllAC = x, 
                                     PEnHelos = mean(vars.all.data$PEnHelos),
                                     PEnProps = mean(vars.all.data$PEnProps),
                                     PTAudAllAC = mean(vars.all.data$PTAudAllAC),
                                     AdultsOnly = vars.all.data$AdultsOnly[1],
                                     WatchBirds = vars.all.data$WatchBirds[1],
                                     ImpCP_VorMore = vars.all.data$ImpCP_VorMore[1]), type = "resp"), add = TRUE,
      col = colz[3], lwd = 2)

# Add error bars

px <- predict(annV.curve, data.frame(SELAllAC = seq(40, 100, by = 0.1), 
                                     PEnHelos = mean(vars.all.data$PEnHelos),
                                     PEnProps = mean(vars.all.data$PEnProps),
                                     PTAudAllAC = mean(vars.all.data$PTAudAllAC),
                                     AdultsOnly = vars.all.data$AdultsOnly[1],
                                     WatchBirds = vars.all.data$WatchBirds[1],
                                     ImpCP_VorMore = vars.all.data$ImpCP_VorMore[1]),
              type = "resp",
              se.fit = TRUE)

lines(seq(40, 100, by = 0.1),
      px$fit+px$se.fit, 
      col=colz[3], lty = 2)

lines(seq(40, 100, by = 0.1),
      px$fit-px$se.fit, 
      col=colz[3], lty = 2)

############################################################################################################################################################################################################
# Interfere

### IntWithNQ_SorMore
# Best model: DprimeL50 + PEnHelos + PEnProps + Survey + ImpCP_VorMore

dose.var = SELAllAC#"DprimeL50"# SELAllAC
vars.dos = c("SELAllAC", "PEnHelos", "PEnProps", "PTAudAllAC")  ###For PEn:
vars.mit = c("AdultsOnly", "AirTour", "WatchBirds", "ImpCP_VorMore", "Site")

varnames.na = c("IntWithNQ_SorMore", vars.dos, vars.mit, "Dataset", AddData)
vars.all.data = Data[varnames.na]													### Grab from proper data set
vars.all.data = subset(vars.all.data, vars.all.data$DurVisitMinutes > minDurVisit)
vars.all.data = subset(vars.all.data, vars.all.data$DurVisitMinutes < maxDurVisit)
vars.all.data = na.omit(vars.all.data)


# Fit model
intS = glmer(IntWithNQ_SorMore ~ SELAllAC + PEnHelos + PEnProps + PTAudAllAC + AdultsOnly + AirTour + WatchBirds + ImpCP_VorMore + (1|Site), family=binomial(link="logit"), data = vars.all.data)

summary(intS) # copy in to final resultsfile

intS.curve = glm(IntWithNQ_SorMore ~ SELAllAC + PEnHelos + PEnProps + PTAudAllAC + AdultsOnly + AirTour + WatchBirds + ImpCP_VorMore, family=binomial(link="logit"), data = vars.all.data)


plot(vars.all.data$SELAllAC, jitter(as.numeric(vars.all.data$IntWithNQ_SorMore)-1, factor = 0.1), 
     pch = 16, cex = 1.1, 
     xlim = c(40, 100),
     col = alpha(colz[1], 0.75),
     xlab = "SELAllAC",
     ylab = "Interfere")

title(main = "Overnight Hikes: Percent Interfere")

xresp = as.numeric(vars.all.data[,"IntWithNQ_SorMore"])-1
cor(xresp, fitted(intS))

curve(predict(intS.curve, data.frame(SELAllAC = x, 
                                     PEnHelos = mean(vars.all.data$PEnHelos),
                                     PEnProps = mean(vars.all.data$PEnProps),
                                     PTAudAllAC = mean(vars.all.data$PTAudAllAC),
                                     ImpCP_VorMore = vars.all.data$ImpCP_VorMore[1]
                                     AirTour = vars.all.data$AirTour[1],
                                     WatchBirds = vars.all.data$WatchBirds[1],
                                     AdultsOnly = vars.all.data$AdultsOnly[1]
                                                                          ), type = "resp"), add = TRUE,
      col = colz[1], lwd = 2)

# Add error bars

px <- predict(intS.curve, data.frame(SELAllAC = seq(40, 90, by = 0.1), 
                                     PEnHelos = mean(vars.all.data$PEnHelos),
                                     PEnProps = mean(vars.all.data$PEnProps),
                                     PTAudAllAC = mean(vars.all.data$PTAudAllAC),
                                     ImpCP_VorMore = vars.all.data$ImpCP_VorMore[1],
                                     AirTour = vars.all.data$AirTour[1],
                                     WatchBirds = vars.all.data$WatchBirds[1],
                                     AdultsOnly = vars.all.data$AdultsOnly[1]),
              type = "resp",
              se.fit = TRUE)

lines(seq(40, 90, by = 0.1),
      px$fit+px$se.fit, 
      col=colz[1], lty = 2)

lines(seq(40, 90, by = 0.1),
      px$fit-px$se.fit, 
      col=colz[1], lty = 2)

legend("topleft",
       lwd = 2,
       col = colz,
       cex = 0.7, 
       inset = 0.06,
       bty = "n",
       legend = c("Slightly or more",
                  "Moderately or more",
                  "Very or more")
)

#########################################
### IntWithNQ_MorMore
# Best model: SELAllAC + TAudAllAC + PEnHelos + PEnProps + ImpNQ_VorMore

dose.var = "SELAllAC"
vars.dos = c("SELAllAC", "PTAudAllAC", "PEnHelos", "PEnProps")  ###For PEn:
vars.mit = c("ImpNQ_VorMore", "Site")

varnames.na = c("IntWithNQ_MorMore", vars.dos, vars.mit, "Dataset", AddData)
vars.all.data = Data[varnames.na]													### Grab from proper data set
vars.all.data = subset(vars.all.data, vars.all.data$DurVisitMinutes > minDurVisit)
vars.all.data = subset(vars.all.data, vars.all.data$DurVisitMinutes < maxDurVisit)
vars.all.data = na.omit(vars.all.data)

# Fit model
intM = glmer(IntWithNQ_MorMore ~ SELAllAC + PTAudAllAC + PEnHelos + PEnProps + ImpNQ_VorMore + (1|Site), family=binomial(link="logit"), data = vars.all.data)

summary(intM) # copy in to final resultsfile

intM.curve = glm(IntWithNQ_MorMore ~ SELAllAC + PTAudAllAC + PEnHelos + PEnProps + ImpNQ_VorMore, family=binomial(link="logit"), data = vars.all.data)

plot(vars.all.data$SELAllAC, jitter(as.numeric(vars.all.data$IntWithNQ_MorMore)-1, factor = 0.1), 
     pch = 16, cex = 1.1, 
     xlim = c(40, 100),
     col = alpha(colz[2], 0.75),
     xlab = "SELAllAC",
     ylab = "Interference")

xresp = as.numeric(vars.all.data[,"IntWithNQ_MorMore"])-1
cor(xresp, fitted(intM))

curve(predict(intM.curve, data.frame(SELAllAC = x, 
                                     PEnHelos = mean(vars.all.data$PEnHelos),
                                     PEnProps = mean(vars.all.data$PEnProps),
                                     PTAudAllAC = mean(vars.all.data$PTAudAllAC),
                                     ImpNQ_VorMore = vars.all.data$ImpNQ_VorMore[1]), type = "resp"), add = TRUE,
      col = colz[2], lwd = 2)

# Add error bars

px <- predict(intM.curve, data.frame(SELAllAC = seq(40, 100, by = 0.1), 
                                     PEnHelos = mean(vars.all.data$PEnHelos),
                                     PEnProps = mean(vars.all.data$PEnProps),
                                     PTAudAllAC = mean(vars.all.data$PTAudAllAC),
                                     ImpNQ_VorMore = vars.all.data$ImpNQ_VorMore[1]),
              type = "resp",
              se.fit = TRUE)

lines(seq(40, 100, by = 0.1),
      px$fit+px$se.fit, 
      col=colz[2], lty = 2)

lines(seq(40, 100, by = 0.1),
      px$fit-px$se.fit, 
      col=colz[2], lty = 2)

#########################################
### IntWithNQ_VorMore
# Best model: SELAllAC + PTAudAllAC + PEnHelos + PEnProps + ImpCP_VorMore + SiteVisitBefore

dose.var = "SELAllAC"
vars.dos = c("SELAllAC", "PEnHelos", "PEnProps", "PTAudAllAC")  
vars.mit = c("ImpCP_VorMore", "SiteVisitBefore", "Site")

varnames.na = c("IntWithNQ_VorMore", vars.dos, vars.mit, "Dataset", AddData)
vars.all.data = Data[varnames.na]													### Grab from proper data set
vars.all.data = subset(vars.all.data, vars.all.data$DurVisitMinutes > minDurVisit)
vars.all.data = subset(vars.all.data, vars.all.data$DurVisitMinutes < maxDurVisit)
vars.all.data = na.omit(vars.all.data)

# Fit model
intV = glmer(IntWithNQ_VorMore ~ SELAllAC + PEnHelos + PEnProps + PTAudAllAC + ImpCP_VorMore + SiteVisitBefore + (1|Site), family=binomial(link="logit"), data = vars.all.data)

summary(intV) # copy in to final resultsfile

intV.curve = glm(IntWithNQ_VorMore ~ SELAllAC + PEnHelos + PEnProps + PTAudAllAC + ImpCP_VorMore + SiteVisitBefore, family=binomial(link="logit"), data = vars.all.data)

plot(vars.all.data$SELAllAC, jitter(as.numeric(vars.all.data$IntWithNQ_VorMore)-1, factor = 0.1), 
     pch = 16, cex = 1.1, 
     xlim = c(40, 100),
     col = alpha(colz[3], 0.75),
     xlab = "SELAllAC",
     ylab = "Interference")

xresp = as.numeric(vars.all.data[,"IntWithNQ_VorMore"])-1
cor(xresp, fitted(intV))

curve(predict(intV.curve, data.frame(SELAllAC = x, 
                                     PEnHelos = mean(vars.all.data$PEnHelos),
                                     PEnProps = mean(vars.all.data$PEnProps),
                                     PTAudAllAC = mean(vars.all.data$PTAudAllAC),
                                     SiteVisitBefore = vars.all.data$SiteVisitBefore[1],
                                     ImpCP_VorMore = vars.all.data$ImpCP_VorMore[1]), type = "resp"), add = TRUE,
      col = colz[3], lwd = 2)

# Add error bars

px <- predict(intV.curve, data.frame(SELAllAC = seq(40, 100, by = 0.1), 
                                     PEnHelos = mean(vars.all.data$PEnHelos),
                                     PEnProps = mean(vars.all.data$PEnProps),
                                     PTAudAllAC = mean(vars.all.data$PTAudAllAC),
                                     SiteVisitBefore = vars.all.data$SiteVisitBefore[1],
                                     ImpCP_VorMore = vars.all.data$ImpCP_VorMore[1]),
              type = "resp",
              se.fit = TRUE)

lines(seq(40, 100, by = 0.1),
      px$fit+px$se.fit, 
      col=colz[3], lty = 2)

lines(seq(40, 100, by = 0.1),
      px$fit-px$se.fit, 
      col=colz[3], lty = 2)




dev.off()
