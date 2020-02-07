# Dose-response work 2016
# Fitting models of response to aviation noise for overnight, backcountry visitors to 4 national parks
# Dan Flynn | daniel.flynn.ctr@dot.gov

# Using both dayhike and vernight together in single model, with additional random effect

setwd("X:/Overnight") # Map server to local computer

library(lme4)   # for glmer() generalized linear mixed effects models
library(sjPlot) # for summary tables using sjt.glmer and summary plots using sjp.glmer
library(scales) # for alpha() and muted()

Data <- read.csv("ATMP2011_CompleteDoseVars_dprime.csv")

Data$IntWithNQ_SorMore <- as.numeric(Data$IntWithNQ_SorMore)-1
Data$IntWithNQ_MorMore <- as.numeric(Data$IntWithNQ_MorMore)-1
Data$IntWithNQ_VorMore <- as.numeric(Data$IntWithNQ_VorMore)-1

Data$Annoy_SorMore <- as.numeric(Data$Annoy_SorMore)-1
Data$Annoy_MorMore <- as.numeric(Data$Annoy_MorMore)-1
Data$Annoy_VorMore <- as.numeric(Data$Annoy_VorMore)-1

# use only data for which there is both day and overnight
keepsites <- tapply(Data$SiteType, Data$Site, function(x) length(x[x=="BCOvernight"])>0)
Data <- Data[Data$Site %in% names(keepsites[keepsites==TRUE]),]

Data$Site <- as.factor(as.character(Data$Site)) # clear empty levels

########### Modeling 

### Annoy_SorMore
# Best model: SELAllAC + PTAudAllAC + PEnHelos + PEnProps + Survey + ImpCP_VorMore + SiteVisitBefore + AdultsOnly + WatchBirds
# Random effects: Site, SiteType

dose.var = "SELAllAC"
vars.dos = c("SELAllAC", "PTAudAllAC", "PEnHelos", "PEnProps")  
vars.mit = c("Survey", "ImpCP_VorMore", "SiteVisitBefore", "AdultsOnly", "WatchBirds")

varnames.na = c("Annoy_SorMore", vars.dos, vars.mit, "Dataset", "Site", "SiteType")
vars.all.data = Data[varnames.na]
vars.all.data = na.omit(vars.all.data)
vars.all.data2 = vars.all.data
vars.all.data2[2:5] <- as.numeric(scale(vars.all.data2[2:5]))


# Fit model
annS = glmer(Annoy_SorMore ~  SELAllAC + PTAudAllAC + PEnHelos + PEnProps + 
                              Survey + ImpCP_VorMore + SiteVisitBefore +
                              WatchBirds + AdultsOnly +
                              (1|Site) + (1|SiteType), 
                              family = binomial(link="logit"),
                              data = vars.all.data)

# Fixed effect only model for plotting
annS.curve = glm(Annoy_SorMore ~ SELAllAC + PTAudAllAC + PEnHelos + PEnProps + 
                   Survey + ImpCP_VorMore + SiteVisitBefore +
                   WatchBirds + AdultsOnly + SiteType, 
                 family = binomial(link="logit"),
                 data = vars.all.data)

annS.ci <- confint(annS, method = "boot")

summary(annS) # copy in to final resultsfile

sjt.glmer(annS,
          string.est = "Estimate")

sjp.glmer(annS, type = "re")
sjp.glmer(annS, type = "fe",
          title = "Annoy, Somewhat or More",
          axis.title = "Estimate")

# Annoy M
varnames.na = c("Annoy_MorMore", vars.dos, vars.mit, "Dataset", "Site", "SiteType")
vars.all.data = Data[varnames.na]
vars.all.data = na.omit(vars.all.data)
vars.all.data2 = vars.all.data
vars.all.data2[2:5] <- as.numeric(scale(vars.all.data2[2:5]))

annM = glmer(Annoy_MorMore ~  SELAllAC + PTAudAllAC + PEnHelos + PEnProps + 
               Survey + ImpCP_VorMore + SiteVisitBefore +
               WatchBirds + AdultsOnly +
               (1|Site) + (1|SiteType), 
             family = binomial(link="logit"),
             data = vars.all.data2)

annM.curve = glm(Annoy_MorMore ~ SELAllAC + PTAudAllAC + PEnHelos + PEnProps + 
                   Survey + ImpCP_VorMore + SiteVisitBefore +
                   WatchBirds + AdultsOnly + SiteType, 
                 family = binomial(link="logit"),
                 data = vars.all.data)

summary(annM) # copy in to final resultsfile


sjp.lmer(annM, type = "re")
sjp.lmer(annM, type = "fe")

sjt.glmer(annM,
          string.est = "Estimate")

sjp.glmer(annM, type = "fe",
          title = "Annoy, Moderately or More",
          axis.title = "Estimate")

# Annoy v
varnames.na = c("Annoy_VorMore", vars.dos, vars.mit, "Dataset", "Site", "SiteType")
vars.all.data = Data[varnames.na]
vars.all.data = na.omit(vars.all.data)
vars.all.data2 = vars.all.data
vars.all.data2[2:5] <- as.numeric(scale(vars.all.data2[2:5]))

annV = glmer(Annoy_VorMore ~  SELAllAC + PTAudAllAC + PEnHelos + PEnProps + 
               Survey + ImpCP_VorMore + SiteVisitBefore +
               WatchBirds + AdultsOnly +
               (1|Site) + (1|SiteType), 
             family = binomial(link="logit"),
             data = vars.all.data2)

annV.curve = glm(Annoy_VorMore ~ SELAllAC + PTAudAllAC + PEnHelos + PEnProps + 
                   Survey + ImpCP_VorMore + SiteVisitBefore +
                   WatchBirds + AdultsOnly + SiteType, 
                 family = binomial(link="logit"),
                 data = vars.all.data)

summary(annV) # copy in to final resultsfile

sjp.lmer(annV, type = "re")

sjt.glmer(annV,
          string.est = "Estimate")

sjp.glmer(annV, type = "fe",
          title = "Annoy, Very much or More",
          axis.title = "Estimate")


########### Interfere

dose.var = "SELAllAC"
vars.dos = c("SELAllAC", "PTAudAllAC", "PEnHelos", "PEnProps")  
vars.mit = c("ImpCP_VorMore", "AdultsOnly")

varnames.na = c("IntWithNQ_SorMore", vars.dos, vars.mit, "Dataset", "Site", "SiteType")
vars.all.data = Data[varnames.na]
vars.all.data = na.omit(vars.all.data)
vars.all.data2 = vars.all.data
vars.all.data2[2:5] <- as.numeric(scale(vars.all.data2[2:5]))

intS = glmer(IntWithNQ_SorMore ~  SELAllAC + PTAudAllAC + PEnHelos + PEnProps + 
               ImpCP_VorMore + AdultsOnly + 
               (1|Site) + (1|SiteType), 
             family = binomial(link="logit"),
             data = vars.all.data2)


intS.curve = glm(IntWithNQ_SorMore ~  SELAllAC + PTAudAllAC + PEnHelos + PEnProps + 
                   ImpCP_VorMore + AdultsOnly + SiteType,
                 family = binomial(link="logit"),
                 data = vars.all.data)

summary(intS) # copy in to final resultsfile

sjp.lmer(intS, type = "re")


sjt.glmer(intS,
          string.est = "Estimate")

sjp.glmer(intS, type = "fe",
          title = "Interfere, Somewhat or More",
          axis.title = "Estimate")

# Interfere M

varnames.na = c("IntWithNQ_MorMore", vars.dos, vars.mit, "Dataset", "Site", "SiteType")
vars.all.data = Data[varnames.na]
vars.all.data = na.omit(vars.all.data)
vars.all.data2 = vars.all.data
vars.all.data2[2:5] <- as.numeric(scale(vars.all.data2[2:5]))

intM = glmer(IntWithNQ_MorMore ~  SELAllAC + PTAudAllAC + PEnHelos + PEnProps + 
               ImpCP_VorMore + AdultsOnly + 
               (1|Site) + (1|SiteType), 
             family = binomial(link="logit"),
             data = vars.all.data2)

intM.curve = glm(IntWithNQ_MorMore ~  SELAllAC + PTAudAllAC + PEnHelos + PEnProps + 
                   ImpCP_VorMore + AdultsOnly + SiteType,
                 family = binomial(link="logit"),
                 data = vars.all.data)

summary(intM) # copy in to final resultsfile

sjp.lmer(intM, type = "re")
sjt.glmer(intM,
          string.est = "Estimate")

sjp.glmer(intM, type = "fe",
          title = "Interfere, Moderately or More",
          axis.title = "Estimate")
# Interfere V

varnames.na = c("IntWithNQ_VorMore", vars.dos, vars.mit, "Dataset", "Site", "SiteType")
vars.all.data = Data[varnames.na]
vars.all.data = na.omit(vars.all.data)
vars.all.data2 = vars.all.data
vars.all.data2[2:5] <- as.numeric(scale(vars.all.data2[2:5]))

intV = glmer(IntWithNQ_VorMore ~  SELAllAC + PTAudAllAC + PEnHelos + PEnProps + 
               ImpCP_VorMore + AdultsOnly + 
               (1|Site) + (1|SiteType), 
             family = binomial(link="logit"),
             data = vars.all.data2)

intV.curve = glm(IntWithNQ_VorMore ~  SELAllAC + PTAudAllAC + PEnHelos + PEnProps + 
                   ImpCP_VorMore + AdultsOnly + SiteType,
                 family = binomial(link="logit"),
                 data = vars.all.data)

summary(intV) # copy in to final resultsfile

sjp.lmer(intV, type = "re")
sjt.glmer(intV,
          string.est = "Estimate")

sjp.glmer(intV, type = "fe",
          title = "Interfere, Very much or More",
          axis.title = "Estimate")

##############################################################################################################
########### Plot
##############################################################################################################
# colz = c("deepskyblue",
#          "dodgerblue",
#          "violet")
# 
# 
# colz2 = c("deepskyblue4",
#           "dodgerblue4",
#           "violetred4")

colz2 = c("darkolivegreen",
         "darkolivegreen3",
         "honeydew3")


colz = muted(colz2, l = 30, c = 70)



pdf("Best Models Overnight 2016-11-28.pdf", width = 5, height = 15)

par(mfrow=c(3,1))
dataxlim = c(25, 110)#range(Data$SELAllAC)


# Annoy and Interfere, overnight hike
# first plot S or more, then M or more and V or more together

plot(Data$SELAllAC, jitter(Data$Annoy_SorMore, factor = 0.3), 
     pch = 16, cex = 1.8, 
     xlim = c(dataxlim[1], dataxlim[2]),
     col = alpha(colz[1], 0.1),
     xlab = "LAE (dBA)",
     ylab = "Prob(Annoyed)")


points(Data$SELAllAC[Data$SiteType=="BCOvernight"], 
       jitter(Data$Annoy_SorMore[Data$SiteType=="BCOvernight"], factor = 0.3), 
     pch = 16, cex = 1.8, 
     col = alpha(colz2[1], 0.4))

title(main = "Overnight Hikes: Likelihood of Annoy")

# Add error bars

px <- predict(annS.curve, data.frame(SELAllAC = seq(dataxlim[1], dataxlim[2], by = 0.1), 
                                     PEnHelos = mean(Data$PEnHelos),
                                     PEnProps = mean(Data$PEnProps),
                                     PTAudAllAC = mean(Data$PTAudAllAC),
                                     Survey= Data$Survey[1],
                                     ImpCP_VorMore = Data$ImpCP_VorMore[1],
                                     SiteVisitBefore = Data$SiteVisitBefore[1],
                                     AdultsOnly = Data$AdultsOnly[1],
                                     WatchBirds = Data$WatchBirds[1],
                                     SiteType = "DayHike"),
        type = "resp",
        se.fit = TRUE)

lines(seq(dataxlim[1], dataxlim[2], by = 0.1),
      px$fit, 
      col=colz[1], lty = 1)

lines(seq(dataxlim[1], dataxlim[2], by = 0.1),
      px$fit+px$se.fit, 
      col=colz[1], lty = 2)

lines(seq(dataxlim[1], dataxlim[2], by = 0.1),
      px$fit-px$se.fit, 
      col=colz[1], lty = 2)

# now again, coloring in region of overnight

overnightxlim = quantile(Data$SELAllAC[Data$SiteType=="BCOvernight"], probs = c(0.05, 0.95))

px <- predict(annS.curve, data.frame(SELAllAC = seq(overnightxlim[1], overnightxlim[2], by = 0.1), 
                                     PEnHelos = mean(Data$PEnHelos),
                                     PEnProps = mean(Data$PEnProps),
                                     PTAudAllAC = mean(Data$PTAudAllAC),
                                     Survey= Data$Survey[1],
                                     ImpCP_VorMore = Data$ImpCP_VorMore[1],
                                     SiteVisitBefore = Data$SiteVisitBefore[1],
                                     AdultsOnly = Data$AdultsOnly[1],
                                     WatchBirds = Data$WatchBirds[1],
                                     SiteType = "BCOvernight"),
              type = "resp",
              se.fit = TRUE)

lines(seq(overnightxlim[1], overnightxlim[2], by = 0.1),
      px$fit, 
      col=colz2[1], lty = 1, lwd = 2)

lines(seq(overnightxlim[1], overnightxlim[2], by = 0.1),
      px$fit+px$se.fit, 
      col=colz2[1], lty = 2, lwd = 2)

lines(seq(overnightxlim[1], overnightxlim[2], by = 0.1),
      px$fit-px$se.fit, 
      col=colz2[1], lty = 2, lwd = 2)

legend("topleft",
       lwd = 2, 
       col = c(colz[1], colz2[1]),
       inset = 0.06,
       bty = "n",
       legend = c("Dayhike","Overnight"),
       title = "Slightly or more",
       cex = 1.5
       )

#########################################
### Annoy_MorMore
# Best model: SELAllAC + PEnHelos + PEnProps + Survey + ImpCP_VorMore
plot(Data$SELAllAC, jitter(Data$Annoy_MorMore, factor = 0.3), 
     pch = 16, cex = 1.8, 
     xlim = c(dataxlim[1], dataxlim[2]),
     col = alpha(colz[2], 0.1),
     xlab = "LAE (dBA)",
     ylab = "Prob(Annoyed)")

points(Data$SELAllAC[Data$SiteType=="BCOvernight"], 
       jitter(Data$Annoy_MorMore[Data$SiteType=="BCOvernight"], factor = 0.3), 
       pch = 16, cex = 1.8, 
       col = alpha(colz2[2], 0.4))

px <- predict(annM.curve, data.frame(SELAllAC = seq(dataxlim[1], dataxlim[2], by = 0.1), 
                                     PEnHelos = mean(Data$PEnHelos),
                                     PEnProps = mean(Data$PEnProps),
                                     PTAudAllAC = mean(Data$PTAudAllAC),
                                     Survey= Data$Survey[1],
                                     ImpCP_VorMore = Data$ImpCP_VorMore[1],
                                     SiteVisitBefore = Data$SiteVisitBefore[1],
                                     AdultsOnly = Data$AdultsOnly[1],
                                     WatchBirds = Data$WatchBirds[1],
                                     SiteType = "DayHike"),
              type = "resp",
              se.fit = TRUE)

lines(seq(dataxlim[1], dataxlim[2], by = 0.1),
      px$fit, 
      col=colz[2], lty = 1)

lines(seq(dataxlim[1], dataxlim[2], by = 0.1),
      px$fit+px$se.fit, 
      col=colz[2], lty = 2)

lines(seq(dataxlim[1], dataxlim[2], by = 0.1),
      px$fit-px$se.fit, 
      col=colz[2], lty = 2)

# now again, coloring in region of overnight
px <- predict(annM.curve, data.frame(SELAllAC = seq(overnightxlim[1], overnightxlim[2], by = 0.1), 
                                     PEnHelos = mean(Data$PEnHelos),
                                     PEnProps = mean(Data$PEnProps),
                                     PTAudAllAC = mean(Data$PTAudAllAC),
                                     Survey= Data$Survey[1],
                                     ImpCP_VorMore = Data$ImpCP_VorMore[1],
                                     SiteVisitBefore = Data$SiteVisitBefore[1],
                                     AdultsOnly = Data$AdultsOnly[1],
                                     WatchBirds = Data$WatchBirds[1],
                                     SiteType = "BCOvernight"),
              type = "resp",
              se.fit = TRUE)

lines(seq(overnightxlim[1], overnightxlim[2], by = 0.1),
      px$fit, 
      col=colz2[2], lty = 1, lwd = 2)

lines(seq(overnightxlim[1], overnightxlim[2], by = 0.1),
      px$fit+px$se.fit, 
      col=colz2[2], lty = 2, lwd = 2)

lines(seq(overnightxlim[1], overnightxlim[2], by = 0.1),
      px$fit-px$se.fit, 
      col=colz2[2], lty = 2, lwd = 2)


legend("topleft",
       lwd = 2, 
       col = c(colz[2], colz2[2]),
       inset = 0.06,
       bty = "n",
       legend = c("Dayhike","Overnight"),
       title = "Moderately or more",
       cex = 1.5
)

#########################################
### Annoy_VorMore
plot(Data$SELAllAC, jitter(Data$Annoy_VorMore, factor = 0.3), 
     pch = 16, cex = 1.8, 
     xlim = c(dataxlim[1], dataxlim[2]),
     col = alpha(colz[3], 0.1),
     xlab = "LAE (dBA)",
     ylab = "Prob(Annoyed)")

points(Data$SELAllAC[Data$SiteType=="BCOvernight"], 
       jitter(Data$Annoy_VorMore[Data$SiteType=="BCOvernight"], factor = 0.3), 
       pch = 16, cex = 1.8, 
       col = alpha(colz2[3], 0.4))

px <- predict(annV.curve, data.frame(SELAllAC = seq(dataxlim[1], dataxlim[2], by = 0.1), 
                                     PEnHelos = mean(Data$PEnHelos),
                                     PEnProps = mean(Data$PEnProps),
                                     PTAudAllAC = mean(Data$PTAudAllAC),
                                     Survey= Data$Survey[1],
                                     ImpCP_VorMore = Data$ImpCP_VorMore[1],
                                     SiteVisitBefore = Data$SiteVisitBefore[1],
                                     AdultsOnly = Data$AdultsOnly[1],
                                     WatchBirds = Data$WatchBirds[1],
                                     SiteType = "DayHike"),
              type = "resp",
              se.fit = TRUE)

lines(seq(dataxlim[1], dataxlim[2], by = 0.1),
      px$fit, 
      col=colz[3], lty = 1)

lines(seq(dataxlim[1], dataxlim[2], by = 0.1),
      px$fit+px$se.fit, 
      col=colz[3], lty = 2)

lines(seq(dataxlim[1], dataxlim[2], by = 0.1),
      px$fit-px$se.fit, 
      col=colz[3], lty = 2)

# now again, coloring in region of overnight
px <- predict(annV.curve, data.frame(SELAllAC = seq(overnightxlim[1], overnightxlim[2], by = 0.1), 
                                     PEnHelos = mean(Data$PEnHelos),
                                     PEnProps = mean(Data$PEnProps),
                                     PTAudAllAC = mean(Data$PTAudAllAC),
                                     Survey= Data$Survey[1],
                                     ImpCP_VorMore = Data$ImpCP_VorMore[1],
                                     SiteVisitBefore = Data$SiteVisitBefore[1],
                                     AdultsOnly = Data$AdultsOnly[1],
                                     WatchBirds = Data$WatchBirds[1],
                                     SiteType = "BCOvernight"),
              type = "resp",
              se.fit = TRUE)

lines(seq(overnightxlim[1], overnightxlim[2], by = 0.1),
      px$fit, 
      col=colz2[3], lty = 1, lwd = 2)

lines(seq(overnightxlim[1], overnightxlim[2], by = 0.1),
      px$fit+px$se.fit, 
      col=colz2[3], lty = 2, lwd = 2)

lines(seq(overnightxlim[1], overnightxlim[2], by = 0.1),
      px$fit-px$se.fit, 
      col=colz2[3], lty = 2, lwd = 2)


legend("topleft",
       lwd = 2, 
       col = c(colz[3], colz2[3]),
       inset = 0.06,
       bty = "n",
       legend = c("Dayhike","Overnight"),
       title = "Very much or more",
       cex = 1.5
)

############################################################################################################################################################################################################
# Interfere

### IntWithNQ_SorMore

par(mfrow=c(3,1))

# Annoy and Interfere, overnight hike
# first plot S or more, then M or more and V or more together

plot(Data$SELAllAC, jitter(Data$IntWithNQ_SorMore, factor = 0.3), 
     pch = 16, cex = 1.8, 
     xlim = c(dataxlim[1], dataxlim[2]),
     col = alpha(colz[1], 0.1),
     xlab = "LAE (dBA)",
     ylab = "Prob(Interference with Natural Quiet)")

points(Data$SELAllAC[Data$SiteType=="BCOvernight"], 
       jitter(Data$IntWithNQ_SorMore[Data$SiteType=="BCOvernight"], factor = 0.3), 
       pch = 16, cex = 1.8, 
       col = alpha(colz2[1], 0.4))

title(main = "Overnight Hikes: Likelihood of Interference with Natural Quiet")

# Add error bars

px <- predict(intS.curve, data.frame(SELAllAC = seq(dataxlim[1], dataxlim[2], by = 0.1), 
                                     PEnHelos = mean(Data$PEnHelos),
                                     PEnProps = mean(Data$PEnProps),
                                     PTAudAllAC = mean(Data$PTAudAllAC),
                                     Survey= Data$Survey[1],
                                     ImpCP_VorMore = Data$ImpCP_VorMore[1],
                                     SiteVisitBefore = Data$SiteVisitBefore[1],
                                     AdultsOnly = Data$AdultsOnly[1],
                                     WatchBirds = Data$WatchBirds[1],
                                     SiteType = "DayHike"),
              type = "resp",
              se.fit = TRUE)

lines(seq(dataxlim[1], dataxlim[2], by = 0.1),
      px$fit, 
      col=colz[1], lty = 1)

lines(seq(dataxlim[1], dataxlim[2], by = 0.1),
      px$fit+px$se.fit, 
      col=colz[1], lty = 2)

lines(seq(dataxlim[1], dataxlim[2], by = 0.1),
      px$fit-px$se.fit, 
      col=colz[1], lty = 2)

# now again, coloring in region of overnight
px <- predict(intS.curve, data.frame(SELAllAC = seq(overnightxlim[1], overnightxlim[2], by = 0.1), 
                                     PEnHelos = mean(Data$PEnHelos),
                                     PEnProps = mean(Data$PEnProps),
                                     PTAudAllAC = mean(Data$PTAudAllAC),
                                     Survey= Data$Survey[1],
                                     ImpCP_VorMore = Data$ImpCP_VorMore[1],
                                     SiteVisitBefore = Data$SiteVisitBefore[1],
                                     AdultsOnly = Data$AdultsOnly[1],
                                     WatchBirds = Data$WatchBirds[1],
                                     SiteType = "BCOvernight"),
              type = "resp",
              se.fit = TRUE)

lines(seq(overnightxlim[1], overnightxlim[2], by = 0.1),
      px$fit, 
      col=colz2[1], lty = 1, lwd = 2)

lines(seq(overnightxlim[1], overnightxlim[2], by = 0.1),
      px$fit+px$se.fit, 
      col=colz2[1], lty = 2, lwd = 2)

lines(seq(overnightxlim[1], overnightxlim[2], by = 0.1),
      px$fit-px$se.fit, 
      col=colz2[1], lty = 2, lwd = 2)

legend("topleft",
       lwd = 2, 
       col = c(colz[1], colz2[1]),
       inset = 0.06,
       bty = "n",
       legend = c("Dayhike","Overnight"),
       title = "Slightly or more",
       cex = 1.5
)

#########################################
### IntWithNQ_MorMore
# Best model: SELAllAC + PEnHelos + PEnProps + Survey + ImpCP_VorMore
plot(Data$SELAllAC, jitter(Data$IntWithNQ_MorMore, factor = 0.3), 
     pch = 16, cex = 1.8, 
     xlim = c(dataxlim[1], dataxlim[2]),
     col = alpha(colz[2], 0.1),
     xlab = "LAE (dBA)",
     ylab = "Prob(Interference with Natural Quiet)")

points(Data$SELAllAC[Data$SiteType=="BCOvernight"], 
       jitter(Data$IntWithNQ_MorMore[Data$SiteType=="BCOvernight"], factor = 0.3), 
       pch = 16, cex = 1.8, 
       col = alpha(colz2[2], 0.4))

px <- predict(intM.curve, data.frame(SELAllAC = seq(dataxlim[1], dataxlim[2], by = 0.1), 
                                     PEnHelos = mean(Data$PEnHelos),
                                     PEnProps = mean(Data$PEnProps),
                                     PTAudAllAC = mean(Data$PTAudAllAC),
                                     Survey= Data$Survey[1],
                                     ImpCP_VorMore = Data$ImpCP_VorMore[1],
                                     SiteVisitBefore = Data$SiteVisitBefore[1],
                                     AdultsOnly = Data$AdultsOnly[1],
                                     WatchBirds = Data$WatchBirds[1],
                                     SiteType = "DayHike"),
              type = "resp",
              se.fit = TRUE)

lines(seq(dataxlim[1], dataxlim[2], by = 0.1),
      px$fit, 
      col=colz[2], lty = 1)

lines(seq(dataxlim[1], dataxlim[2], by = 0.1),
      px$fit+px$se.fit, 
      col=colz[2], lty = 2)

lines(seq(dataxlim[1], dataxlim[2], by = 0.1),
      px$fit-px$se.fit, 
      col=colz[2], lty = 2)

# now again, coloring in region of overnight
px <- predict(intM.curve, data.frame(SELAllAC = seq(overnightxlim[1], overnightxlim[2], by = 0.1), 
                                     PEnHelos = mean(Data$PEnHelos),
                                     PEnProps = mean(Data$PEnProps),
                                     PTAudAllAC = mean(Data$PTAudAllAC),
                                     Survey= Data$Survey[1],
                                     ImpCP_VorMore = Data$ImpCP_VorMore[1],
                                     SiteVisitBefore = Data$SiteVisitBefore[1],
                                     AdultsOnly = Data$AdultsOnly[1],
                                     WatchBirds = Data$WatchBirds[1],
                                     SiteType = "BCOvernight"),
              type = "resp",
              se.fit = TRUE)

lines(seq(overnightxlim[1], overnightxlim[2], by = 0.1),
      px$fit, 
      col=colz2[2], lty = 1, lwd = 2)

lines(seq(overnightxlim[1], overnightxlim[2], by = 0.1),
      px$fit+px$se.fit, 
      col=colz2[2], lty = 2, lwd = 2)

lines(seq(overnightxlim[1], overnightxlim[2], by = 0.1),
      px$fit-px$se.fit, 
      col=colz2[2], lty = 2, lwd = 2)

legend("topleft",
       lwd = 2, 
       col = c(colz[1], colz2[1]),
       inset = 0.06,
       bty = "n",
       legend = c("Dayhike","Overnight"),
       title = "Moderately or more",
       cex = 1.5)

#########################################
### IntWithNQ_VorMore
plot(Data$SELAllAC, jitter(Data$IntWithNQ_VorMore, factor = 0.3), 
     pch = 16, cex = 1.8, 
     xlim = c(dataxlim[1], dataxlim[2]),
     col = alpha(colz[3], 0.1),
     xlab = "LAE (dBA)",
     ylab = "Prob(Interference with Natural Quiet)")

points(Data$SELAllAC[Data$SiteType=="BCOvernight"], 
       jitter(Data$IntWithNQ_VorMore[Data$SiteType=="BCOvernight"], factor = 0.3), 
       pch = 16, cex = 1.8, 
       col = alpha(colz2[3], 0.4))

px <- predict(intV.curve, data.frame(SELAllAC = seq(dataxlim[1], dataxlim[2], by = 0.1), 
                                     PEnHelos = mean(Data$PEnHelos),
                                     PEnProps = mean(Data$PEnProps),
                                     PTAudAllAC = mean(Data$PTAudAllAC),
                                     Survey= Data$Survey[1],
                                     ImpCP_VorMore = Data$ImpCP_VorMore[1],
                                     SiteVisitBefore = Data$SiteVisitBefore[1],
                                     AdultsOnly = Data$AdultsOnly[1],
                                     WatchBirds = Data$WatchBirds[1],
                                     SiteType = "DayHike"),
              type = "resp",
              se.fit = TRUE)

lines(seq(dataxlim[1], dataxlim[2], by = 0.1),
      px$fit, 
      col=colz[3], lty = 1)

lines(seq(dataxlim[1], dataxlim[2], by = 0.1),
      px$fit+px$se.fit, 
      col=colz[3], lty = 2)

lines(seq(dataxlim[1], dataxlim[2], by = 0.1),
      px$fit-px$se.fit, 
      col=colz[3], lty = 2)

# now again, coloring in region of overnight
px <- predict(intV.curve, data.frame(SELAllAC = seq(overnightxlim[1], overnightxlim[2], by = 0.1), 
                                     PEnHelos = mean(Data$PEnHelos),
                                     PEnProps = mean(Data$PEnProps),
                                     PTAudAllAC = mean(Data$PTAudAllAC),
                                     Survey= Data$Survey[1],
                                     ImpCP_VorMore = Data$ImpCP_VorMore[1],
                                     SiteVisitBefore = Data$SiteVisitBefore[1],
                                     AdultsOnly = Data$AdultsOnly[1],
                                     WatchBirds = Data$WatchBirds[1],
                                     SiteType = "BCOvernight"),
              type = "resp",
              se.fit = TRUE)

lines(seq(overnightxlim[1], overnightxlim[2], by = 0.1),
      px$fit, 
      col=colz2[3], lty = 1, lwd = 2)

lines(seq(overnightxlim[1], overnightxlim[2], by = 0.1),
      px$fit+px$se.fit, 
      col=colz2[3], lty = 2, lwd = 2)

lines(seq(overnightxlim[1], overnightxlim[2], by = 0.1),
      px$fit-px$se.fit, 
      col=colz2[3], lty = 2, lwd = 2)

legend("topleft",
       lwd = 2, 
       col = c(colz[3], colz2[3]),
       inset = 0.06,
       bty = "n",
       legend = c("Dayhike","Overnight"),
       title = "Very much or more",
       cex = 1.5)

dev.off()
