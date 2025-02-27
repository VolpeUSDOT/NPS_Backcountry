# Dose-response work 2016
# Fitting models of response to aviation noise for overnight, backcountry visitors to 4 national parks
# Dan Flynn | daniel.flynn@dot.gov

# Using both dayhike and vernight together in single model, with additional random effect

library(lme4)   # for glmer() generalized linear mixed effects models
library(sjPlot) # for summary tables using tab_model
library(scales) # for alpha() and muted()

Data <- read.csv(file.path("Data", "ATMP2011_CompleteDoseVars_dprime.csv"))

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


Data <- Data[Data$SiteType != "ShortHike",]

levels(Data$SiteType) = c("Overnight", "Dayhike", "sh")

Data$SiteType <- as.factor(as.character(Data$SiteType))

# additionally, omit the few remaining day hikes which have durations fewer than 60 min.

Data <- Data[Data$DurVisitMinutes > 60,]

# <<>><<>><<>><<>><<>><<>><<>><<>><<>>
# Re-run final models ----

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

with(vars.all.data, tapply(Annoy_SorMore, SiteType, function(x) sum(x)/length(x)))

# Fit model
annS = glmer(Annoy_SorMore ~  SELAllAC + PTAudAllAC + PEnHelos + PEnProps + 
                              Survey + ImpCP_VorMore + SiteVisitBefore +
                              WatchBirds + AdultsOnly +
                              (1|Site) + (1|SiteType), 
                              family = binomial(link="logit"),
                              data = vars.all.data2)

# Fixed effect only model for plotting
annS.curve = glm(Annoy_SorMore ~ SELAllAC + PTAudAllAC + PEnHelos + PEnProps + 
                   Survey + ImpCP_VorMore + SiteVisitBefore +
                   WatchBirds + AdultsOnly + SiteType + Site, 
                 family = binomial(link="logit"),
                 data = vars.all.data)

summary(annS) # copy in to final resultsfile

tab_model(annS,
          string.est = "Estimate",
          file = file.path("Output", paste0(varnames.na[1], ".html")))

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
                   WatchBirds + AdultsOnly + SiteType + Site, 
                 family = binomial(link="logit"),
                 data = vars.all.data)

summary(annM) # copy in to final resultsfile

tab_model(annM,
          string.est = "Estimate",
          file = file.path("Output", paste0(varnames.na[1], ".html")))

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
                   WatchBirds + AdultsOnly + SiteType + Site, 
                 family = binomial(link="logit"),
                 data = vars.all.data)

summary(annV) # copy in to final resultsfile

tab_model(annV,
          string.est = "Estimate",
          file = file.path("Output", paste0(varnames.na[1], ".html")))


########### Interfere

dose.var = "SELAllAC"
vars.dos = c("SELAllAC", "PTAudAllAC", "PEnHelos", "PEnProps")  
vars.mit = c("ImpCP_VorMore", "AdultsOnly")

varnames.na = c("IntWithNQ_SorMore", vars.dos, vars.mit, "Dataset", "Site", "SiteType")
vars.all.data = Data[varnames.na]
vars.all.data = na.omit(vars.all.data)
vars.all.data2 = vars.all.data
vars.all.data2[2:5] <- as.numeric(scale(vars.all.data2[2:5]))

with(vars.all.data, tapply(IntWithNQ_SorMore, SiteType, function(x) sum(x)/length(x)))

intS = glmer(IntWithNQ_SorMore ~  SELAllAC + PTAudAllAC + PEnHelos + PEnProps + 
               ImpCP_VorMore + AdultsOnly + 
               (1|Site) + (1|SiteType), 
             family = binomial(link="logit"),
             data = vars.all.data2)


intS.curve = glm(IntWithNQ_SorMore ~  SELAllAC + PTAudAllAC + PEnHelos + PEnProps + 
                   ImpCP_VorMore + AdultsOnly + SiteType + Site,
                 family = binomial(link="logit"),
                 data = vars.all.data)

summary(intS) # copy in to final resultsfile

tab_model(intS,
          string.est = "Estimate",
          file = file.path("Output", paste0(varnames.na[1], ".html")))

# Interfere M

varnames.na = c("IntWithNQ_MorMore", vars.dos, vars.mit, "Dataset", "Site", "SiteType")
vars.all.data = Data[varnames.na]
vars.all.data = na.omit(vars.all.data)
vars.all.data2 = vars.all.data
vars.all.data2[2:5] <- as.numeric(scale(vars.all.data2[2:5]))

with(vars.all.data, tapply(IntWithNQ_MorMore, SiteType, function(x) sum(x)/length(x)))

intM = glmer(IntWithNQ_MorMore ~  SELAllAC + PTAudAllAC + PEnHelos + PEnProps + 
               ImpCP_VorMore + AdultsOnly + 
               (1|Site) + (1|SiteType), 
             family = binomial(link="logit"),
             data = vars.all.data2)

intM.curve = glm(IntWithNQ_MorMore ~  SELAllAC + PTAudAllAC + PEnHelos + PEnProps + 
                   ImpCP_VorMore + AdultsOnly + SiteType + Site,
                 family = binomial(link="logit"),
                 data = vars.all.data)

summary(intM) # copy in to final resultsfile

tab_model(intM,
          string.est = "Estimate",
          file = file.path("Output", paste0(varnames.na[1], ".html")))

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
                   ImpCP_VorMore + AdultsOnly + SiteType + Site,
                 family = binomial(link="logit"),
                 data = vars.all.data)

summary(intV) # copy in to final resultsfile

tab_model(intV,
          string.est = "Estimate",
          file = file.path("Output", paste0(varnames.na[1], ".html")))

# <<>><<>><<>><<>><<>><<>><<>><<>><<>>
# Plot ----

colz2 = rep("dodgerblue4", 3)
colz = rep("grey40", 3)
ptcex = 0.8
ptoffset = 0.015
ptalpha = 0.08
ptalpha.bc = 0.16

pdf(file.path("Output", paste0("Best Models Overnight ", Sys.Date(),".pdf")), width = 11, height = 4.5)

par(mfrow=c(1, 3), xpd = F)
dataxlim = range(vars.all.data$SELAllAC) #c(25, 110)

# Annoyance plots ----
# first plot S or more, then M or more and V or more together

plot(Data$SELAllAC, jitter(Data$Annoy_SorMore, factor = 0.03)-ptoffset, 
     pch = 16, cex = ptcex, 
     xlim = c(dataxlim[1], dataxlim[2]),
     col = alpha(colz[1], ptalpha),
     xlab = "LAE (dBA)",
     ylab = "Prob(Annoyed)")


points(Data$SELAllAC[Data$SiteType=="Overnight"], 
       jitter(Data$Annoy_SorMore[Data$SiteType=="Overnight"], factor = 0.03)+ptoffset, 
     pch = 16, cex = ptcex, 
     col = alpha(colz2[1], ptalpha.bc))

# Do all combinations of categorical variables. Plot median, 5 and 95 quantiles profile curves
predgrid <- expand.grid(
            levels(Data$Survey),
            levels(Data$ImpCP_VorMore),
            levels(Data$SiteVisitBefore),
            levels(Data$AdultsOnly),
            levels(Data$WatchBirds),
            levels(Data$Site))
           
gridres <- gridres.se <- vector()

for(i in 1:nrow(predgrid)){
  px <- predict(annS.curve, data.frame(SELAllAC = seq(dataxlim[1], dataxlim[2], by = 0.1), 
                                       PEnHelos = mean(Data$PEnHelos),
                                       PEnProps = mean(Data$PEnProps),
                                       PTAudAllAC = mean(Data$PTAudAllAC),
                                       Survey= predgrid[i,1],
                                       ImpCP_VorMore = predgrid[i,2],
                                       SiteVisitBefore = predgrid[i,3],
                                       AdultsOnly = predgrid[i,4],
                                       WatchBirds = predgrid[i,5],
                                       Site = predgrid[i,6],
                                       SiteType = "Dayhike"),
                type = "resp",
                se.fit = TRUE)
  
  gridres = rbind(gridres, px$fit)
  gridres.se = rbind(gridres.se, px$se.fit)
  }
  
medfit <- apply(gridres, 2, function(x) quantile(x, 0.5))

sefit <- apply(gridres.se, 2, function(x) quantile(x, 0.5))

polygon(c(
  seq(dataxlim[1], dataxlim[2], by = 0.1),
  seq(dataxlim[2], dataxlim[1], by = -0.1)),
  c(medfit-sefit, 
    rev(medfit+sefit)), 
  col=alpha(colz[1], 0.2),
  border = NA)

lines(seq(dataxlim[1], dataxlim[2], by = 0.1),
      medfit, 
      col=colz[1], lty = 1)

# now again, coloring in region of overnight

overnightxlim = quantile(Data$SELAllAC[Data$SiteType=="Overnight"], probs = c(0.05, 0.95))

gridres <- gridres.se <- vector()

for(i in 1:nrow(predgrid)){
  px <- predict(annS.curve, data.frame(SELAllAC = seq(dataxlim[1], dataxlim[2], by = 0.1), 
                                       PEnHelos = mean(Data$PEnHelos),
                                       PEnProps = mean(Data$PEnProps),
                                       PTAudAllAC = mean(Data$PTAudAllAC),
                                       Survey= predgrid[i,1],
                                       ImpCP_VorMore = predgrid[i,2],
                                       SiteVisitBefore = predgrid[i,3],
                                       AdultsOnly = predgrid[i,4],
                                       WatchBirds = predgrid[i,5],
                                       Site = predgrid[i,6],
                                       SiteType = "Overnight"),
                type = "resp",
                se.fit = TRUE)
  
  gridres = rbind(gridres, px$fit)
  gridres.se = rbind(gridres.se, px$se.fit)
}

medfit <- apply(gridres, 2, function(x) quantile(x, 0.5))

sefit <- apply(gridres.se, 2, function(x) quantile(x, 0.5))

# First draw whole range, very light
polygon(c(
  seq(dataxlim[1], dataxlim[2], by = 0.1),
  seq(dataxlim[2], dataxlim[1], by = -0.1)),
  c(medfit-sefit, 
    rev(medfit+sefit)), 
  col=alpha(colz2[1], 0.1),
  border = NA)

lines(seq(dataxlim[1], dataxlim[2], length.out = length(medfit)),
      medfit, 
      col=alpha(colz2[1], 0.1), lty = 1, lwd = 2)

# Then draw overnight range, a bit darker

gridres <- gridres.se <- vector()

for(i in 1:nrow(predgrid)){
  px <- predict(annS.curve, data.frame(SELAllAC = seq(overnightxlim[1], overnightxlim[2], by = 0.1), 
                                       PEnHelos = mean(Data$PEnHelos),
                                       PEnProps = mean(Data$PEnProps),
                                       PTAudAllAC = mean(Data$PTAudAllAC),
                                       Survey= predgrid[i,1],
                                       ImpCP_VorMore = predgrid[i,2],
                                       SiteVisitBefore = predgrid[i,3],
                                       AdultsOnly = predgrid[i,4],
                                       WatchBirds = predgrid[i,5],
                                       Site = predgrid[i,6],
                                       SiteType = "Overnight"),
                type = "resp",
                se.fit = TRUE)
  
  gridres = rbind(gridres, px$fit)
  gridres.se = rbind(gridres.se, px$se.fit)
}

medfit <- apply(gridres, 2, function(x) quantile(x, 0.5))

sefit <- apply(gridres.se, 2, function(x) quantile(x, 0.5))

polygon(c(
  seq(overnightxlim[1], overnightxlim[2], by = 0.1),
  seq(overnightxlim[2], overnightxlim[1], by = -0.1)),
  c(medfit-sefit, 
    rev(medfit+sefit)), 
  col=alpha(colz2[1], 0.4),
  border = NA)

lines(seq(overnightxlim[1], overnightxlim[2], length.out = length(medfit)),
      medfit, 
      col=colz2[1], lty = 1, lwd = 2)

legend("topleft",
       lwd = c(1, 2), 
       col = c(colz[1], colz2[1]),
       inset = 0.08,
       bty = "n",
       legend = c("Dayhike","Multi-day"),
       title = "Slightly or more",
       cex = 1.5)

### Annoy_MorMore
# Best model: SELAllAC + PEnHelos + PEnProps + Survey + ImpCP_VorMore
plot(Data$SELAllAC, jitter(Data$Annoy_MorMore, factor = 0.03)-ptoffset, 
     pch = 16, cex = ptcex, 
     xlim = c(dataxlim[1], dataxlim[2]),
     col = alpha(colz[2], ptalpha),
     xlab = "LAE (dBA)",
     ylab = "Prob(Annoyed)")

title(main = "Likelihood of Annoyance")


points(Data$SELAllAC[Data$SiteType=="Overnight"], 
       jitter(Data$Annoy_MorMore[Data$SiteType=="Overnight"], factor = 0.03)+ptoffset, 
       pch = 16, cex = ptcex, 
       col = alpha(colz2[2], ptalpha.bc))

gridres <- gridres.se <- vector()

for(i in 1:nrow(predgrid)){
  px <- predict(annM.curve, data.frame(SELAllAC = seq(dataxlim[1], dataxlim[2], by = 0.1), 
                                       PEnHelos = mean(Data$PEnHelos),
                                       PEnProps = mean(Data$PEnProps),
                                       PTAudAllAC = mean(Data$PTAudAllAC),
                                       Survey= predgrid[i,1],
                                       ImpCP_VorMore = predgrid[i,2],
                                       SiteVisitBefore = predgrid[i,3],
                                       AdultsOnly = predgrid[i,4],
                                       WatchBirds = predgrid[i,5],
                                       Site = predgrid[i,6],
                                       SiteType = "Dayhike"),
                type = "resp",
                se.fit = TRUE)
  
  gridres = rbind(gridres, px$fit)
  gridres.se = rbind(gridres.se, px$se.fit)
}

medfit <- apply(gridres, 2, function(x) quantile(x, 0.5))

sefit <- apply(gridres.se, 2, function(x) quantile(x, 0.5))


polygon(c(
  seq(dataxlim[1], dataxlim[2], by = 0.1),
  seq(dataxlim[2], dataxlim[1], by = -0.1)),
  c(medfit-sefit, 
    rev(medfit+sefit)), 
  col=alpha(colz[2], 0.2),
  border = NA)

lines(seq(dataxlim[1], dataxlim[2], by = 0.1),
      medfit, 
      col=colz[2], lty = 1)

# now again, coloring in region of overnight
gridres <- gridres.se <- vector()

for(i in 1:nrow(predgrid)){
  px <- predict(annM.curve, data.frame(SELAllAC = seq(dataxlim[1], dataxlim[2], by = 0.1), 
                                       PEnHelos = mean(Data$PEnHelos),
                                       PEnProps = mean(Data$PEnProps),
                                       PTAudAllAC = mean(Data$PTAudAllAC),
                                       Survey= predgrid[i,1],
                                       ImpCP_VorMore = predgrid[i,2],
                                       SiteVisitBefore = predgrid[i,3],
                                       AdultsOnly = predgrid[i,4],
                                       WatchBirds = predgrid[i,5],
                                       Site = predgrid[i,6],
                                       SiteType = "Overnight"),
                type = "resp",
                se.fit = TRUE)
  
  gridres = rbind(gridres, px$fit)
  gridres.se = rbind(gridres.se, px$se.fit)
}

medfit <- apply(gridres, 2, function(x) quantile(x, 0.5))

sefit <- apply(gridres.se, 2, function(x) quantile(x, 0.5))

# First draw whole range, very light
polygon(c(
  seq(dataxlim[1], dataxlim[2], by = 0.1),
  seq(dataxlim[2], dataxlim[1], by = -0.1)),
  c(medfit-sefit, 
    rev(medfit+sefit)), 
  col=alpha(colz2[1], 0.1),
  border = NA)

lines(seq(dataxlim[1], dataxlim[2], length.out = length(medfit)),
      medfit, 
      col=alpha(colz2[1], 0.1), lty = 1, lwd = 2)

# Then draw overnight range, a bit darker

gridres <- gridres.se <- vector()

for(i in 1:nrow(predgrid)){
  px <- predict(annM.curve, data.frame(SELAllAC = seq(overnightxlim[1], overnightxlim[2], by = 0.1), 
                                       PEnHelos = mean(Data$PEnHelos),
                                       PEnProps = mean(Data$PEnProps),
                                       PTAudAllAC = mean(Data$PTAudAllAC),
                                       Survey= predgrid[i,1],
                                       ImpCP_VorMore = predgrid[i,2],
                                       SiteVisitBefore = predgrid[i,3],
                                       AdultsOnly = predgrid[i,4],
                                       WatchBirds = predgrid[i,5],
                                       Site = predgrid[i,6],
                                       SiteType = "Overnight"),
                type = "resp",
                se.fit = TRUE)
  
  gridres = rbind(gridres, px$fit)
  gridres.se = rbind(gridres.se, px$se.fit)
}

medfit <- apply(gridres, 2, function(x) quantile(x, 0.5))

sefit <- apply(gridres.se, 2, function(x) quantile(x, 0.5))

polygon(c(
  seq(overnightxlim[1], overnightxlim[2], by = 0.1),
  seq(overnightxlim[2], overnightxlim[1], by = -0.1)),
  c(medfit-sefit, 
    rev(medfit+sefit)), 
  col=alpha(colz2[1], 0.4),
  border = NA)

lines(seq(overnightxlim[1], overnightxlim[2], length.out = length(medfit)),
      medfit, 
      col=colz2[1], lty = 1, lwd = 2)

legend("topleft",
       inset = 0.08,
       bty = "n",
       legend = "",
       col = "white",
       title = "Moderately or more",
       cex = 1.5)

### Annoy_VorMore
plot(Data$SELAllAC, jitter(Data$Annoy_VorMore, factor = 0.03)-ptoffset, 
     pch = 16, cex = ptcex, 
     xlim = c(dataxlim[1], dataxlim[2]),
     col = alpha(colz[3], ptalpha),
     xlab = "LAE (dBA)",
     ylab = "Prob(Annoyed)")

points(Data$SELAllAC[Data$SiteType=="Overnight"], 
       jitter(Data$Annoy_VorMore[Data$SiteType=="Overnight"], factor = 0.03)+ptoffset, 
       pch = 16, cex = ptcex, 
       col = alpha(colz2[3], ptalpha.bc))

gridres <- gridres.se <- vector()

for(i in 1:nrow(predgrid)){
  px <- predict(annV.curve, data.frame(SELAllAC = seq(dataxlim[1], dataxlim[2], by = 0.1), 
                                       PEnHelos = mean(Data$PEnHelos),
                                       PEnProps = mean(Data$PEnProps),
                                       PTAudAllAC = mean(Data$PTAudAllAC),
                                       Survey= predgrid[i,1],
                                       ImpCP_VorMore = predgrid[i,2],
                                       SiteVisitBefore = predgrid[i,3],
                                       AdultsOnly = predgrid[i,4],
                                       WatchBirds = predgrid[i,5],
                                       Site = predgrid[i,6],
                                       SiteType = "Overnight"),
                type = "resp",
                se.fit = TRUE)
  
  gridres = rbind(gridres, px$fit)
  gridres.se = rbind(gridres.se, px$se.fit)
}

medfit <- apply(gridres, 2, function(x) quantile(x, 0.5))

sefit <- apply(gridres.se, 2, function(x) quantile(x, 0.5))

polygon(c(
  seq(dataxlim[1], dataxlim[2], by = 0.1),
  seq(dataxlim[2], dataxlim[1], by = -0.1)),
  c(medfit-sefit, 
    rev(medfit+sefit)), 
  col=alpha(colz[3], 0.1),
  border = NA)

lines(seq(dataxlim[1], dataxlim[2], by = 0.1),
      medfit, 
      col=alpha(colz[3], 0.1), lty = 1)

# now again, coloring in region of overnight
gridres <- gridres.se <- vector()

for(i in 1:nrow(predgrid)){
  px <- predict(annV.curve, data.frame(SELAllAC = seq(overnightxlim[1], overnightxlim[2], by = 0.1), 
                                       PEnHelos = mean(Data$PEnHelos),
                                       PEnProps = mean(Data$PEnProps),
                                       PTAudAllAC = mean(Data$PTAudAllAC),
                                       Survey= predgrid[i,1],
                                       ImpCP_VorMore = predgrid[i,2],
                                       SiteVisitBefore = predgrid[i,3],
                                       AdultsOnly = predgrid[i,4],
                                       WatchBirds = predgrid[i,5],
                                       Site = predgrid[i,6],
                                       SiteType = "Overnight"),
                type = "resp",
                se.fit = TRUE)
  
  gridres = rbind(gridres, px$fit)
  gridres.se = rbind(gridres.se, px$se.fit)
}

medfit <- apply(gridres, 2, function(x) quantile(x, 0.5))

sefit <- apply(gridres.se, 2, function(x) quantile(x, 0.5))

polygon(c(
  seq(overnightxlim[1], overnightxlim[2], by = 0.1),
  seq(overnightxlim[2], overnightxlim[1], by = -0.1)),
  c(medfit-sefit, 
    rev(medfit+sefit)), 
  col=alpha(colz2[3], 0.4),
  border = NA)

lines(seq(overnightxlim[1], overnightxlim[2], length.out = length(medfit)),
      medfit, 
      col=colz2[3], lty = 1, lwd = 2)

legend("topleft",
       lwd = 2, 
       inset = 0.08,
       bty = "n",
       legend = "",
       col = "white",
       title = "Very or more",
       cex = 1.5)

# Interfere Plots ----
### IntWithNQ_SorMore

plot(Data$SELAllAC, jitter(Data$IntWithNQ_SorMore, factor = 0.03)-ptoffset, 
     pch = 16, cex = ptcex, 
     xlim = c(dataxlim[1], dataxlim[2]),
     col = alpha(colz[1], ptalpha),
     xlab = "LAE (dBA)",
     ylab = "Prob(Interference with Natural Quiet)")

points(Data$SELAllAC[Data$SiteType=="Overnight"], 
       jitter(Data$IntWithNQ_SorMore[Data$SiteType=="Overnight"], factor = 0.03)+ptoffset, 
       pch = 16, cex = ptcex, 
       col = alpha(colz2[1], ptalpha.bc))


predgrid <- expand.grid(
  levels(Data$ImpCP_VorMore),
  levels(Data$AdultsOnly),
  levels(Data$Site))

gridres <- gridres.se <- vector()

for(i in 1:nrow(predgrid)){
  px <- predict(intS.curve, data.frame(SELAllAC = seq(dataxlim[1], dataxlim[2], by = 0.1), 
                                       PEnHelos = mean(Data$PEnHelos),
                                       PEnProps = mean(Data$PEnProps),
                                       PTAudAllAC = mean(Data$PTAudAllAC),
                                       ImpCP_VorMore = predgrid[i,1],
                                       AdultsOnly = predgrid[i,2],
                                       Site = predgrid[i,3],
                                       SiteType = "Dayhike"),
                type = "resp",
                se.fit = TRUE)
  
  gridres = rbind(gridres, px$fit)
  gridres.se = rbind(gridres.se, px$se.fit)
}

medfit <- apply(gridres, 2, function(x) quantile(x, 0.5))

sefit <- apply(gridres.se, 2, function(x) quantile(x, 0.5))

polygon(c(
  seq(dataxlim[1], dataxlim[2], by = 0.1),
  seq(dataxlim[2], dataxlim[1], by = -0.1)),
  c(medfit-sefit, 
    rev(medfit+sefit)), 
  col=alpha(colz[1], 0.2),
  border = NA)

lines(seq(dataxlim[1], dataxlim[2], by = 0.1),
      medfit, 
      col=colz[1], lty = 1)

# now again, coloring in region of overnight
gridres <- gridres.se <- vector()

for(i in 1:nrow(predgrid)){
  px <- predict(intS.curve, data.frame(SELAllAC = seq(dataxlim[1], dataxlim[2], by = 0.1), 
                                       PEnHelos = mean(Data$PEnHelos),
                                       PEnProps = mean(Data$PEnProps),
                                       PTAudAllAC = mean(Data$PTAudAllAC),
                                       ImpCP_VorMore = predgrid[i,1],
                                       AdultsOnly = predgrid[i,2],
                                       Site = predgrid[i,3],
                                       SiteType = "Overnight"),
                type = "resp",
                se.fit = TRUE)
  
  gridres = rbind(gridres, px$fit)
  gridres.se = rbind(gridres.se, px$se.fit)
}

medfit <- apply(gridres, 2, function(x) quantile(x, 0.5))

sefit <- apply(gridres.se, 2, function(x) quantile(x, 0.5))

polygon(c(
  seq(dataxlim[1], dataxlim[2], by = 0.1),
  seq(dataxlim[2], dataxlim[1], by = -0.1)),
  c(medfit-sefit, 
    rev(medfit+sefit)), 
  col=alpha(colz2[1], 0.1),
  border = NA)

lines(seq(dataxlim[1], dataxlim[2], by = 0.1),
      medfit, 
      col=alpha(colz2[1], 0.1), lty = 1, lwd = 2)

gridres <- gridres.se <- vector()

for(i in 1:nrow(predgrid)){
  px <- predict(intS.curve, data.frame(SELAllAC = seq(overnightxlim[1], overnightxlim[2], by = 0.1), 
                                       PEnHelos = mean(Data$PEnHelos),
                                       PEnProps = mean(Data$PEnProps),
                                       PTAudAllAC = mean(Data$PTAudAllAC),
                                       ImpCP_VorMore = predgrid[i,1],
                                       AdultsOnly = predgrid[i,2],
                                       Site = predgrid[i,3],
                                       SiteType = "Overnight"),
                type = "resp",
                se.fit = TRUE)
  
  gridres = rbind(gridres, px$fit)
  gridres.se = rbind(gridres.se, px$se.fit)
}

medfit <- apply(gridres, 2, function(x) quantile(x, 0.5))

sefit <- apply(gridres.se, 2, function(x) quantile(x, 0.5))


polygon(c(
  seq(overnightxlim[1], overnightxlim[2], by = 0.1),
  seq(overnightxlim[2], overnightxlim[1], by = -0.1)),
  c(medfit-sefit, 
    rev(medfit+sefit)), 
  col=alpha(colz2[1], 0.4),
  border = NA)

lines(seq(overnightxlim[1], overnightxlim[2], by = 0.1),
      medfit, 
      col=colz2[1], lty = 1, lwd = 2)

legend("topleft",
       lwd = c(1, 2), 
       col = c(colz[1], colz2[1]),
       inset = 0.08,
       bty = "n",
       legend = c("Dayhike", "Multi-day"),
       title = "Slightly or more",
       cex = 1.5)


### IntWithNQ_MorMore
# Best model: SELAllAC + PEnHelos + PEnProps + Survey + ImpCP_VorMore

plot(Data$SELAllAC, jitter(Data$IntWithNQ_MorMore, factor = 0.03)-ptoffset, 
     pch = 16, cex = ptcex, 
     xlim = c(dataxlim[1], dataxlim[2]),
     col = alpha(colz[2], ptalpha),
     xlab = "LAE (dBA)",
     ylab = "Prob(Interference with Natural Quiet)")

points(Data$SELAllAC[Data$SiteType=="Overnight"], 
       jitter(Data$IntWithNQ_MorMore[Data$SiteType=="Overnight"], factor = 0.03)+ptoffset, 
       pch = 16, cex = ptcex, 
       col = alpha(colz2[2], ptalpha.bc))

title(main = "Likelihood of Interference with Natural Quiet")

gridres <- gridres.se <- vector()

for(i in 1:nrow(predgrid)){
  px <- predict(intM.curve, data.frame(SELAllAC = seq(dataxlim[1], dataxlim[2], by = 0.1), 
                                       PEnHelos = mean(Data$PEnHelos),
                                       PEnProps = mean(Data$PEnProps),
                                       PTAudAllAC = mean(Data$PTAudAllAC),
                                       ImpCP_VorMore = predgrid[i,1],
                                       AdultsOnly = predgrid[i,2],
                                       Site = predgrid[i,3],
                                       SiteType = "Dayhike"),
                type = "resp",
                se.fit = TRUE)
  
  gridres = rbind(gridres, px$fit)
  gridres.se = rbind(gridres.se, px$se.fit)
}

medfit <- apply(gridres, 2, function(x) quantile(x, 0.5))

sefit <- apply(gridres.se, 2, function(x) quantile(x, 0.5))


polygon(c(
  seq(dataxlim[1], dataxlim[2], by = 0.1),
  seq(dataxlim[2], dataxlim[1], by = -0.1)),
  c(medfit-sefit, 
    rev(medfit+sefit)), 
  col=alpha(colz[2], 0.2),
  border = NA)

lines(seq(dataxlim[1], dataxlim[2], by = 0.1),
      medfit, 
      col=colz[2], lty = 1)


gridres <- gridres.se <- vector()

for(i in 1:nrow(predgrid)){
  px <- predict(intM.curve, data.frame(SELAllAC = seq(dataxlim[1], dataxlim[2], by = 0.1), 
                                       PEnHelos = mean(Data$PEnHelos),
                                       PEnProps = mean(Data$PEnProps),
                                       PTAudAllAC = mean(Data$PTAudAllAC),
                                       ImpCP_VorMore = predgrid[i,1],
                                       AdultsOnly = predgrid[i,2],
                                       Site = predgrid[i,3],
                                       SiteType = "Overnight"),
                type = "resp",
                se.fit = TRUE)
  
  gridres = rbind(gridres, px$fit)
  gridres.se = rbind(gridres.se, px$se.fit)
}

medfit <- apply(gridres, 2, function(x) quantile(x, 0.5))

sefit <- apply(gridres.se, 2, function(x) quantile(x, 0.5))

polygon(c(
  seq(dataxlim[1], dataxlim[2], by = 0.1),
  seq(dataxlim[2], dataxlim[1], by = -0.1)),
  c(medfit-sefit, 
    rev(medfit+sefit)), 
  col=alpha(colz2[1], 0.1),
  border = NA)

lines(seq(dataxlim[1], dataxlim[2], by = 0.1),
      medfit, 
      col=alpha(colz2[1], 0.1), lty = 1, lwd = 2)

gridres <- gridres.se <- vector()

for(i in 1:nrow(predgrid)){
  px <- predict(intM.curve, data.frame(SELAllAC = seq(overnightxlim[1], overnightxlim[2], by = 0.1), 
                                       PEnHelos = mean(Data$PEnHelos),
                                       PEnProps = mean(Data$PEnProps),
                                       PTAudAllAC = mean(Data$PTAudAllAC),
                                       ImpCP_VorMore = predgrid[i,1],
                                       AdultsOnly = predgrid[i,2],
                                       Site = predgrid[i,3],
                                       SiteType = "Overnight"),
                type = "resp",
                se.fit = TRUE)
  
  gridres = rbind(gridres, px$fit)
  gridres.se = rbind(gridres.se, px$se.fit)
}

medfit <- apply(gridres, 2, function(x) quantile(x, 0.5))

sefit <- apply(gridres.se, 2, function(x) quantile(x, 0.5))


polygon(c(
  seq(overnightxlim[1], overnightxlim[2], by = 0.1),
  seq(overnightxlim[2], overnightxlim[1], by = -0.1)),
  c(medfit-sefit, 
    rev(medfit+sefit)), 
  col=alpha(colz2[1], 0.4),
  border = NA)

lines(seq(overnightxlim[1], overnightxlim[2], by = 0.1),
      medfit, 
      col=colz2[1], lty = 1, lwd = 2)

legend("topleft",
       lwd = 2, 
       col = "white",
       inset = 0.08,
       bty = "n",
       legend = "",
       title = "Moderately or more",
       cex = 1.5)

### IntWithNQ_VorMore
plot(Data$SELAllAC, jitter(Data$IntWithNQ_VorMore, factor = 0.03)-ptoffset, 
     pch = 16, cex = ptcex, 
     xlim = c(dataxlim[1], dataxlim[2]),
     col = alpha(colz[3], ptalpha),
     xlab = "LAE (dBA)",
     ylab = "Prob(Interference with Natural Quiet)")

points(Data$SELAllAC[Data$SiteType=="Overnight"], 
       jitter(Data$IntWithNQ_VorMore[Data$SiteType=="Overnight"], factor = 0.03)+ptoffset, 
       pch = 16, cex = ptcex, 
       col = alpha(colz2[3], ptalpha.bc))

gridres <- gridres.se <- vector()

for(i in 1:nrow(predgrid)){
  px <- predict(intV.curve, data.frame(SELAllAC = seq(dataxlim[1], dataxlim[2], by = 0.1), 
                                       PEnHelos = mean(Data$PEnHelos),
                                       PEnProps = mean(Data$PEnProps),
                                       PTAudAllAC = mean(Data$PTAudAllAC),
                                       ImpCP_VorMore = predgrid[i,1],
                                       AdultsOnly = predgrid[i,2],
                                       Site = predgrid[i,3],
                                       SiteType = "Dayhike"),
                type = "resp",
                se.fit = TRUE)
  
  gridres = rbind(gridres, px$fit)
  gridres.se = rbind(gridres.se, px$se.fit)
}

medfit <- apply(gridres, 2, function(x) quantile(x, 0.5))

sefit <- apply(gridres.se, 2, function(x) quantile(x, 0.5))


polygon(c(
  seq(dataxlim[1], dataxlim[2], by = 0.1),
  seq(dataxlim[2], dataxlim[1], by = -0.1)),
  c(medfit-sefit, 
    rev(medfit+sefit)), 
  col=alpha(colz[3], 0.2),
  border = NA)

lines(seq(dataxlim[1], dataxlim[2], by = 0.1),
      medfit, 
      col=colz[3], lty = 1)

# now again, coloring in region of overnight

gridres <- gridres.se <- vector()

for(i in 1:nrow(predgrid)){
  px <- predict(intV.curve, data.frame(SELAllAC = seq(dataxlim[1], dataxlim[2], by = 0.1), 
                                       PEnHelos = mean(Data$PEnHelos),
                                       PEnProps = mean(Data$PEnProps),
                                       PTAudAllAC = mean(Data$PTAudAllAC),
                                       ImpCP_VorMore = predgrid[i,1],
                                       AdultsOnly = predgrid[i,2],
                                       Site = predgrid[i,3],
                                       SiteType = "Overnight"),
                type = "resp",
                se.fit = TRUE)
  
  gridres = rbind(gridres, px$fit)
  gridres.se = rbind(gridres.se, px$se.fit)
}

medfit <- apply(gridres, 2, function(x) quantile(x, 0.5))

sefit <- apply(gridres.se, 2, function(x) quantile(x, 0.5))

polygon(c(
  seq(dataxlim[1], dataxlim[2], by = 0.1),
  seq(dataxlim[2], dataxlim[1], by = -0.1)),
  c(medfit-sefit, 
    rev(medfit+sefit)), 
  col=alpha(colz2[1], 0.1),
  border = NA)

lines(seq(dataxlim[1], dataxlim[2], by = 0.1),
      medfit, 
      col=alpha(colz2[1], 0.1), lty = 1, lwd = 2)

gridres <- gridres.se <- vector()

for(i in 1:nrow(predgrid)){
  px <- predict(intV.curve, data.frame(SELAllAC = seq(overnightxlim[1], overnightxlim[2], by = 0.1), 
                                       PEnHelos = mean(Data$PEnHelos),
                                       PEnProps = mean(Data$PEnProps),
                                       PTAudAllAC = mean(Data$PTAudAllAC),
                                       ImpCP_VorMore = predgrid[i,1],
                                       AdultsOnly = predgrid[i,2],
                                       Site = predgrid[i,3],
                                       SiteType = "Overnight"),
                type = "resp",
                se.fit = TRUE)
  
  gridres = rbind(gridres, px$fit)
  gridres.se = rbind(gridres.se, px$se.fit)
}

medfit <- apply(gridres, 2, function(x) quantile(x, 0.5))

sefit <- apply(gridres.se, 2, function(x) quantile(x, 0.5))


polygon(c(
  seq(overnightxlim[1], overnightxlim[2], by = 0.1),
  seq(overnightxlim[2], overnightxlim[1], by = -0.1)),
  c(medfit-sefit, 
    rev(medfit+sefit)), 
  col=alpha(colz2[1], 0.4),
  border = NA)

lines(seq(overnightxlim[1], overnightxlim[2], by = 0.1),
      medfit, 
      col=colz2[1], lty = 1, lwd = 2)

legend("topleft",
       lwd = 2, 
       col = "white",
       inset = 0.08,
       bty = "n",
       legend = "",
       title = "Very or more",
       cex = 1.5)

dev.off()

# compile random effects into a table ---- 

siteran <- typeran <- vector()
AICs <- Nobs <- vector()

counter = 1
for(i in c(annS, annM, annV, 
           intS, intM, intV)){

  AICs <- c(AICs, AIC(i))
  
  Nobs <- c(Nobs, length(summary(i)$residuals))
  
  if(counter == 1){
    siteran <- ranef(i)$Site[,1]
    typeran <- ranef(i)$SiteType[,1]
    
  } else {

    siteran <- data.frame(siteran, ranef(i)$Site[,1])
    typeran <- data.frame(typeran, ranef(i)$SiteType[,1])

  }
  counter = counter+1
  }

names(AICs) <- names(Nobs) <- colnames(siteran) <- names(typeran) <- c('annS', 'annM', 'annV', 'intS', 'intM', 'intV')
rownames(siteran) <- c('Grandview','Hermit','Sperry','Wrim')
rownames(typeran) <- c('Dayhike', 'Overnight')

round(siteran, 3)
round(typeran, 3)

data.frame(AICs,Nobs)
