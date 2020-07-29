# Dose-response work 2016, updated for Grand Analysis using CLMM
# Dan Flynn | daniel.flynn@dot.gov

# Following https://cran.r-project.org/web/packages/ordinal/vignettes/clmm2_tutorial.pdf for plotting ideas

library(lme4)   # for glmer() generalized linear mixed effects models
library(sjPlot) # for summary tables using tab_model
library(scales) # for alpha() and muted()
library(ordinal) # for clmm

project_shared_drive = "//vntscex/DFS/Projects/PROJ-VXK600/MLB48"

output = file.path(project_shared_drive,
                   '2020 Grand Analysis',
                   'Output')

if(!dir.exists(output)){ dir.create(output) }

# For dAll data frame, 5233 observations and 31 variables
load(file.path(project_shared_drive,
               '2020 Grand Analysis',
               'GrandAnalysis_CompleteDoseVars.RData'))

dAll$SELAllAC <- as.numeric(dAll$SELAllAC)
dAll$PTAudHelos <- as.numeric(dAll$PTAudHelos)
dAll$lg10.PTAudAllAC <- as.numeric(dAll$lg10.PTAudAllAC)
dAll$PEnHelos <- as.numeric(dAll$PEnHelos)
dAll$PEnProps <- as.numeric(dAll$PEnProps)
dAll$DurVisitMinutes <- as.numeric(dAll$DurVisitMinutes)


dos_vars = c('SELAllAC', 'PEnProps','PEnHelos', 'PTAudAllAC', 'lg10.PTAudAllAC')
dat_vars = c('Dataset', 'Site', 'SiteType', 'Park')
med_vars = c('ImpHistCult_VorMore','ImpNQ_VorMore','SiteFirstVisit', 'DurVisitMinutes')
res_vars = c('Annoy3', 'IntWithNQ3')

# <<>><<>><<>><<>><<>><<>><<>><<>><<>>
# Re-run final models ----

# Complete dataset 
dC = dAll[complete.cases(dAll[,c('Annoy3',
                                 'SELAllAC',
                                 'PEnProps',
                                 'PEnHelos',
                                 'Dataset',
                                 'SiteType',
                                 'Site')]),]

# models 1 and 7 for annoy and interfere

# Manual model runs to interpret thresholds
annoy_01.0 <- clmm(Annoy3 ~ SELAllAC + PEnProps + PEnHelos + Dataset + SiteType + (1 | Site),
             data = dC,
             Hess = T,
             link = "logit") 

annoy_01.1 <- clmm(Annoy3 ~ SELAllAC + PEnProps + PEnHelos + SiteType + (1 | Site),
                 data = dC,
                 Hess = T,
                 link = "logit") 


annoy_01.2 <- clmm2(Annoy3 ~ SELAllAC + PEnProps + PEnHelos + SiteType, random = Site,
                    data = dC,
                    Hess = T) 

anova(annoy_01.0,
      annoy_01.1)

tab_model(annoy_01.2)

# Drop PEN vars
annoy_01.3 <- clmm2(Annoy3 ~ SELAllAC + SiteType, random = Site,
                    data = dC,
                    Hess = T) 


tab_model(annoy_01.3)

anova(annoy_01.2, annoy_01.3)
AIC(annoy_01.2, annoy_01.3) # Better without PENs

predict(annoy_01.3)

plot(dC$Annoy3, exp(fitted(annoy_01.3)))


int_01.2 <- clmm2(IntWithNQ3 ~ SELAllAC + PEnProps + PEnHelos + SiteType, random = Site,
                    data = dC) 


AIC(annoy_01.0,
    annoy_01.1,
    annoy_01.2) # Identical to clmm

# Slow..
# pa1 <- profile(annoy_01.2,  range=c(.1, 3), nSteps = 30, trace=0)
# plot(pa1)

plot(dC$Annoy3, annoy_01.2$fitted.values)

# Plotting random effects ----

par(mar = c(8, 4, 4, 2))
ci <- annoy_01.2$ranef + qnorm(0.975) * sqrt(annoy_01.2$condVar) %o% c(-1, 1)
ord.re <- order(annoy_01.2$ranef)
ci <- ci[order(annoy_01.2$ranef),]

plot(1:16, annoy_01.2$ranef[ord.re], axes=FALSE, ylim = range(ci),
        xlab="", ylab="Site effect",
     type = 'n')

axis(1, at = 1:16, labels = levels(dC$Site)[ord.re],
     las = 2)
axis(2)
for(i in 1:16) {
  segments(i, ci[i,1], i, ci[i, 2],
                        lwd = 2, col = 'grey50') 
  points(1:16, annoy_01.2$ranef[ord.re],
       pch = 16, 
       col = scales::alpha('midnightblue', 0.3))
  
  }

abline(h = 0, lty=2)
title(main = 'Site Effects for Annoyance Model')

# Plotting predicted curves ----
colz2 = rep("dodgerblue4", 3)
colz = rep("grey40", 3)
ptcex = 0.8
ptoffset = 0.015
ptalpha = 0.08
ptalpha.bc = 0.16

# par(mfrow=c(1, 3), xpd = F)
dataxlim = range(dC$SELAllAC) #c(25, 110)

# Annoyance plots ----
plot(dC$SELAllAC, as.numeric(as.character(dC$Annoy3)), 
     pch = 16, cex = ptcex, 
     xlim = c(dataxlim[1], dataxlim[2]),
     col = alpha(colz[1], ptalpha),
     xlab = "LAE (dBA)",
     ylab = "Annoyed level")

# Do all combinations of categorical variables. Plot median, 5 and 95 quantiles profile curves
predgrid <- expand.grid(
  levels(dC$Annoy3),
  levels(dC$SiteType),
  levels(dC$Site))

gridres <- gridres.se <- vector()

for(i in 1:nrow(predgrid)){
  # i = 1
  px <- predict(annoy_01.3, newdata = data.frame(SELAllAC = seq(dataxlim[1], dataxlim[2], by = 0.1),
                                                 Annoy3 = predgrid[i,1],
                                                 Site = predgrid[i,2],
                                                 SiteType = predgrid[i,3]),
                type = "prob",
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

