# Generates descriptive tables and figures for the overnight report.

library(lme4)
library(sjPlot)
library(scales)

################### Table 1 ----

d <- read.csv("Data/ATMP2011_CompleteDoseVars_dprime.csv")

d <- d[d$SiteType != "ShortHike",]

d <- d[d$DurVisitMinutes > 60,]

levels(d$SiteType) = c("Overnight", "Dayhike", "sh")

d$SiteType <- as.factor(as.character(d$SiteType))


# d <- d[d$Survey != "AC",] # Uncomment to excluding audio clip surveys

# Row 1
summary(d$SiteType)

tapply(d$DurVisitMinutes/60, d$SiteType, summary)

# Row 3
aggregate(d$DurVisitMinutes/60,
          by = list(d$Site, d$SiteType),
          mean, na.rm=T)

# Rows 4-10
xx <- aggregate(d[,c("ImpCP_VorMore",
                     "ImpNQ_VorMore",
                     "SiteVisitBefore",
                     "AdultsOnly",
                     "WatchBirds",
                     "Talk",
                     "AirTour")],
                by = list(d$SiteType),
                summary)

xx <- xx[1:2,]

xy = vector(); count=1

for(i in c("ImpCP_VorMore",
           "ImpNQ_VorMore",
           "SiteVisitBefore",
           "AdultsOnly",
           "WatchBirds",
           "Talk",
           "AirTour")){
  xy <- rbind(xy, apply(xx[,i],
                        1,
                        function(x)
                          x[2]/sum(x[1:2])))
  rownames(xy)[count] = i
  count=count+1
}

xy[7,] = 1-xy[7,]

round(xy*100,2)

#######################  Figure 1 histograms ---- 
colzbar = c("deepskyblue","deepskyblue4",
            "dodgerblue","dodgerblue4",
            "violet","violetred4")

colzbar[3] = alpha(colzbar[3], 0.3)
colzbar[4] = alpha(colzbar[4], 0.6)

par(mfrow = c(1, 2))

with(d[d$SiteType == "Dayhike",], hist(SELAllAC,
                                       xlab = "LAE",
                                       breaks = 25, 
                                       xlim = c(40, 95),
                                       col = colzbar[3],
                                       border = colzbar[3],
                                       main = ""))

with(d[d$SiteType == "Overnight",], hist(SELAllAC,
                                         add = TRUE,
                                         breaks = 15,
                                         col = colzbar[4],
                                         border = colzbar[4]))


with(d[d$SiteType == "Dayhike",], hist(PTAudAllAC,
                                       xlab = "% Time Audible",
                                       breaks = 25, 
                                       xlim = c(0, 100),
                                       col = colzbar[3],
                                       border = colzbar[3],
                                       main = ""))

with(d[d$SiteType == "Overnight",], hist(PTAudAllAC,
                                         add = TRUE,
                                         breaks = 15,
                                         col = colzbar[4],
                                         border = colzbar[4]))

legend("topright",
       bty = "n",
       fill = c(colzbar[3], colzbar[4]),
       legend = c("Dayhike", "Overnight")
)

dev.print(pdf, width = 8.5, height = 5, file = "Output/Histograms.pdf")

# Figure 2 SiteType by Day/Overnight ----

d <- read.csv("Data/ATMP2011_CompleteDoseVars_dprime.csv")

d <- d[d$SiteType != "ShortHike",]

d <- d[d$DurVisitMinutes > 60,]

keepsites <- tapply(d$SiteType, d$Site, function(x) length(x[x=="BCOvernight"])>0)
d <- d[d$Site %in% names(keepsites[keepsites==TRUE]),]

d$Site <- as.factor(as.character(d$Site)) # clear empty levels

levels(d$SiteType) = c("Overnight", "Dayhike", "sh")

d$SiteType <- as.factor(as.character(d$SiteType))

colzbar = alpha(c("deepskyblue","deepskyblue4",
                  "dodgerblue","dodgerblue4",
                  "violet","violetred4"), 0.8)

pdf("Output/Percent responses Xtab SEL SiteType.pdf", width = 10, height = 6)

par(xpd=T, mar = c(4, 4, 4, 4), mfrow = c(1,1))

j = "SiteType"
  res <- vector()
  res.mean <- vector()
  res.se <- vector()
  for(i in c("Annoy_SorMore", "Annoy_MorMore", "Annoy_VorMore",
             "IntWithNQ_SorMore", "IntWithNQ_MorMore", "IntWithNQ_VorMore")){
    
    xtab <- table(d[,i], d[,j])
    pcts <- apply(xtab, 2, function(x) round(100*x[2]/sum(x),2))
    
    res <- rbind(res, data.frame(var=i, t(data.frame(pcts))))
    
    sel.mean <- tapply(d[d[,i]=="Yes","SELAllAC"], d[d[,i]=="Yes", j], mean)
    sel.se <- tapply(d[d[,i]=="Yes","SELAllAC"], d[d[,i]=="Yes", j], function(x) sd(x)/sqrt(length(x)-1))
    
    res.mean <- rbind(res.mean, data.frame(var=i, t(data.frame(sel.mean))))
    res.se <- rbind(res.se, data.frame(var=i, t(data.frame(sel.se))))
    
    
  }
  
  rownames(res) = c("Annoy, Slight+", "Annoy, Mod+", "Annoy, Very+",
                    "Interfere, Slight+", "Interfere, Mod+", "Interfere, Very+")
  
  bp <- barplot(t(res[,2:3]), beside = T, col = colzbar,
                ylab = "Percent of respondents",
                ylim = c(0, 90))
  
  title(main = j)
  
  axis(4, at = c(0, 30, 60, 90))
  mtext("Sound Exposure Level (dBA)", side = 4, at = 50, line = 2)
  
  legend(x = 15, y = 100, 
         fill = colzbar[1:2],
         legend = levels(d[,j]),
         bty = "n"
  )
  
  points(as.numeric(bp),
         as.numeric(t(res.mean[,2:3])),
         pch = 5)
  
  
  arrows(as.numeric(bp),
         as.numeric(t(res.mean[,2:3]))-as.numeric(t(res.se[,2:3])),
         as.numeric(bp),
         as.numeric(t(res.mean[,2:3]))+as.numeric(t(res.se[,2:3])),
         angle = 90,
         length = 0.01,
         code = 3)
  
dev.off()

# Figure 3 by Site ----
# Pct reporting annoyance at dayhike/overnight
# site

colzbar4 = alpha(c("deepskyblue4", "dodgerblue4", "violet","seagreen"), 0.8)


pdf("Output/Percent responses Xtab SEL Site.pdf", width = 10, height = 6)

par(xpd=T, mar = c(4, 4, 4, 4))

j = "Site"
  res <- vector()
  res.mean <- vector()
  res.se <- vector()
  for(i in c("Annoy_SorMore", "Annoy_MorMore", "Annoy_VorMore",
             "IntWithNQ_SorMore", "IntWithNQ_MorMore", "IntWithNQ_VorMore")){
    
    xtab <- table(d[,i], d[,j])
    pcts <- apply(xtab, 2, function(x) round(100*x[2]/sum(x),2))
    
    res <- rbind(res, data.frame(var=i, t(data.frame(pcts))))
    
    sel.mean <- tapply(d[d[,i]=="Yes","SELAllAC"], d[d[,i]=="Yes", j], mean)
    sel.se <- tapply(d[d[,i]=="Yes","SELAllAC"], d[d[,i]=="Yes", j], function(x) sd(x)/sqrt(length(x)-1))
    
    res.mean <- rbind(res.mean, data.frame(var=i, t(data.frame(sel.mean))))
    res.se <- rbind(res.se, data.frame(var=i, t(data.frame(sel.se))))
    
    
  }
  
  rownames(res) = c("Annoy, Slight+", "Annoy, Mod+", "Annoy, Very+",
                    "Interfere, Slight+", "Interfere, Mod+", "Interfere, Very+")
  
  bp <- barplot(t(res[,2:ncol(res)]), beside = T, col = colzbar4,
                ylab = "Percent of respondents",
                ylim = c(0, 90))
  
  title(main = j)
  
  axis(4, at = c(0, 30, 60, 90))
  mtext("Sound Exposure Level (dBA)", side = 4, at = 50, line = 2)
  
  legend(x = 0, y = 100, 
         fill = colzbar4,
         legend = levels(d$Site),
         bty = "n",
         ncol = 2
  )
  
  points(as.numeric(bp),
         as.numeric(t(res.mean[,2:5])),
         pch = 5)
  
  
  arrows(as.numeric(bp),
         as.numeric(t(res.mean[,2:5]))-as.numeric(t(res.se[,2:5])),
         as.numeric(bp),
         as.numeric(t(res.mean[,2:5]))+as.numeric(t(res.se[,2:5])),
         angle = 90,
         length = 0.01,
         code = 3)
  

dev.off()

# Extra figures: All Characteristics  ----

d <- read.csv("Data/ATMP2011_CompleteDoseVars_dprime.csv")

d <- d[d$SiteType != "ShortHike",]

d <- d[d$DurVisitMinutes > 60,]

keepsites <- tapply(d$SiteType, d$Site, function(x) length(x[x=="BCOvernight"])>0)
d <- d[d$Site %in% names(keepsites[keepsites==TRUE]),]

d$Site <- as.factor(as.character(d$Site)) # clear empty levels

levels(d$SiteType) = c("Overnight", "Dayhike", "sh")

d$SiteType <- as.factor(as.character(d$SiteType))

colzbar = alpha(c("deepskyblue","deepskyblue4",
                  "dodgerblue","dodgerblue4",
                  "violet","violetred4"), 0.8)

pdf("Output/Percent responses Xtab SEL.pdf", width = 10, height = 6)

par(xpd=T, mar = c(4, 4, 4, 4))

for(j in c("SiteType", "WatchBirds", "ImpCP_VorMore", "AdultsOnly", "SiteVisitBefore", "Talk")){
  res <- vector()
  res.mean <- vector()
  res.se <- vector()
  for(i in c("Annoy_SorMore", "Annoy_MorMore", "Annoy_VorMore",
             "IntWithNQ_SorMore", "IntWithNQ_MorMore", "IntWithNQ_VorMore")){
    
    xtab <- table(d[,i], d[,j])
    pcts <- apply(xtab, 2, function(x) round(100*x[2]/sum(x),2))
    
    res <- rbind(res, data.frame(var=i, t(data.frame(pcts))))
    
    sel.mean <- tapply(d[d[,i]=="Yes","SELAllAC"], d[d[,i]=="Yes", j], mean)
    sel.se <- tapply(d[d[,i]=="Yes","SELAllAC"], d[d[,i]=="Yes", j], function(x) sd(x)/sqrt(length(x)-1))
    
    res.mean <- rbind(res.mean, data.frame(var=i, t(data.frame(sel.mean))))
    res.se <- rbind(res.se, data.frame(var=i, t(data.frame(sel.se))))
    
    
  }
  
  rownames(res) = c("Annoy, Slight+", "Annoy, Mod+", "Annoy, Very+",
                    "Interfere, Slight+", "Interfere, Mod+", "Interfere, Very+")
  
  bp <- barplot(t(res[,2:3]), beside = T, col = colzbar,
                ylab = "Percent of respondents",
                ylim = c(0, 90))
  
  title(main = j)
  
  axis(4, at = c(0, 30, 60, 90))
  mtext("Sound Exposure Level (dBA)", side = 4, at = 50, line = 2)
  
  legend(x = 15, y = 100, 
         fill = colzbar[1:2],
         legend = levels(d[,j]),
         bty = "n"
  )
  
  points(as.numeric(bp),
         as.numeric(t(res.mean[,2:3])),
         pch = 5)
  
  
  arrows(as.numeric(bp),
         as.numeric(t(res.mean[,2:3]))-as.numeric(t(res.se[,2:3])),
         as.numeric(bp),
         as.numeric(t(res.mean[,2:3]))+as.numeric(t(res.se[,2:3])),
         angle = 90,
         length = 0.01,
         code = 3)
  
}

dev.off()

