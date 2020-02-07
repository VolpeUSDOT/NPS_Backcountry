# Find the best model

setwd("X:/Overnight")


aicfiles <- dir()[grep("CoeffAIC.csv", dir())]

# compile in one file for each set of compatible models

annoy <- aicfiles[grep("Annoy", aicfiles)]

res.annoy <- vector()

for(i in 1:length(annoy)){
  d <- read.csv(annoy[i])
  d$filename = annoy[i]
  d<-d[c("X","Response","AIC","BIC","logLike","Deviance","n.obs","filename")]
  res.annoy <- rbind(res.annoy, d)
}

# Sort for each of the three 

annS.res <- res.annoy[res.annoy$Response == "Annoy_SorMore",]
annS.res <- annS.res[order(annS.res$AIC),]
annS.res$deltaAIC <- exp(-0.5*c(0,diff(annS.res$AIC)))
annS.res$wi <- annS.res$deltaAIC/sum(annS.res$deltaAIC)

response = "Annoy_SorMore"

bestmodel = annS.res[order(annS.res$AIC),][1,]


daymodelS = annS.res[grep("SELAllACPTAudAllACPEnHelosPEnPropsSurveyImpCP_VorMoreSiteVisitBeforeAdultsOnlyWatchBirds",
                         annS.res$filename),]
daymodelS$rank = grep("SELAllACPTAudAllACPEnHelosPEnPropsSurveyImpCP_VorMoreSiteVisitBeforeAdultsOnlyWatchBirds",
                     annS.res$filename)

# 

annM.res <- res.annoy[res.annoy$Response == "Annoy_MorMore",]
response = c(response, "Annoy_MorMore")

annM.res <- annM.res[order(annM.res$AIC),]
annM.res$deltaAIC <- exp(-0.5*c(0,diff(annM.res$AIC)))
annM.res$wi <- annM.res$deltaAIC/sum(annM.res$deltaAIC)


bestmodel = rbind(bestmodel, annM.res[order(annM.res$AIC),][1,])


daymodelM = annM.res[grep("SELAllACPTAudAllACPEnHelosPEnPropsSurveyImpCP_VorMoreSiteVisitBeforeAdultsOnlyWatchBirds",
                         annM.res$filename),]
daymodelM$rank = grep("SELAllACPTAudAllACPEnHelosPEnPropsSurveyImpCP_VorMoreSiteVisitBeforeAdultsOnlyWatchBirds",
                     annM.res$filename)

#
annV.res <- res.annoy[res.annoy$Response == "Annoy_VorMore",]
response = c(response, "Annoy_VorMore")

annV.res <- annV.res[order(annV.res$AIC),]
annV.res$deltaAIC <- exp(-0.5*c(0,diff(annV.res$AIC)))
annV.res$wi <- annV.res$deltaAIC/sum(annV.res$deltaAIC)


bestmodel = rbind(bestmodel, annV.res[order(annV.res$AIC),][1,])


daymodelV = annV.res[grep("SELAllACPTAudAllACPEnHelosPEnPropsSurveyImpCP_VorMoreSiteVisitBeforeAdultsOnlyWatchBirds",
                          annV.res$filename),]
daymodelV$rank = grep("SELAllACPTAudAllACPEnHelosPEnPropsSurveyImpCP_VorMoreSiteVisitBeforeAdultsOnlyWatchBirds",
                      annV.res$filename)

daymodelA = rbind(daymodelS, daymodelM, daymodelV)

# Now, interfere

interfere <- aicfiles[grep("Interfere", aicfiles)]

res.interfere <- vector()

for(i in 1:length(interfere)){
  d <- read.csv(interfere[i])
  d$filename = interfere[i]
  d<-d[c("X","Response","AIC","BIC","logLike","Deviance","n.obs","filename")]
  res.interfere <- rbind(res.interfere, d)
}


# Sort for each of the three 

intS.res <- res.interfere[res.interfere$Response == "IntWithNQ_SorMore",]
intS.res[order(intS.res$AIC),][1:5,]

response = c(response, "IntWithNQ_SorMore")

intS.res <- intS.res[order(intS.res$AIC),]
intS.res$deltaAIC <- exp(-0.5*c(0,diff(intS.res$AIC)))
intS.res$wi <- intS.res$deltaAIC/sum(intS.res$deltaAIC)


bestmodel = rbind(bestmodel, intS.res[order(intS.res$AIC),][1,])


daymodelS = intS.res[grep("SELAllACPTAudAllACPEnHelosPEnPropsImpCP_VorMoreAdultsOnly_",
                          intS.res$filename),]
daymodelS$rank = grep("SELAllACPTAudAllACPEnHelosPEnPropsImpCP_VorMoreAdultsOnly_",
                      intS.res$filename)



# 

intM.res <- res.interfere[res.interfere$Response == "IntWithNQ_MorMore",]
response = c(response, "IntWithNQ_MorMore")
intM.res <- intM.res[order(intM.res$AIC),]
intM.res$deltaAIC <- exp(-0.5*c(0,diff(intM.res$AIC)))
intM.res$wi <- intM.res$deltaAIC/sum(intM.res$deltaAIC)



bestmodel = rbind(bestmodel, intM.res[order(intM.res$AIC),][1,])


daymodelM = intM.res[grep("SELAllACPTAudAllACPEnHelosPEnPropsImpCP_VorMoreAdultsOnly_",
                          intM.res$filename),]
daymodelM$rank = grep("SELAllACPTAudAllACPEnHelosPEnPropsImpCP_VorMoreAdultsOnly_",
                      intM.res$filename)


#
intV.res <- res.interfere[res.interfere$Response == "IntWithNQ_VorMore",]
response = c(response, "IntWithNQ_VorMore")

intV.res <- intV.res[order(intV.res$AIC),]
intV.res$deltaAIC <- exp(-0.5*c(0,diff(intV.res$AIC)))
intV.res$wi <- intV.res$deltaAIC/sum(intV.res$deltaAIC)


bestmodel = rbind(bestmodel, intV.res[order(intV.res$AIC),][1,])

daymodelV = intV.res[grep("SELAllACPTAudAllACPEnHelosPEnPropsImpCP_VorMoreAdultsOnly_",
                          intV.res$filename),]
daymodelV$rank = grep("SELAllACPTAudAllACPEnHelosPEnPropsImpCP_VorMoreAdultsOnly_",
                      intV.res$filename)

daymodel = rbind(daymodelA, daymodelS, daymodelM, daymodelV)

write.csv(data.frame(response, bestmodel), file = "Best Fit Models.csv", row.names=FALSE)


write.csv(daymodel, file = "Best Fit Models - From Dayhike.csv", row.names=FALSE)


