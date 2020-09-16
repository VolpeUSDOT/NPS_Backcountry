# clmm2 tutorial 

library(tidyverse)
library(ordinal)
data(wine)
head(wine)


fm1 <- clmm2(rating ~ temp + contact, random = judge, data = wine)


omitted <- wine[1:nrow(wine) %in% fm1$na.action,]
used <- wine[!1:nrow(wine) %in% fm1$na.action,]

fm2 <- clmm2(rating ~ temp + contact, 
             random = judge,
             data = wine,
             Hess=TRUE, nAGQ=10)

# These are the etas. Use these with the pred_clmm function to generate predictions for the proportion of the observations in each category
# feed new data into the predict step to see influence of different sound exposure variables.
# Don't plot these against observed categories
pred_eta <- predict(fm1, newdata = wine)

pred_clmm <- function(eta, theta, cat = 1:(length(theta)+1), inv.link = plogis) {
    Theta <- c(-1e3, theta, 1e3)
    sapply(cat, function(j)
      inv.link(Theta[j+1] - eta) - inv.link(Theta[j] - eta) )
  }

pred_clmm(qnorm(0.05) * fm1$stDev, fm1$Theta)
pred_clmm(qnorm(0.05) * fm2$stDev, fm2$Theta)


mat <- expand.grid(judge = qnorm(0.95) * c(-1, 0, 1) * fm2$stDev,
                      contact = c(0, fm2$beta[2]),
                      temp = c(0, fm2$beta[1]))

pred.mat <- pred_clmm(eta=rowSums(mat), theta=fm2$Theta)

lab <- paste("contact=", rep(levels(wine$contact), 2), ", ",
                "temp=", rep(levels(wine$temp), each=2), sep="")
par(mfrow=c(2, 2))
for(k in c(1, 4, 7, 10)) {
  plot(1:5, pred.mat[k,], lty=2, type = "l", ylim=c(0,1),
       xlab="Bitterness rating scale", axes=FALSE,
       ylab="Probability", main=lab[ceiling(k/3)], las=1)
  axis(1); axis(2)
  lines(1:5, pred.mat[k+1, ], lty=1)
  lines(1:5, pred.mat[k+2, ], lty=3)
  legend("topright",
         c("avg. judge", "5th %-tile judge", "95th %-tile judge"),
         lty=1:3, bty="n")
}

### From annoy models in Plot_Run_Final_CLMM_Models.R

annoy_01.2 <- clmm2(Annoy3 ~ SELAllAC + PEnProps + PEnHelos + SiteType, random = Site,
                    data = dC,
                    Hess = T) 

# Matrix: 5th, 50th (Average), and 95th percentile of site effects
# Then put in the coefficients for each predictor, as well as the absence of that predictor with a 0
# To generate across range of SELAllAC, need to feed in new range of sound exposure to a predict(clmm_model) statement to generate the etas, rather than making a prediction matrix as here. Then feed that to the pred_clmm function.

SELvals = c(40, 60, 80, 100)

mat <- expand.grid(Site = annoy_01.2$stDev,
                   SELAllAC = annoy_01.2$beta[1] * SELvals,
                   PEnProps = annoy_01.2$beta[2] * median(dC$PEnProps),
                   PEnHelos = annoy_01.2$beta[3] * median(dC$PEnHelos),
                   SiteTypeDayHike = c(0, annoy_01.2$beta[4]),
                   SiteTypeOverlook = c(0, annoy_01.2$beta[5]),
                   SiteTypeShortHike = c(0, annoy_01.2$beta[6])
                   )

# only keep rows where one site type is included
keep <- apply(mat[,c("SiteTypeDayHike", "SiteTypeOverlook", "SiteTypeShortHike")],
              1,
              function(x) sum(x == 0) >= 2)

mat <- mat[keep,]
rownames(mat) = 1:nrow(mat)

# thetas: transition thresholds

pred.mat <- pred_clmm(eta = rowSums(mat), theta = annoy_01.2$Theta)


# Plot probabilities for 5th, average, and 95th percentile sites, 
# For each site type

lab = c('Overnight', 'Day hike', 'Overlook', 'Short hike')

par(mfrow=c(2, 2))

for(k in c(1, 5, 9, 13)) {
  plot(1:4, pred.mat[k,], lty=2, type = "l", ylim = c(0,1),
       xlab="Annoyance Category", axes=FALSE,
       ylab="Probability", main = lab[ceiling(k/4)], las=1)
  axis(1); axis(2)
  lines(1:4, pred.mat[k+1, ], lty=1)
  lines(1:4, pred.mat[k+2, ], lty=3)
  lines(1:4, pred.mat[k+3, ], lty=4)
  legend("topright",
         title = 'SEL',
         legend = SELvals,
         lty=1:3, bty="n")
}


# Re-organized the prediction matrix, so that lines are for annoyace categories,
# and the x-axis is the sound exposure levels 

# Base (Site random - need random effect in model)
m1 <- clmm(Annoy3 ~ SELAllAC + PEnHelos + PEnProps + (1|Site),
           data = dC,
           Hess = T,
           link = "logit") # for proportional odds mixed model

# m1 summary
summary(m1)
confint(m1)
m1$Theta
m1$info[,c('nobs','AIC')] #nobs 4117 AIC 7197.1 

# Function scratch ----

library(tidyverse)
library(ordinal)

# Arugument: model name
model_object_name = 'm1'

# Dependency: pred_clmm function
pred_clmm <- function(eta, theta, cat = 1:(length(theta)+1), inv.link = plogis) {
  Theta <- c(-1e3, theta, 1e3)
  sapply(cat, function(j)
    inv.link(Theta[j+1] - eta) - inv.link(Theta[j] - eta) )
}

# Function content

plot_curves = function(model_object_name){

  model_object = get(model_object_name)
  
  stopifnot(class(model_object) %in% 'clmm')
  
  # Name of response?
  resp_name = names(model_object$model)[1]
  
  # Has sitetype?
  
  # Name of sound variable (leq vs SEL)
  has_SEL = grep('SELAllAC', names(model_object$model))
  has_Leq = grep('LeqAllAC', names(model_object$model))
  
  sound_var = ifelse(has_SEL, 'SELAllAC',
                     ifelse(has_Leq, 'LeqAllAC', NA))
  
  # Has lg10.PTAudAllAC
  
  #  DurVisitMinutes 
  #  AdultsOnly  
  # ImpNQ_VorMore
  
  if(resp_name == 'SEL')
  SELvals = seq(35, 125, by = 1)
  
  coef_names = coef(model_object)
  
  model_object$beta
  
  mat = expand.grid(Site = sd(summary(m1)$ranef),
                   SELAllAC = model_object$beta[1] * SELvals,
                   PEnProps = model_object$beta[2] * median(dC$PEnProps),
                   PEnHelos = model_object$beta[3] * median(dC$PEnHelos))
  
  # thetas: transition thresholds
  
  pred.mat <- pred_clmm(eta = rowSums(mat), theta = model_object$Theta)
  
  # Plot
  
  pred.df <- as.data.frame(pred.mat)
  colnames(pred.df) = c('NotAtAll', 'Somewhat', 'Moderately', 'Very+')  
  pred.df$SELAllAC = SELvals
  
  # Reformat long
  pred_long = pred.df %>%
    pivot_longer(cols = c('NotAtAll', 'Somewhat', 'Moderately', 'Very+'),
                 names_to = 'Level',
                 values_to = resp_name)
  
  ggplot(pred_long,
         aes(x = SELAllAC,
             y = Annoy3,
             col = Level)) + 
    geom_line()
  
  g1 <- ggplot(pred_long,
               aes(x = get(sound_var),
                   y = get(resp_name),
                   col = Level)) + 
    geom_line(size = 2) + 
    theme_bw() +
    xlab(sound_var) + ylab(resp_name) +
    ggtitle(paste(model_object_name, resp_name, sound_var))
  
  g1
}
