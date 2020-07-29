# clmm2 tutorial 

library(ordinal)
data(wine)
head(wine)


fm1 <- clmm2(rating ~ temp + contact, random = judge, data = wine)


omitted <- wine[1:nrow(wine) %in% fm1$na.action,]
used <- wine[!1:nrow(wine) %in% fm1$na.action,]
obs <- as.numeric(used$rating)
pred <- exp(fm1$fitted.values)

plot(pred ~ obs)
boxplot(pred ~ obs)

fm2 <- clmm2(rating ~ temp + contact, 
             random = judge,
             data = wine,
             Hess=TRUE, nAGQ=10)

pred2 <- predict(fm1, newdata = wine)

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
