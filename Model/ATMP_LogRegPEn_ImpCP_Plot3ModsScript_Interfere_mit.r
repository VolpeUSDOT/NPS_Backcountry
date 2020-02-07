### Script to fit published model to new data (DRMerged2011subset.csv)
### Erika A Sudderth Sept 12, 2013. Modified from scripts by Grant Anderson. Some extra code from Grant used to plot dose-response curves for other site types )Overlook, short hike) is still included below.

### The code below produces the model fits to the new (2011) survey data that using the published models that were determined to have the best fit to the older survey data (Noise Control Eng. J. 59(5):519-540).

###########################################################################
#### COMPUTATIONS
###########################################################################
######	DEFINE plot VARIABLES FROM DesiredRegrType and DesiredPlotY..., AND

#EAS: Add the two survey types to the list of varnames (change if the reference survey is different than "AC")

varnames.plot = c(DesiredPlotY, vars.dos, vars.interact, vars.mit)

	if (DesiredPlotY == "IntWithNQ_SorMore") {
		fit.plot = fit.1
		#vars.all.data = vars.all.data.1
		eq.plot = results[1,"eq"]
		add.plot=FALSE
    col.line="black"
	}
	if (DesiredPlotY == "IntWithNQ_MorMore") {
		fit.plot = fit.2
		#vars.all.data = vars.all.data.2
		eq.plot = results[2,"eq"]
    add.plot=TRUE
    col.line="gray40"
	}	
	if (DesiredPlotY == "IntWithNQ_VorMore") {
		fit.plot = fit.3
		#vars.all.data = vars.all.data.3
		eq.plot = results[3,"eq"]
		add.plot=TRUE
    col.line="gray80"
}
# 	if (DesiredPlotY == "IntWithNQ_SorMore") {
# 		fit.plot = fit.4
# 		#vars.all.data = vars.all.data.4
# 		eq.plot = results[4,"eq"]
# 		add.plot=FALSE
# 		col.line="black"
# }
# 	if (DesiredPlotY == "IntWithNQ_MorMore") {
# 		fit.plot = fit.5
# 		#vars.all.data = vars.all.data.5
# 		eq.plot = results[5,"eq"]
# 		add.plot=TRUE
# 		col.line="gray40"
# }
# 	if (DesiredPlotY == "IntWithNQ_VorMore") {
# 		fit.plot = fit.6
# 		#vars.all.data = vars.all.data.6
# 		eq.plot = results[6,"eq"]
# 		add.plot=TRUE
# 		col.line="gray80"
# }
	

######  CONSTRUCT MITIGATOR PERCENTAGES FOR JUST THIS SiteType
######  Comment out if want the input values
SiteType <- SType

if (SType == "DayHike") {
		sset = subset(vars.all.data, SiteType == SType)
	} else {
		sset = subset(vars.all.data, SiteType == SType)
	}

######	CONSTRUCT PLOT LABEL WITH HEADERS FOR PLOTTED MITIGATOR PERCENTAGES
# 	Label = paste("ImpCP_VorMore: ", Perct.ImpCP,
# 		"%   SiteVisitBefore: ", Perct.Before,
# 		"%   AdultsOnly: ", Perct.Adults,
# 		"%", sep="")

######	MODIFY VARIABLES AS NEEDED BELOW
##	Modify DesiredPlotX, if needed, so it looks good along the X-axis of the plots
	DesiredPlotX.logpre = gsub("10(", "", DesiredPlotX, fixed = T)
	DesiredPlotX.logpre = gsub(" + 0.001)", "", DesiredPlotX.logpre, fixed = T)
	DesiredPlotX.nolog = gsub("log", "", DesiredPlotX.logpre, fixed = T)

###### DETERMINE LINEAR REGRESSIONS AGAINST PLOT VARIABLE, FOR ALL NON-PLOTTED DOSES
######    Good for all DesiredPlotY

	####	Code below here glued to four site types
	####	Set up regressions
		#### Adding Yes to vars.testdosmit
#### Adding Yes to vars.testdosmit
vars.dosmit = c(vars.dos, vars.interact, vars.mit)
#vars.dosmitYes <- c(names(fixef(fit.plot))[2:5],paste(names(fixef(fit.plot))[6:7],"Yes",sep=''),names(fixef(fit.plot))[8:10])

			vars.dosmitYes = rep(NA, length(vars.dosmit))
			for (i in 1:length(vars.dosmit)) {
				try = FALSE
				for (j in 1:length(names(fixef(fit.plot)))) {
					n.char = nchar(names(fixef(fit.plot)))[j]
					if (substring(names(fixef(fit.plot)[j]), 1, n.char - 3) == vars.dosmit[i]) {
						try = TRUE
					} #else if (vars.dosmit[i] == "SurveyHR1" | vars.dosmit[i] == "SurveyHR2"){
					  #try = TRUE
					#}
				}
				if (try == TRUE) {
					vars.dosmitYes[i] = paste(vars.dosmit[i], "Yes", sep="")
				}
				if (try == FALSE) {
					vars.dosmitYes[i] = vars.dosmit[i]
				}
			}
		

		#### Eliminating one instance of SiteType from vars.dosmitYes
			index = 0
			for (i in 1:length(vars.dosmit)) {
				if (vars.dosmit[i] == "SiteType") {
					index = i
				}
			}
			vars.notplot = vars.dosmit[-index]
			
			numb.lm = length(vars.dosmit)
			lm.matrix = rep(NA, 2 * numb.lm)
			dim(lm.matrix) = c(numb.lm, 2)
			lm.input = as.data.frame(lm.matrix)
			colnames(lm.input) = c("Constant","Slope")
			rownames(lm.input) = vars.dosmit
			
	#### Do regressions, to relate all the non-plotted doses to the plotted dose
	#### Code below here glued to ImpCP_VorMore, SiteVisitBefore, AdultsOnly, as well as CultOverlook (ref), DayHike, Overlook, ShortHike

#### Do regressions, to relate all the non-plotted doses to the plotted dose
#### Code below here glued to ImpCP_VorMore, SiteVisitBefore, AdultsOnly, as well as CultOverlook (ref), DayHike, Overlook, ShortHike

#### UPDATE EAS: Use logistic regression to relate non-plotted doses (PEn) to the plotted doses - for the 2011 survey data, linear regressions are not a good fit to the dose/PEn relationships. 

#attach(vars.all.data)	

for (i in 1:numb.lm) {
  if (vars.dosmit[i] == DesiredPlotX) {
    lm.input[vars.dosmit[i], "Constant"] = 0			#This is the x-axis var.
    lm.input[vars.dosmit[i], "Slope"] = 1
  }
  if (vars.dosmit[i] == AddDose) {
    lm.input[vars.dosmit[i], "Constant"] = 0			#This is the additional dose variable(s): regress against primary dose.
    lm.input[vars.dosmit[i], "Slope"] = 1
    eq = paste(noquote(vars.dosmit[i]), " ~ ", noquote(DesiredPlotX), sep="")
    eq
    lmfit = lm(eq, data=vars.all.data)
    lm.input[vars.dosmit[i],"Constant"] = lmfit$coefficients[1]
    lm.input[vars.dosmit[i],"Slope"] = lmfit$coefficients[2]
  }
  if (vars.dosmit[i] == "Survey") {          					#Specific var.
    table.var=which(names(vars.all.data)==Survey)
    lm.input[vars.dosmit[i], "Constant"] = table(vars.all.data[,table.var])[2]/(sum(table(vars.all.data[,table.var])))
    lm.input[vars.dosmit[i], "Slope"] = 0
  } 
  if (vars.dosmit[i] != vars.dosmitYes[i]) {          #Not YesNo vars.
      table.var=which(names(vars.all.data)==vars.dosmit[i])
      lm.input[vars.dosmit[i], "Constant"] = table(vars.all.data[,table.var])[2]/(sum(table(vars.all.data[,table.var])))
      lm.input[vars.dosmit[i], "Slope"] = 0
  } else {
      if (vars.dosmit[i] == "PEnHelos" | vars.dosmit[i] == "PEnProps") {
        eq = paste(noquote(vars.dosmit[i]),".Binary"," ~ ", noquote(DesiredPlotX), sep="")
        eq
        lmfit = glm(eq, data=vars.all.data, family=binomial(link="logit"))
        lm.input[vars.dosmit[i],"Constant"] = lmfit$coefficients[1]
        lm.input[vars.dosmit[i],"Slope"] = lmfit$coefficients[2]
      }
    }
  }			
#detach()

##########
write.csv(lm.input,file = paste("lm_coeff_", DesiredPlotY,"_",paste(vars.dosmit,collapse="_"),"_Interfere.csv",sep=""))

###### Jitter response
	library("stats")
#	source("R_Working\\jitterbinary.R")

jitter.binary <- function(a, jitt=.05){ a + (1-2*a)*runif(length(a),0,jitt) } 

#Minus unity converts 1,2 to 0,1
Res.jit = 100*jitter.binary(as.numeric(vars.all.data[,1])-1)

#### Variable names for plots
	#### Variable names
		varnames.nores = varnames.plot
		varnames.nores = varnames.nores[-c(1)]

		tempp = rep("NA", length(varnames.nores))
		indx = 1
		for (v in 1:length(varnames.nores)) {
			if (varnames.nores[v] != "SiteType") {
				tempp[indx] = varnames.nores[v]
				indx = indx + 1
			} 
		}
	
		if (tempp[length(varnames.nores)] == "NA") {
			tempp = tempp[-length(varnames.nores)]
		}
		
		if (length(varnames.nores) != length(tempp)) {
			YesSTyp = TRUE
		}	else {
			YesSTyp = FALSE
		}

		varnames.nores = tempp
		varnames.nores.NoI = gsub("I(", "(", varnames.nores, fixed = T)
		varnames.nores.NoI.logpre = gsub("10", "", varnames.nores.NoI, fixed = T)
				
		#### Modify varnames.nores when it needs the "Yes" at end
			varnames.noresYes = rep(NA, length(varnames.nores))
			for (i in 1:length(varnames.nores)) {
				try = FALSE
				for (j in 1:length(names(fixef(fit.plot)))) {
				  n.char = nchar(names(fixef(fit.plot)))[j]
				  if (substring(names(fixef(fit.plot)[j]), 1, n.char - 3) == varnames.nores[i]) {
				    try = TRUE
					}
				}
				if (try == TRUE) {
					varnames.noresYes[i] = paste(varnames.nores[i], "Yes", sep="")
				}
				if (try == FALSE) {
					varnames.noresYes[i] = varnames.nores[i]
				}
			}

### Build of vars.math: use logistic curve to relate PEn to Leq
  vars.math = varnames.nores.NoI
	 	for (i in 1:length(vars.math)) {
			for (j in 1:dim(lm.input)[1]) {
			  if (vars.math[i] == "PEnHelos") {
			    vars.math[i] = gsub(
			      rownames(lm.input)[j],
			      paste("100*invlogit(",lm.input[j ,1],"+",lm.input[j, 2],"*xplot.comp)", sep=""), vars.math[i], fixed = T) 
			  } else if (vars.math[i] == "PEnProps"){
			    vars.math[i] = gsub(
			      rownames(lm.input)[j],
			      paste("100*invlogit(",lm.input[j ,1],"+",lm.input[j, 2],"*xplot.comp)", sep=""), vars.math[i], fixed = T) 
			  } else if (vars.math[i] == "(PEnHelos * PEnProps)"){
			    vars.math[i] = 
			      paste("100*invlogit(",lm.input[which(rownames(lm.input)=="PEnHelos"),1],"+",lm.input[which(rownames(lm.input)=="PEnHelos"), 2],"*xplot.comp)","*","100*invlogit(",lm.input[which(rownames(lm.input)=="PEnProps"),1],"+",lm.input[which(rownames(lm.input)=="PEnProps"), 2],"*xplot.comp)", sep="") 
			    #} else if (vars.math[i] == "(PEnHelos * PEnProps)"){
			    #vars.math[i] = "(0 + 1*xplot.comp)" 
			  } else {
				vars.math[i] = gsub(
          rownames(lm.input)[j],
					paste("(", lm.input[j ,1], " + ", lm.input[j, 2], "*xplot.comp)", sep=""), vars.math[i], fixed = T) 
			        }
			}
		}

#######################################################################
##### Set up graph by plotting points

####  Set up xplot values
xnumb = 50
  xmin = min(vars.all.data[DesiredPlotX])
  xmax = max(vars.all.data[DesiredPlotX])
   xmin = xlimits[1]
   xmax = xlimits[2]

if (substring(DesiredPlotX, 1, 3) != "log") {
	xplot = seq(xmin, xmax, (xmax - xmin)/(xnumb - 1))
	xplot.comp = xplot
}
if (substring(DesiredPlotX, 1, 3) == "log") {
	xplot = seq(log10(xmin + 0.001), log10(xmax), (log10(xmax) - log10(xmin + 0.001))/(xnumb - 1))
xplot = 10^xplot
xplot.comp = log10(xplot + 0.001)
}

log.eq.plot		= gsub("10(", "", eq.plot, fixed = T)
log.eq.plot		= gsub(" + 0.001)", "", log.eq.plot, fixed = T)
log.eq.plot 	= gsub("I(", "(", log.eq.plot, fixed = T)
log.eq.plot   = gsub(") + S", ")\n+ S", log.eq.plot, fixed = T)

 
if (HeaderEq == TRUE) {
	HeadMain = log.eq.plot
} else {
	HeadMain = ""
}

if (YesSTyp == TRUE) {
	STypText = SType
	} else {
	STypText = ""
}
if (SType == "DayHike") {
	STypText = SType
}
	
#### THE ACTUAL FIRST PLOT
if (DesiredRegrType != "Ordinal" & add.plot==FALSE) {	
	plot(Res.jit ~ vars.all.data[[DesiredPlotX]],
    col=col.line,
		xlim = xlimits,
		main = paste(Title,SType),
		xlab = xlabel, ylab = ylabel, font.lab = 2,
		lab = c(8,10,6), cex = 0.45, cex.lab = 1.8, cex.main = 2, cex.sub = 1.5,
		cex.axis = 1.5
    )
  legend(xlimits[1],95,legend=c("Slightly or more","Moderately or more","Very or more"),lty=1,lwd=4, text.font=2,col=c("black", "gray40", "gray80"),bty="n", xjust=0, y.intersp=.9,cex=1.6)
}

#### Simulate
library("arm")
n.sims = max(num.sims, num.curves)
fit.plot.sim = sim(fit.plot, n.sims)
fit.plot.fixef <- data.frame(fixef(fit.plot.sim))
names(fit.plot.fixef) <- names(fixef(fit.plot))
head(fit.plot.fixef)

#### Plot EquationCurve
#### Equation (eq.curve)
SType.full = paste("SiteType", SType, sep="")

eq.curve = "100*invlogit(fixef(fit.plot)[1]"

if (YesSTyp == TRUE) {
  if (DataType == "2011") {
    if (SType != "CultOverlook") {
      eq.curve = paste(eq.curve, " + fixef(fit.plot)['", SType.full, "']", sep="")
    }
  } else if (DataType == "AllCorrected") {
    if (SType != "BackCtyDayHike") {
      eq.curve = paste(eq.curve, " + fixef(fit.plot)['", SType.full, "']", sep="")
    }
  } else if (DataType == "AllCorrectedNoPrior") {
    if (SType != "BackCtyDayHike") {
      eq.curve = paste(eq.curve, " + fixef(fit.plot)['", SType.full, "']", sep="")
    }
  } else if (DataType == "AllCorrectedOnlyPrior") {
    if (SType != "BCOvernight") {
      eq.curve = paste(eq.curve, " + fixef(fit.plot)['", SType.full, "']", sep="")
    }
  } else {
    if (SType != "BCOvernight") {
      eq.curve = paste(eq.curve, " + fixef(fit.plot)['", SType.full, "']", sep="")
    }
  }
}


for (i in 1:length(varnames.noresYes)) {
	eq.curve = paste(eq.curve, " + fixef(fit.plot)['", varnames.noresYes[i], "']*", vars.math[i], sep="")
}
eq.curve.truncParen = eq.curve
eq.curve = paste(eq.curve, ")", sep="")

library("arm")
if (EquationCurve == TRUE & DesiredRegrType != "Ordinal") {
	yplot = rep(NA,50)
	yplot = eval(parse(text = eq.curve))		
	lines(xplot, yplot, col=col.line,lwd=4)
}

##### Plot GrayCurves

if (GrayCurves == TRUE & DesiredRegrType != "Ordinal") {
  eq.gray = gsub("fixef(fit.plot)[", "fit.plot.fixef[j,", eq.curve, fixed = T)
  

	library("arm")
	for (j in 1:num.curves){
		yplot = rep(NA, 50)
		yplot = eval(parse(text = eq.gray))
#		lines(xplot, yplot, col="gray")
		lines(xplot, yplot, col="gray60")
	}
}		

##### Plot ConfLimits
if (ConfLimits == TRUE & DesiredRegrType != "Ordinal") {
  eq.conf = gsub("fixef(fit.plot)[", "fit.plot.fixef[j,", eq.curve, fixed = T)
  eq.conf = gsub("*xplot.comp", "*xplot.comp[x.95]", eq.conf, fixed = T)
  
	## Determine y.top and y.bot values for each x.95[i]
	y.top = 1:xnumb
	y.bot = 1:xnumb
	
	for (x.95 in 1:xnumb) {	
		y.95 = 1:num.sims
		for (j in 1:num.sims) {
			y.95[j] = eval(parse(text = eq.conf))
		}
		yquant = quantile(y.95, probs=c(0:5, 95:100)/100, na.rm = TRUE)
		yint.smallest = 100
		for (k in 0:5) {                              #finds smallest 95% int
			yint.test = yquant[7+k] - yquant[1+k]       #  and returns ytop, ybot
			if(yint.test < yint.smallest) {             #  for this x[i]
				y.top[x.95] = yquant[7+k]
				y.bot[x.95] = yquant[1+k]
				yint.smallest = yint.test
			}
		}
	}

	lines(smooth.spline(xplot, y.top, spar=0.6), lwd=3, col = col.line, lty = "dashed")
	lines(smooth.spline(xplot, y.bot, spar=0.6), lwd=3, col = col.line, lty = "dashed")
}

#####	Plot random parts
	if (RandomParts == TRUE & DesiredRegrType != "Ordinal") {
		library("arm")
		loops = length(fit.plot@ranef)
		for (i in 1:loops) {
			eq.curve = paste(eq.curve.truncParen, " + ", fit.plot@ranef[i], sep="")
			eq.curve = paste(eq.curve, ")", sep="")
				
			yplot = rep(NA,50)
			yplot = eval(parse(text = eq.curve))		
			lines(xplot, yplot)
		}
	}
	
##### Replot points and curve, plus axes

if (EquationCurve == TRUE & DesiredRegrType != "Ordinal" & add.plot==FALSE) {
	points(vars.all.data[[DesiredPlotX]], Res.jit, cex = 0.5, col=col.line)

	library("arm")
	eq.curve = paste(eq.curve.truncParen, ")", sep="")
	yplot = rep(NA,50)
	yplot = eval(parse(text = eq.curve))		
		lines(xplot, yplot, col=col.line, lwd=5)
			
	curve (0*x, add=TRUE)
	curve (100+0*x, add=TRUE)

#	text(0.5*xlimits[1] + 0.5*xlimits[2], 90, STypText, cex = 1.4, vfont = NULL, font = 2)
# 	if (!is.na(Label)) {
# 		mtext(Label,side=3, font = 2, cex=1.2)
# 	}
}

