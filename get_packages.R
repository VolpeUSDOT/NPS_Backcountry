# Get all necessary packages across data prep and analysis scripts for the NPS Backcountry Dose-Response project

loadpacks <- c(
  "arm",
  "lme4",
  "sjPlot",
  "scales")

for(i in loadpacks){if(length(grep(i, (.packages(all.available=T))))==0) install.packages(i, dependencies =TRUE)}