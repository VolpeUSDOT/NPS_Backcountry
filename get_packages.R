# Get all necessary packages across data prep and analysis scripts for the NPS Backcountry Dose-Response project

loadpacks <- c(
  "arm",
  "ggplot2",
  "lme4",
  "MASS",
  "ordinal",
  "readxl",
  "sjPlot",
  "scales",
  "tidyverse")

for(i in loadpacks){if(length(grep(i, (.packages(all.available=T))))==0) install.packages(i, dependencies =TRUE)}
