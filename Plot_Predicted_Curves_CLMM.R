library(tidyverse) # for tidyr::pivot_wider and ggplot
library(ordinal)

# Dependency: pred_clmm function
pred_clmm <- function(eta, theta, cat = 1:(length(theta)+1), inv.link = plogis) {
  Theta <- c(-1e3, theta, 1e3)
  sapply(cat, function(j)
    inv.link(Theta[j+1] - eta) - inv.link(Theta[j] - eta) )
}

# Function content
# model_object_name = 'm11'

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
  
  if(sound_var == 'SELAllAC'){
    sound_vals = seq(35, 125, by = 1)
  }
  if(sound_var == 'LeqAllAC'){
    sound_vals = seq(-1, 65, by = 1)
  }
  
  coef_names = coef(model_object)
  
  mat = expand.grid(Site = sd(summary(m1)$ranef),
                   sound_var_temp = model_object$beta[1] * sound_vals,
                   PEnProps = model_object$beta[2] * median(dC$PEnProps),
                   PEnHelos = model_object$beta[3] * median(dC$PEnHelos))
  
  names(mat)[names(mat) == 'sound_var_temp'] = sound_var
  
  # thetas: transition thresholds
  
  pred.mat <- pred_clmm(eta = rowSums(mat), theta = model_object$Theta)
  
  # Plot
  
  pred.df <- as.data.frame(pred.mat)
  colnames(pred.df) = c('NotAtAll', 'Somewhat', 'Moderately', 'Very+')  
  pred.df$sound_var_temp = sound_vals
  
  names(pred.df)[names(pred.df) == 'sound_var_temp'] = sound_var
  
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
  
  print(g1)
}
