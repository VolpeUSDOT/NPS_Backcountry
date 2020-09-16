library(tidyverse) # for tidyr::pivot_wider and ggplot
library(ordinal)

# Dependency: pred_clmm function
pred_clmm <- function(eta, theta, cat = 1:(length(theta)+1), inv.link = plogis) {
  Theta <- c(-1e3, theta, 1e3)
  sapply(cat, function(j)
    inv.link(Theta[j+1] - eta) - inv.link(Theta[j] - eta) )
}

# Function content
# model_object_name = 'm4'; plot_se = TRUE

plot_curves = function(model_object_name, plot_se = TRUE){

  model_object = get(model_object_name)
  
  stopifnot(class(model_object) %in% 'clmm')
  
  # Name of response?
  resp_name = names(model_object$model)[1]
  
  # Has sitetype?
  has_sitetype = 'SiteType' %in% names(model_object$model)
  
  # Name of sound variable (leq vs SEL)
  has_SEL = 'SELAllAC' %in% names(model_object$model)
  has_Leq = 'LeqAllAC' %in% names(model_object$model)
  
  sound_var = ifelse(has_SEL, 'SELAllAC',
                     ifelse(has_Leq, 'LeqAllAC', NA))
  
  # Has PTAudAllAC, lg10.PTAudAllAC, or DurVisitMinutes: continuous  
  has_PTAudAllAC = 'PTAudAllAC' %in% names(model_object$model)
  has_lg10.PTAudAllAC = 'lg10.PTAudAllAC' %in% names(model_object$model)
  has_DurVisitMinutes = 'DurVisitMinutes' %in% names(model_object$model)
  
  # AdultsOnly or ImpNQ_VorMore: factor
  has_AdultsONly = 'AdultsOnly' %in% names(model_object$model)
  has_ImpNQ_VorMore = 'ImpNQ_VorMore' %in% names(model_object$model)
  
  if(has_AdultsONly | has_ImpNQ_VorMore) { stop('AdultsOnly and ImpNQ_VorMore not yet implemented')}
  
  if(sound_var == 'SELAllAC'){
    sound_vals = seq(35, 100, by = 1)
  }
  if(sound_var == 'LeqAllAC'){
    sound_vals = seq(-1, 65, by = 1)
  }
  
  std_errs = summary(model_object)$coefficients[,'Std. Error']
  
  if(!has_sitetype){
    mat = expand.grid(Site = sd(summary(model_object)$ranef),
                     sound_var_temp = model_object$beta[1] * sound_vals,
                     PEnProps = model_object$beta[names(model_object$beta) == 'PEnProps'] * median(dC$PEnProps),
                     PEnHelos = model_object$beta[names(model_object$beta) == 'PEnHelos'] * median(dC$PEnHelos))
    
    lo_mat = expand.grid(Site = min(summary(model_object)$ranef),
                          sound_var_temp = model_object$beta[1] * sound_vals,
                          PEnProps = model_object$beta[names(model_object$beta) == 'PEnProps'] * median(dC$PEnProps) - std_errs[names(std_errs) == 'PEnProps'],
                          PEnHelos = model_object$beta[names(model_object$beta) == 'PEnHelos'] * median(dC$PEnHelos)- std_errs[names(std_errs) == 'PEnHelos'])
    
    hi_mat = expand.grid(Site = max(summary(model_object)$ranef),
                          sound_var_temp = model_object$beta[1] * sound_vals,
                          PEnProps = model_object$beta[names(model_object$beta) == 'PEnProps'] * median(dC$PEnProps) + std_errs[names(std_errs) == 'PEnProps'],
                         PEnHelos = model_object$beta[names(model_object$beta) == 'PEnHelos'] * median(dC$PEnHelos) + std_errs[names(std_errs) == 'PEnHelos'])
    
  }
  if(has_sitetype){
    mat = expand.grid(Site = sd(summary(model_object)$ranef),
                      sound_var_temp = model_object$beta[1] * sound_vals,
                      PEnProps = model_object$beta[names(model_object$beta) == 'PEnProps'] * median(dC$PEnProps),
                      PEnHelos = model_object$beta[names(model_object$beta) == 'PEnHelos'] * median(dC$PEnHelos),
                      SiteTypeDayHike = c(0, model_object$beta[names(model_object$beta) == 'SiteTypeDayHike']),
                      SiteTypeOverlook = c(0, model_object$beta[names(model_object$beta) == 'SiteTypeOverlook']),
                      SiteTypeShortHike = c(0, model_object$beta[names(model_object$beta) == 'SiteTypeShortHike'])
                      )
    

    lo_mat = expand.grid(Site = sd(summary(model_object)$ranef) * qnorm(.95) * -1,
                         sound_var_temp = model_object$beta[1] * sound_vals,
                         PEnProps = model_object$beta[names(model_object$beta) == 'PEnProps'] * median(dC$PEnProps) - std_errs[names(std_errs) == 'PEnProps'],
                         PEnHelos = model_object$beta[names(model_object$beta) == 'PEnHelos'] * median(dC$PEnHelos) - std_errs[names(std_errs) == 'PEnHelos'],
                         SiteTypeDayHike = c(0, model_object$beta[names(model_object$beta) == 'SiteTypeDayHike']),
                         SiteTypeOverlook = c(0, model_object$beta[names(model_object$beta) == 'SiteTypeOverlook']),
                         SiteTypeShortHike = c(0, model_object$beta[names(model_object$beta) == 'SiteTypeShortHike']))
    
    hi_mat = expand.grid(Site = sd(summary(model_object)$ranef) * qnorm(.95) * 1,
                         sound_var_temp = model_object$beta[1] * sound_vals,
                         PEnProps = model_object$beta[names(model_object$beta) == 'PEnProps'] * median(dC$PEnProps) + std_errs[names(std_errs) == 'PEnProps'],
                         PEnHelos = model_object$beta[names(model_object$beta) == 'PEnHelos'] * median(dC$PEnHelos) + std_errs[names(std_errs) == 'PEnHelos'],
                         SiteTypeDayHike = c(0, model_object$beta[names(model_object$beta) == 'SiteTypeDayHike']),
                         SiteTypeOverlook = c(0, model_object$beta[names(model_object$beta) == 'SiteTypeOverlook']),
                         SiteTypeShortHike = c(0, model_object$beta[names(model_object$beta) == 'SiteTypeShortHike']))
    
    # only keep rows where one site type is included
    keep <- apply(mat[,c("SiteTypeDayHike", "SiteTypeOverlook", "SiteTypeShortHike")],
                  1,
                  function(x) sum(x == 0) >= 2)
    
    mat <- mat[keep,]
    rownames(mat) = 1:nrow(mat)
    
    keep <- apply(lo_mat[,c("SiteTypeDayHike", "SiteTypeOverlook", "SiteTypeShortHike")],
                  1,
                  function(x) sum(x == 0) >= 2)
    
    lo_mat <- lo_mat[keep,]
    rownames(lo_mat) = 1:nrow(lo_mat)
    
    keep <- apply(hi_mat[,c("SiteTypeDayHike", "SiteTypeOverlook", "SiteTypeShortHike")],
                  1,
                  function(x) sum(x == 0) >= 2)
    
    hi_mat <- hi_mat[keep,]
    rownames(hi_mat) = 1:nrow(hi_mat)
  }
  
  if(has_PTAudAllAC) { 
    mat[ncol(mat)+1] = model_object$beta[names(model_object$beta) == 'PTAudAllAC'] * median(dC$PTAudAllAC)
    lo_mat[ncol(lo_mat)+1] = model_object$beta[names(model_object$beta) == 'PTAudAllAC'] * median(dC$PTAudAllAC)
    hi_mat[ncol(hi_mat)+1] = model_object$beta[names(model_object$beta) == 'PTAudAllAC'] * median(dC$PTAudAllAC)
  }
  if(has_lg10.PTAudAllAC) { 
    mat[ncol(mat)+1] = model_object$beta[names(model_object$beta) == 'lg10.PTAudAllAC'] * median(dC$lg10.PTAudAllAC)
    lo_mat[ncol(lo_mat)+1] = model_object$beta[names(model_object$beta) == 'lg10.PTAudAllAC'] * median(dC$lg10.PTAudAllAC)
    hi_mat[ncol(hi_mat)+1] = model_object$beta[names(model_object$beta) == 'lg10.PTAudAllAC'] * median(dC$lg10.PTAudAllAC)
    
  }
  if(has_DurVisitMinutes) {
    mat[ncol(mat)+1] = model_object$beta[names(model_object$beta) == 'DurVisitMinutes'] * median(dC$DurVisitMinutes)
    lo_mat[ncol(lo_mat)+1] = model_object$beta[names(model_object$beta) == 'DurVisitMinutes'] * median(dC$DurVisitMinutes)
    hi_mat[ncol(hi_mat)+1] = model_object$beta[names(model_object$beta) == 'DurVisitMinutes'] * median(dC$DurVisitMinutes)
  }
  
  
  names(mat)[names(mat) == 'sound_var_temp'] = sound_var
  names(lo_mat)[names(lo_mat) == 'sound_var_temp'] = sound_var
  names(hi_mat)[names(hi_mat) == 'sound_var_temp'] = sound_var
  
  # thetas: transition thresholds
  
  pred.mat <- pred_clmm(eta = rowSums(mat), theta = model_object$Theta)
  
  pred.mat.lo <- pred_clmm(eta = rowSums(lo_mat), theta = model_object$Theta)
  pred.mat.hi <- pred_clmm(eta = rowSums(hi_mat), theta = model_object$Theta)
  
  
  # Plot
  
  pred.df <- as.data.frame(pred.mat)
  colnames(pred.df) = c('NotAtAll', 'Somewhat', 'Moderately', 'Very+')  
  pred.df$sound_var_temp = sound_vals
  names(pred.df)[names(pred.df) == 'sound_var_temp'] = sound_var
  
  pred.df.lo <- as.data.frame(pred.mat.lo)
  colnames(pred.df.lo) = c('NotAtAll', 'Somewhat', 'Moderately', 'Very+')  
  pred.df.lo$sound_var_temp = sound_vals
  names(pred.df.lo)[names(pred.df.lo) == 'sound_var_temp'] = sound_var
  
  pred.df.hi <- as.data.frame(pred.mat.hi)
  colnames(pred.df.hi) = c('NotAtAll', 'Somewhat', 'Moderately', 'Very+')  
  pred.df.hi$sound_var_temp = sound_vals
  names(pred.df.hi)[names(pred.df.hi) == 'sound_var_temp'] = sound_var
  
  
  # Reformat long
  if(!has_sitetype){
    pred_long = pred.df %>%
      pivot_longer(cols = c('NotAtAll', 'Somewhat', 'Moderately', 'Very+'),
                   names_to = 'Level',
                   values_to = resp_name)
    
    pred_long_lo = pred.df.lo %>%
      pivot_longer(cols = c('NotAtAll', 'Somewhat', 'Moderately', 'Very+'),
                   names_to = 'Level',
                   values_to = paste0(resp_name, '_lo'))
    
    
    pred_long_hi = pred.df.hi %>%
      pivot_longer(cols = c('NotAtAll', 'Somewhat', 'Moderately', 'Very+'),
                   names_to = 'Level',
                   values_to = paste0(resp_name, '_hi'))
    
    pred_long = pred_long %>%
        left_join(pred_long_lo) %>%
        left_join(pred_long_hi)
    
    
    g1 <- ggplot(pred_long,
                 aes(x = get(sound_var),
                     y = get(resp_name),
                     col = Level)) + 
      geom_line(size = 2) + 
      theme_bw() +
      xlab(sound_var) + ylab(resp_name) +
      ggtitle(paste(model_object_name, resp_name, sound_var))
    
  
    
  }
  if(has_sitetype){
    pred_long = pred.df %>%
      mutate(SiteType = rep(c('Backcountry', 'DayHike', 'Overlook', 'ShortHike'), each = length(sound_vals))) %>%
      pivot_longer(cols = c('NotAtAll', 'Somewhat', 'Moderately', 'Very+'),
                   names_to = 'Level',
                   values_to = resp_name)
    
    
    pred_long_lo = pred.df.lo %>%
      mutate(SiteType = rep(c('Backcountry', 'DayHike', 'Overlook', 'ShortHike'), each = length(sound_vals))) %>%
      pivot_longer(cols = c('NotAtAll', 'Somewhat', 'Moderately', 'Very+'),
                   names_to = 'Level',
                   values_to = paste0(resp_name, '_lo'))
    
    
    pred_long_hi = pred.df.hi %>%
      mutate(SiteType = rep(c('Backcountry', 'DayHike', 'Overlook', 'ShortHike'), each = length(sound_vals))) %>%
      pivot_longer(cols = c('NotAtAll', 'Somewhat', 'Moderately', 'Very+'),
                   names_to = 'Level',
                   values_to = paste0(resp_name, '_hi'))
    
    pred_long = pred_long %>%
      left_join(pred_long_lo) %>%
      left_join(pred_long_hi)
    
    g1 <- ggplot(pred_long,
                 aes(x = get(sound_var),
                     y = get(resp_name),
                     col = Level)) + 
      geom_line(size = 2) + 
      theme_bw() +
      xlab(sound_var) + ylab(resp_name) +
      ggtitle(paste(model_object_name, resp_name, sound_var)) +
      facet_wrap(~SiteType)
  }
  
  if(plot_se) {
    g1 + geom_ribbon(aes(ymin = get(paste0(resp_name, '_lo')), 
                         ymax = get(paste0(resp_name, '_hi'))
    ),
    fill = pred_long$Level,
    alpha = 0.1)
    
    g1 = g1 + geom_ribbon(aes(ymin = Annoy3_lo, 
                              ymax = Annoy3_hi), alpha = 0.05)
    
  }
  
  print(g1)
}
