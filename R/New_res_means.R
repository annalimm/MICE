source("/Users/ash/Desktop/MICE/R/H-G_all.R")

sett <- function(df){
  colnames(df)[4] <- 'res'
  df$dist <- fct_rev(factor(df$dist, levels = c('norm', 'unif', 'lognorm1', 'lognorm2', 'gamma1', 'gamma2', 'norm_mix1', 'norm_mix2')))
  df$estimator <- fct_rev(factor(df$estimator, levels = c('missForest', 'micePMM', 'mean')))
  return(df)
}

# mean_values <- function(df){
#   return(list(
#     paste('mean: mean:', mean(abs(df[which(df$estimator == 'mean'), ]$res)) * 100),
#     paste('mean: sd:', ci.sd(abs(df[which(df$estimator == 'mean'), ]$res), level = 0.95, na.rm = TRUE) * 100),
#     paste('micePMM: mean:', mean(abs(df[which(df$estimator == 'micePMM'), ]$res)) * 100),
#     paste('micePMM: sd:', ci.sd(abs(df[which(df$estimator == 'micePMM'), ]$res), level = 0.95, na.rm = TRUE) * 100),
#     paste('missForest: mean:', mean(abs(df[which(df$estimator == 'missForest'), ]$res)) * 100),
#     paste('missForest: sd:',ci.sd(abs(df[which(df$estimator == 'missForest'), ]$res), level = 0.95, na.rm = TRUE) * 100)))
# }

mean_values <- function(df){
  return(list(
    paste('mean: mean:', mean(abs(df[which(df$estimator == 'mean'), ]$res)), 'sd: ', sd(abs(df[which(df$estimator == 'mean'), ]$res))) ,
    paste('micePMM: mean:', mean(abs(df[which(df$estimator == 'micePMM'), ]$res)), 'sd: ', sd(abs(df[which(df$estimator == 'micePMM'), ]$res))),
    paste('missForest: mean:', mean(abs(df[which(df$estimator == 'missForest'), ]$res)), 'sd: ', sd(abs(df[which(df$estimator == 'missForest'), ]$res)))
    ))
}

#----nrmse
funn <- function(df, n){
  df <- sett(df)
  
  df_plot <- plott(res = df, sc_num = n, metrica = 'nrmse')
  df_plot[[1]] #+ xlim(0, 1)
  
  return(list(mean_values(df), df_plot[[1]]))
}
#-----------R
nmrse_scen1_R <- read.csv("/Users/ash/Desktop/MICE/R/H-G csv/New_EB/R_nrmse_sc1.csv")
funn(nmrse_scen1_R, 1)


nmrse_scen2_R <- read.csv("/Users/ash/Desktop/MICE/R/H-G csv/New_EB/R_nrmse_sc2.csv")
funn(nmrse_scen2_R, 2)


nmrse_scen3_R <- read.csv("/Users/ash/Desktop/MICE/R/H-G csv/New_EB/R_nrmse_sc3.csv")
funn(nmrse_scen3_R, 3)


nmrse_scen4_R <- read.csv("/Users/ash/Desktop/MICE/R/H-G csv/New_EB/R_nrmse_sc4.csv")
funn(nmrse_scen4_R, 4)



#----rel_bias_X
fun_relb_X <- function(df, n){
  df <- sett(df)
  
  df_plot <- plott(res = df, sc_num = n, metrica = 'rel_bias')
  
  return(list(mean_values(df[df$col == 'X',]), df_plot[[1]]))
}
#-----------R
relb_sc1_R <- read.csv("/Users/ash/Desktop/MICE/R/H-G csv/New_EB/R_rel_bias_sc1.csv")
relb_sc1_R
fun_relb_X(relb_sc1_R, 1)

relb_sc2_R <- read.csv("/Users/ash/Desktop/MICE/R/H-G csv/New_EB/R_rel_bias_sc2.csv")
fun_relb_X(relb_sc2_R, 2)

relb_sc3_R <- read.csv("/Users/ash/Desktop/MICE/R/H-G csv/New_EB/R_rel_bias_sc3.csv")
fun_relb_X(relb_sc3_R, 3)

relb_sc4_R <- read.csv("/Users/ash/Desktop/MICE/R/H-G csv/New_EB/R_rel_bias_sc4.csv")
fun_relb_X(relb_sc4_R, 4)



#----rel_bias_X2/XZ
#-----------R
relb_sc1_R <- read.csv("/Users/ash/Desktop/MICE/R/H-G csv/New_EB/R_rel_bias_sc1.csv")
mean_values(sett(relb_sc1_R[relb_sc1_R$col == 'X2',]))


relb_sc2_R <- read.csv("/Users/ash/Desktop/MICE/R/H-G csv/New_EB/R_rel_bias_sc2.csv")
relb_sc2_R
mean_values(sett(relb_sc2_R[relb_sc2_R$col == 'X2',]))

relb_sc3_R <- read.csv("/Users/ash/Desktop/MICE/R/H-G csv/New_EB/R_rel_bias_sc3.csv")
mean_values(sett(relb_sc3_R[relb_sc3_R$col == 'XZ',]))

relb_sc4_R <- read.csv("/Users/ash/Desktop/MICE/R/H-G csv/New_EB/R_rel_bias_sc4.csv")
mean_values(sett(relb_sc4_R[relb_sc4_R$col == 'XZ',]))




#----relb_coef__X
fun_relb_coef_X <- function(df, n){
  df <- sett(df)
  
  df_plot <- plott(res = df, sc_num = n, metrica = 'relb_coef')
  
  return(list(mean_values(df[df$col == 'X',]), df_plot[[1]]))
}
#-----------R
relb_coef_sc1_R <- read.csv("/Users/ash/Desktop/MICE/R/H-G csv/New_EB/R_relb_coef_sc1.csv")
fun_relb_coef_X(relb_coef_sc1_R, 1)

relb_coef_sc2_R <- read.csv("/Users/ash/Desktop/MICE/R/H-G csv/New_EB/R_relb_coef_sc2.csv")
fun_relb_coef_X(relb_coef_sc2_R, 2)

relb_coef_sc3_R <- read.csv("/Users/ash/Desktop/MICE/R/H-G csv/New_EB/R_relb_coef_sc3.csv")
fun_relb_coef_X(relb_coef_sc3_R, 3)

relb_coef_sc4_R <- read.csv("/Users/ash/Desktop/MICE/R/H-G csv/New_EB/R_relb_coef_sc4.csv")
fun_relb_coef_X(relb_coef_sc4_R, 4)


#----relb_coef_X2/XZ
#-----------R
mean_values(sett(relb_coef_sc1_R[relb_coef_sc1_R$col == 'X2',]))
mean_values(sett(relb_coef_sc2_R[relb_coef_sc2_R$col == 'X2',]))
mean_values(sett(relb_coef_sc3_R[relb_coef_sc3_R$col == 'XZ',]))
mean_values(sett(relb_coef_sc4_R[relb_coef_sc4_R$col == 'XZ',]))

