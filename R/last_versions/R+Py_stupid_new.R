source("/Users/ash/Desktop/MICE/R/H-G_all.R")

alltoge <- function(df){
  df <- df[which(df$estimator != 'ice_rf'), ]

  colnames(df)[4] <- 'res' 

  df$dist <- fct_rev(factor(df$dist, levels = c('norm', 'unif', 'lognorm1', 'lognorm2', 'gamma1', 'gamma2', 'norm_mix1', 'norm_mix2')))
  df$estimator <- fct_rev(factor(df$estimator, levels = c('missForest', 'ice_rf', 'rfcont10', 'mice_rf', 'micePMM', 'knn')))
  return(df)
}


# puki <- function(df_R, df_Py){
#   df_R <- df_R[which(df_R$dist != 'norm_mix1' & df_R$dist != 'norm_mix2'), ]
#   df_Py <- df_Py[which(df_Py$dist != 'norm_mix1' & df_Py$dist != 'norm_mix2'), ]
#   df_tog <- rbind(df_R, df_Py)
# 
#   return(df_tog)
# }

#Results from PY and R
#['lr_quadr', 'logr_quadr', 'lr_inter', 'logr_inter']

#Rel_bias
#----------
#---------1
relb_sc1 <- alltoge(read.csv("/Users/ash/Desktop/MICE/R/H-G csv/rel_bias_sc1_PY_R.csv"))
#relb_sc1[which(relb_sc1$dist == 'norm_mix1'), ]

rel_bias_sc1_pl <- plott(res = relb_sc1, sc_num = 1, metrica = 'rel_bias')
rel_bias_sc1_pl[[1]] + xlim(-0.18, 0.1)
rel_bias_sc1_pl[[2]] + xlim(-0.4, 0.26)



relb_sc1_BR <- read.csv("/Users/ash/Desktop/MICE/PY/fin_res_csv/miceBR_rel_bias_scen1.csv")
relb_sc1_BR[which(relb_sc1_BR$col == 'X^2'), ]$col = 'X2'
colnames(relb_sc1_BR)[4] <- 'res' 
relb_sc1_BR$dist <- fct_rev(factor(relb_sc1_BR$dist, levels = c('norm', 'unif', 'lognorm1', 'lognorm2', 'gamma1', 'gamma2', 'norm_mix1', 'norm_mix2')))
relb_sc1_BR_pl <- plott(res = relb_sc1_BR, sc_num = 1, metrica = 'rel_bias')
relb_sc1_BR_pl[[1]] 
relb_sc1_BR_pl[[2]] 

#---------2
relb_sc2 <- alltoge(read.csv("/Users/ash/Desktop/MICE/R/H-G csv/rel_bias_scen2_PY_R.csv"))

rel_bias_sc2_pl <- plott(res = relb_sc2, sc_num = 2, metrica = 'rel_bias')
rel_bias_sc2_pl[[1]] + xlim(-0.16, 0.22)
rel_bias_sc2_pl[[2]] + xlim(-0.26, 0.26)

#---------3
# relb_sc3 <- read.csv("/Users/ash/Desktop/MICE/R/H-G csv/old_version/rel_bias_scen3_100cy_old_mixNorm.csv")
# relb_sc3 <- relb_sc3[which(relb_sc3$dist != 'norm_mix1' & relb_sc3$dist != 'norm_mix2'), ]
# relb_sc3
# pukk <- read.csv("/Users/ash/Desktop/MICE/R/H-G csv/old_version/Normmixs_rel_bias_scen3.csv")
# 
# df_togg <- rbind(pukk[,-1], relb_sc3)
# df_togg
# write.csv(df_togg, "/Users/ash/Desktop/MICE/R/H-G csv/rel_bias_scen3_PY_R.csv", row.names = FALSE)
# 
# 
# relb_sc3 <- alltoge(read.csv("/Users/ash/Desktop/MICE/R/H-G csv/rel_bias_scen3_PY_R.csv"))
# 
# rel_bias_sc3_pl <- plott(res = relb_sc3, sc_num = 3, metrica = 'rel_bias')
# rel_bias_sc3_pl[[1]] + xlim(-0.2, 0.14)
# rel_bias_sc3_pl[[2]] + xlim(-0.16, 0.13)

relb_sc3_BR <- read.csv("/Users/ash/Desktop/MICE/PY/fin_res_csv/miceBR_rel_bias_scen3.csv")
colnames(relb_sc3_BR)[4] <- 'res' 
relb_sc3_BR$dist <- fct_rev(factor(relb_sc3_BR$dist, levels = c('norm', 'unif', 'lognorm1', 'lognorm2', 'gamma1', 'gamma2', 'norm_mix1', 'norm_mix2')))
relb_sc3_BR_pl <- plott(res = relb_sc3_BR, sc_num = 3, metrica = 'rel_bias')
relb_sc3_BR_pl[[1]] 
relb_sc3_BR_pl[[2]] 

#---------4
relb_sc4 <- read.csv("/Users/ash/Desktop/MICE/R/H-G csv/rel_bias_scen4_PY_R.csv")
rel_bias_scen4 <- alltoge(relb_sc4)

rel_bias_scen4_plot <- plott(res = rel_bias_scen4, sc_num = 4, metrica = 'rel_bias')
rel_bias_scen4_plot[[1]] + xlim(-0.16, 0.08)
rel_bias_scen4_plot[[2]] + xlim(-0.15, 0.08)



#Nrmse
#----------
#---------1
nmrse_sc1 <- alltoge(read.csv("/Users/ash/Desktop/MICE/R/H-G csv/nrmse_scen1_PY_R.csv"))

nmrse_scen1_plot <- plott(res = nmrse_sc1, sc_num = 1, metrica = 'nrmse')
nmrse_scen1_plot[[1]] + xlim(0, 1)

#---------2
nmrse_sc2 <- alltoge(read.csv("/Users/ash/Desktop/MICE/R/H-G csv/nrmse_scen2_PY_R.csv"))


nmrse_scen2_plot <- plott(res = nmrse_sc2, sc_num = 2, metrica = 'nrmse')
nmrse_scen2_plot[[1]] + xlim(0.2, 1.6)

#---------3
nmrse_sc3 <- alltoge(read.csv("/Users/ash/Desktop/MICE/R/H-G csv/nrmse_scen3_PY_R.csv"))

nmrse_scen3_plot <- plott(res = nmrse_scen3, sc_num = 3, metrica = 'nrmse')
nmrse_scen3_plot[[1]] + xlim(0, 1)

#---------4
nmrse_sc4 <- alltoge(read.csv("/Users/ash/Desktop/MICE/R/H-G csv/nrmse_scen4_PY_R.csv"))

nmrse_scen4_plot <- plott(res = nmrse_scen4, sc_num = 4, metrica = 'nrmse')
nmrse_scen4_plot[[1]] + xlim(0.4, 1.4)



#Rel_bias_coef
#----------
#---------1
relb_coef_scen1 <- alltoge(read.csv("/Users/ash/Desktop/MICE/R/H-G csv/relb_coef_scen1_PY_R.csv"))

relb_coef_scen1_plot <- plott(res = relb_coef_scen1, sc_num = 1, metrica = 'relb_coef')
relb_coef_scen1_plot[[1]] + xlim(-7.5, 2.5)
relb_coef_scen1_plot[[2]] + xlim(-1, 4.5)

#---------2
relb_coef_scen2 <- alltoge(read.csv("/Users/ash/Desktop/MICE/R/H-G csv/relb_coef_scen2_PY_R.csv"))

relb_coef_scen2_plot <- plott(res = relb_coef_scen2, sc_num = 2, metrica = 'relb_coef')
relb_coef_scen2_plot[[1]] + xlim(-25, 25)
relb_coef_scen2_plot[[2]] + xlim(-40, 20)

#---------3
relb_coef_scen3 <- alltoge(read.csv("/Users/ash/Desktop/MICE/R/H-G csv/relb_coef_scen3_PY_R.csv"))
relb_coef_scen3[which(relb_coef_scen3$dist == 'norm_mix1'), ]

relb_coef_scen3_plot <- plott(res = relb_coef_scen3, sc_num = 3, metrica = 'relb_coef')
relb_coef_scen3_plot[[1]] + xlim(-5, 3)
relb_coef_scen3_plot[[2]] + xlim(-1, 1.5)

#---------4
relb_coef_scen4 <- alltoge(read.csv("/Users/ash/Desktop/MICE/R/H-G csv/relb_coef_scen4_PY_R.csv"))

relb_coef_scen4_plot <- plott(res = relb_coef_scen4, sc_num = 4, metrica = 'relb_coef')
relb_coef_scen4_plot[[1]] + xlim(-2, 4)
relb_coef_scen4_plot[[2]] + xlim(-2.5, 7.5)
