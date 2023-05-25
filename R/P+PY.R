source("/Users/ash/Desktop/MICE/R/H-G_all.R")

# nrmse 7.21x3.90 else 7.21x6.66

alltoge <- function(df_R, df_PY){
  #df_PY <- df_PY[which(df_PY$estimator != 'ice_rf'), ]
  
  df_tog <- rbind(df_R, df_PY)
  colnames(df_tog)[4] <- 'res' 
  #df_tog$dist <- fct_rev(factor(df_tog$dist, levels = c('norm', 'unif', 'lognorm1', 'lognorm2', 'gamma1', 'gamma2', 'norm_mix1', 'norm_mix2')))
  #df_tog$estimator <- fct_rev(factor(df_tog$estimator, levels = c('knn', 'miceRF','miceBR','micePMM','missForest')))
  return(df_tog)
}

sett <- function(df){
  #df <- df[which(df$estimator != 'ice_rf'), ]
  #df <- df[df$estimator != 'rfcont10',]
  
  df[df$estimator == 'mice_rf',]$estimator <- 'miceRF'
  df[df$estimator == 'mice_br',]$estimator <- 'miceRidge'
  
  colnames(df)[4] <- 'res'
  df$dist <- fct_rev(factor(df$dist, levels = c('norm', 'unif', 'lognorm1', 'lognorm2', 'gamma1', 'gamma2', 'norm_mix1', 'norm_mix2')))
  df$estimator <- fct_rev(factor(df$estimator, levels = c('knn', 'miceRF','miceRidge','micePMM','missForest')))
  return(df)
}

#Results from PY and R
#['lr_quadr', 'logr_quadr', 'lr_inter', 'logr_inter']

#Rel_bias
#----------
new_df_mixnorms <- function(old_df, new_df_PY){
  colnames(new_df_PY)[4] <- 'res' 
  old <- subset(old_df, !(dist %in% c("norm_mix1", "norm_mix2") & estimator == 'knn'))
  new_df <- rbind( old , new_df_PY )
  
  return(new_df)
}

mean_values <- function(df){
  return(list(
    paste('missForest: mean:', mean(abs(df[which(df$estimator == 'missForest'), ]$res)), 'sd: ', sd(abs(df[which(df$estimator == 'missForest'), ]$res))),
  #mean(abs(df[which(df$estimator == 'rfcont10'), ]$res)) * 100,
    paste('micePMM: mean:', mean(abs(df[which(df$estimator == 'micePMM'), ]$res)), 'sd: ', sd(abs(df[which(df$estimator == 'micePMM'), ]$res))),
    paste('miceRF: mean:', mean(abs(df[which(df$estimator == 'mice_rf'), ]$res)), 'sd: ', sd(abs(df[which(df$estimator == 'mice_rf'), ]$res))),
    paste('miceRidge: mean:', mean(abs(df[which(df$estimator == 'mice_br'), ]$res)), 'sd: ', sd(abs(df[which(df$estimator == 'mice_br'), ]$res))),
    paste('knn: mean:', mean(abs(df[which(df$estimator == 'knn'), ]$res)), 'sd: ', sd(abs(df[which(df$estimator == 'knn'), ]$res)))))
}

#---------1
#-------MAR
relb_sc1 <- read.csv("/Users/ash/Desktop/MICE/R/H-G csv/rel_bias_sc1_PY_R.csv")
#relb_sc1_plot <- plott(res = sett(relb_sc1), sc_num = 1, metrica = 'rel_bias')
#relb_sc1_plot[[1]] + xlim(-0.18, 0.1)
#relb_sc1_plot[[2]] + xlim(-0.4, 0.26)

relb_sc1_BR <- read.csv("/Users/ash/Desktop/MICE/PY/fin_res_csv/miceBR_rel_bias_scen1.csv")
relb_sc1_BR[which(relb_sc1_BR$col == 'X^2'), ]$col = 'X2'
colnames(relb_sc1_BR)[4] <- 'res' 
new_relb_sc1 <- rbind(relb_sc1[which(relb_sc1$estimator != 'mice_rf'), ], relb_sc1_BR)

knn_mxnorms_relb_scen1 <- read.csv("/Users/ash/Desktop/MICE/PY/fin_res_csv/knn_relb_scen1.csv")
knn_mxnorms_relb_scen1[which(knn_mxnorms_relb_scen1$col == 'X^2'), ]$col = 'X2'
new2_relb_sc1 <- new_df_mixnorms(new_relb_sc1, knn_mxnorms_relb_scen1)

new_relb_sc1_plot <- plott(res = sett(new2_relb_sc1)[sett(new2_relb_sc1)$estimator != 'miceRidge',], sc_num = 1, metrica = 'rel_bias')
new_relb_sc1_plot[[1]] #+ xlim(-0.18, 0.1)
new_relb_sc1_plot[[2]] + xlim(-0.4, 0.26)

mean_values(new2_relb_sc1[which(new2_relb_sc1$col == 'X'), ])
mean_values(new2_relb_sc1[which(new2_relb_sc1$col == 'X2'), ])


#-------MAR 0.45
relb_sc1_R <- read.csv("/Users/ash/Desktop/MICE/R/H-G csv/0.45_miss/rel_bias_sc1.csv")
relb_sc1_PY <- read.csv("/Users/ash/Desktop/MICE/PY/fin_res_csv/0.45_miss/rel_bias_sc1.csv")
relb_sc1_PY[which(relb_sc1_PY$col == 'X^2'), ]$col = 'X2'

relb_sc1 <- alltoge(relb_sc1_R, relb_sc1_PY)
#sett(relb_sc1)[sett(relb_sc1)$estimator == 'miceRidge',]

mean_values(relb_sc1[which(relb_sc1$col == 'X'), ])
mean_values(relb_sc1[which(relb_sc1$col == 'X2'), ])

#-------MNAR
MNAR_relb_sc1_R <- read.csv("/Users/ash/Desktop/MICE/PY/fin_res_csv/MNAR_rel_bias_scen1.csv")
MNAR_relb_sc1_PY <- read.csv("/Users/ash/Desktop/MICE/PY/fin_res_csv/MNAR_relb_sc1.csv")
MNAR_relb_sc1_PY[which(MNAR_relb_sc1_PY$col == 'X^2'), ]$col = 'X2'
MNAR_relb_sc1 <- alltoge(MNAR_relb_sc1_R, MNAR_relb_sc1_PY[MNAR_relb_sc1_PY$estimator != 'mice_br',])

fin_plot <- plott(res = MNAR_relb_sc1, sc_num = 1, metrica = 'rel_bias')
fin_plot[[1]] + xlim(-0.18, 0.1)
fin_plot[[2]] + xlim(-0.4, 0.26)

mean_values(MNAR_relb_sc1[which(MNAR_relb_sc1$col == 'X'), ])
mean_values(MNAR_relb_sc1[which(MNAR_relb_sc1$col == 'X2'), ])

#---------2
relb_sc2 <- read.csv("/Users/ash/Desktop/MICE/R/H-G csv/rel_bias_sc2_PY_R.csv")
# relb_sc2_plot <- plott(res = sett(relb_sc2), sc_num = 2, metrica = 'rel_bias')
# relb_sc2_plot[[1]] + xlim(-0.16, 0.22)
# relb_sc2_plot[[2]] + xlim(-0.26, 0.26)

relb_sc2_BR <- read.csv("/Users/ash/Desktop/MICE/PY/fin_res_csv/miceBR_rel_bias_scen2.csv")
relb_sc2_BR[which(relb_sc2_BR$col == 'X^2'), ]$col = 'X2'
colnames(relb_sc2_BR)[4] <- 'res' 
new_relb_sc2 <- rbind(relb_sc2[which(relb_sc2$estimator != 'mice_rf'), ], relb_sc2_BR)


knn_mxnorms_relb_scen2 <- read.csv("/Users/ash/Desktop/MICE/PY/fin_res_csv/knn_relb_scen2.csv")
knn_mxnorms_relb_scen2[which(knn_mxnorms_relb_scen2$col == 'X^2'), ]$col = 'X2'
new2_relb_sc2 <- new_df_mixnorms(new_relb_sc2, knn_mxnorms_relb_scen2)

new_relb_sc2_plot <- plott(res = sett(new2_relb_sc2), sc_num = 2, metrica = 'rel_bias')
new_relb_sc2_plot[[1]] + xlim(-0.16, 0.22)
new_relb_sc2_plot[[2]] + xlim(-0.26, 0.26)

mean_values(new2_relb_sc2[which(new2_relb_sc2$col == 'X'), ])
mean_values(new2_relb_sc2[which(new2_relb_sc2$col == 'X2'), ])


#-------MAR 0.45
relb_sc2_R <- read.csv("/Users/ash/Desktop/MICE/R/H-G csv/0.45_miss/rel_bias_sc2.csv")
relb_sc2_PY <- read.csv("/Users/ash/Desktop/MICE/PY/fin_res_csv/0.45_miss/rel_bias_sc2.csv")
relb_sc2_PY[which(relb_sc2_PY$col == 'X^2'), ]$col = 'X2'

relb_sc2 <- alltoge(relb_sc2_R, relb_sc2_PY)
relb_sc2
relb_sc2_plot <- plott(res = sett(relb_sc2), sc_num = 2, metrica = 'rel_bias')
relb_sc2_plot[[1]] #+ xlim(-0.16, 0.22)

mean_values(relb_sc2[which(relb_sc2$col == 'X'), ])
mean_values(relb_sc2[which(relb_sc2$col == 'X2'), ])


#-------MNAR
MNAR_relb_sc2_R <- read.csv("/Users/ash/Desktop/MICE/PY/fin_res_csv/MNAR_rel_bias_scen2.csv")
MNAR_relb_sc2_PY <- read.csv("/Users/ash/Desktop/MICE/PY/fin_res_csv/MNAR_relb_sc2.csv")
MNAR_relb_sc2_PY[which(MNAR_relb_sc2_PY$col == 'X^2'), ]$col = 'X2'
MNAR_relb_sc2 <- alltoge(MNAR_relb_sc2_R, MNAR_relb_sc2_PY)

fin_plot_2 <- plott(res = MNAR_relb_sc2, sc_num = 2, metrica = 'rel_bias')
fin_plot_2[[1]] #+ xlim(0.6, 1.6)
fin_plot_2[[2]]

mean_values(MNAR_relb_sc2[which(MNAR_relb_sc2$col == 'X'), ])
mean_values(MNAR_relb_sc2[which(MNAR_relb_sc2$col == 'X2'), ])

#---------3
relb_sc3 <- read.csv("/Users/ash/Desktop/MICE/R/H-G csv/rel_bias_sc3_PY_R.csv")

# relb_sc3_plot <- plott(res = sett(relb_sc3), sc_num = 3, metrica = 'rel_bias')
# relb_sc3_plot[[1]] + xlim(-0.2, 0.14)
# relb_sc3_plot[[2]] + xlim(-0.16, 0.13)

relb_sc3_BR <- read.csv("/Users/ash/Desktop/MICE/PY/fin_res_csv/miceBR_rel_bias_scen3.csv")
colnames(relb_sc3_BR)[4] <- 'res' 
new_relb_sc3 <- rbind(relb_sc3[which(relb_sc3$estimator != 'mice_rf'), ], relb_sc3_BR)


knn_mxnorms_relb_scen3 <- read.csv("/Users/ash/Desktop/MICE/PY/fin_res_csv/knn_relb_scen3.csv")
new2_relb_sc3 <- new_df_mixnorms(new_relb_sc3, knn_mxnorms_relb_scen3)

new_relb_sc3_plot <- plott(res = sett(new2_relb_sc3)[sett(new2_relb_sc3)$estimator != 'miceRidge',], sc_num = 3, metrica = 'rel_bias')
new_relb_sc3_plot[[1]] #+ xlim(-0.2, 0.14)
new_relb_sc3_plot[[2]] + xlim(-0.16, 0.13)

mean_values(new2_relb_sc3[which(new2_relb_sc3$col == 'X'), ])
mean_values(new2_relb_sc3[which(new2_relb_sc3$col == 'XZ'), ])


#-------MAR 0.45
relb_sc3_R <- read.csv("/Users/ash/Desktop/MICE/R/H-G csv/0.45_miss/rel_bias_sc3.csv")
relb_sc3_PY <- read.csv("/Users/ash/Desktop/MICE/PY/fin_res_csv/0.45_miss/rel_bias_sc3.csv")

relb_sc3 <- alltoge(relb_sc3_R, relb_sc3_PY)

mean_values(relb_sc3[which(relb_sc3$col == 'X'), ])
mean_values(relb_sc3[which(relb_sc3$col == 'XZ'), ])


#-------MNAR
MNAR_relb_sc3_R <- read.csv("/Users/ash/Desktop/MICE/PY/fin_res_csv/MNAR_rel_bias_scen3.csv")
MNAR_relb_sc3_PY <- read.csv("/Users/ash/Desktop/MICE/PY/fin_res_csv/MNAR_relb_sc3.csv")
MNAR_relb_sc3 <- alltoge(MNAR_relb_sc3_R, MNAR_relb_sc3_PY[MNAR_relb_sc3_PY$estimator != 'mice_br',])

fin_plot <- plott(res = MNAR_relb_sc3, sc_num = 3, metrica = 'rel_bias')
fin_plot[[1]] + xlim(-0.2, 0.14)
fin_plot[[2]] + xlim(-0.16, 0.13)

mean_values(MNAR_relb_sc3[which(MNAR_relb_sc3$col == 'X'), ])
mean_values(MNAR_relb_sc3[which(MNAR_relb_sc3$col == 'XZ'), ])

#---------4
relb_sc4 <- read.csv("/Users/ash/Desktop/MICE/R/H-G csv/rel_bias_sc4_PY_R.csv")
# relb_sc4_plot <- plott(res = sett(relb_sc4), sc_num = 4, metrica = 'rel_bias')
# relb_sc4_plot[[1]] + xlim(-0.16, 0.08)
# relb_sc4_plot[[2]] + xlim(-0.15, 0.08)

relb_sc4_BR <- read.csv("/Users/ash/Desktop/MICE/PY/fin_res_csv/miceBR_rel_bias_scen4.csv")
colnames(relb_sc4_BR)[4] <- 'res' 
new_relb_sc4 <- rbind(relb_sc4[which(relb_sc4$estimator != 'mice_rf'), ], relb_sc4_BR)


knn_mxnorms_relb_scen4 <- read.csv("/Users/ash/Desktop/MICE/PY/fin_res_csv/knn_relb_scen4.csv")
new2_relb_sc4 <- new_df_mixnorms(new_relb_sc4, knn_mxnorms_relb_scen4)

new_relb_sc4_plot <- plott(res = sett(new2_relb_sc4), sc_num = 4, metrica = 'rel_bias')
new_relb_sc4_plot[[1]] + xlim(-0.16, 0.08)
new_relb_sc4_plot[[2]] + xlim(-0.15, 0.08)

mean_values(new2_relb_sc4[which(new2_relb_sc4$col == 'X'), ])
mean_values(new2_relb_sc4[which(new2_relb_sc4$col == 'XZ'), ])


#-------MAR 0.45
relb_sc4_R <- read.csv("/Users/ash/Desktop/MICE/R/H-G csv/0.45_miss/rel_bias_sc4.csv")
relb_sc4_PY <- read.csv("/Users/ash/Desktop/MICE/PY/fin_res_csv/0.45_miss/rel_bias_sc4.csv")

relb_sc4 <- alltoge(relb_sc4_R, relb_sc4_PY)

mean_values(relb_sc4[which(relb_sc4$col == 'X'), ])
mean_values(relb_sc4[which(relb_sc4$col == 'XZ'), ])

#-------MNAR
MNAR_relb_sc4_R <- read.csv("/Users/ash/Desktop/MICE/PY/fin_res_csv/MNAR_rel_bias_scen4.csv")
MNAR_relb_sc4_PY <- read.csv("/Users/ash/Desktop/MICE/PY/fin_res_csv/MNAR_relb_sc4.csv")
MNAR_relb_sc4 <- alltoge(MNAR_relb_sc4_R, MNAR_relb_sc4_PY)

fin_plot_4 <- plott(res = MNAR_relb_sc4, sc_num = 4, metrica = 'rel_bias')
fin_plot_4[[1]] #+ xlim(0.6, 1.6)
fin_plot_4[[2]]

mean_values(MNAR_relb_sc4[which(MNAR_relb_sc4$col == 'X'), ])
mean_values(MNAR_relb_sc4[which(MNAR_relb_sc4$col == 'XZ'), ])


#----------------------------------
#----------------------------------
#Nrmse
#----------
#----------------------------------
new_nrmse <- function(old_df, new_df_PY){
  colnames(new_df_PY)[4] <- 'res' 
  old <- subset(old_df, !(dist %in% c("norm_mix1", "norm_mix2") & estimator %in% c('knn', 'mice_rf')))
  new_df <- rbind( old , new_df_PY )
  
  return(new_df)
}

new_nrmse2 <- function(old_df, new_df_PY){
  colnames(new_df_PY)[4] <- 'res' 
  new_df <- rbind( old_df , new_df_PY )
  
  return(new_df)
}

#---------1
nrmse_sc1 <- read.csv("/Users/ash/Desktop/MICE/R/H-G csv/nrmse_sc1_PY_R.csv")
#nrmse_sc1_plot <- plott(res = sett(nrmse_sc1), sc_num = 1, metrica = 'nrmse')
#nrmse_sc1_plot[[1]] + xlim(0, 1)


PY_nrmse_sc1 <- read.csv("/Users/ash/Desktop/MICE/PY/fin_res_csv/mixNorms_nrmse_scen1.csv")
PY_nrmse_sc1[which(PY_nrmse_sc1$col == 'X^2'), ]$col = 'X'
new_nrmse_sc1 <- new_nrmse(nrmse_sc1, PY_nrmse_sc1)

miceBR_nrmse_sc1 <- read.csv("/Users/ash/Desktop/MICE/PY/fin_res_csv/miceBR_nrmse_scen1.csv")
miceBR_nrmse_sc1[which(miceBR_nrmse_sc1$col == 'X^2'), ]$col = 'X'
new2_nrmse_sc1 <- new_nrmse2(new_nrmse_sc1, miceBR_nrmse_sc1)


new_nrmse_sc1_plt <- plott(res = sett(new2_nrmse_sc1), sc_num = 1, metrica = 'nrmse')
new_nrmse_sc1_plt[[1]] + xlim(0, 1)

#new2_nrmse_sc1[new2_nrmse_sc1$estimator == 'mice_br',]
mean_values(new2_nrmse_sc1[which(new2_nrmse_sc1$col == 'X'), ])

#-------MAR 0.45
nrmse_sc1_R <- read.csv("/Users/ash/Desktop/MICE/R/H-G csv/0.45_miss/nrmse_sc1.csv")
nrmse_sc1_PY <- read.csv("/Users/ash/Desktop/MICE/PY/fin_res_csv/0.45_miss/nrmse_sc1.csv")
nrmse_sc1_PY[which(nrmse_sc1_PY$col == 'X^2'), ]$col = 'X'
nrmse_sc1 <- alltoge(nrmse_sc1_R, nrmse_sc1_PY)

mean_values(nrmse_sc1[which(nrmse_sc1$col == 'X'), ])


#-------MNAR
MNAR_nrmse_sc1_R <- read.csv("/Users/ash/Desktop/MICE/R/H-G csv/MNAR_nrmse_sc1.csv")
MNAR_nrmse_sc1_PY <- read.csv("/Users/ash/Desktop/MICE/PY/fin_res_csv/MNAR_nrmse_sc1.csv")
MNAR_nrmse_sc1_PY[MNAR_nrmse_sc1_PY$col == 'X^2', ]$col = 'X'
MNAR_nrmse_sc1 <- alltoge(MNAR_nrmse_sc1_R, MNAR_nrmse_sc1_PY[MNAR_nrmse_sc1_PY$estimator != 'mice_br',])

fin_plot <- plott(res = MNAR_nrmse_sc1, sc_num = 1, metrica = 'nrmse')
fin_plot[[1]] + xlim(0, 1)

mean_values(MNAR_nrmse_sc1[MNAR_nrmse_sc1$col == 'X', ]) 


#---------2
nrmse_sc2 <- read.csv("/Users/ash/Desktop/MICE/R/H-G csv/nrmse_sc2_PY_R.csv")
#nrmse_sc2_plot <- plott(res = sett(nrmse_sc2), sc_num = 2, metrica = 'nrmse')
#nrmse_sc2_plot[[1]] + xlim(0.2, 1.6)

PY_nrmse_sc2 <- read.csv("/Users/ash/Desktop/MICE/PY/fin_res_csv/mixNorms_nrmse_scen2.csv")
PY_nrmse_sc2[which(PY_nrmse_sc2$col == 'X^2'), ]$col = 'X'
new_nrmse_sc2 <- new_nrmse(nrmse_sc2, PY_nrmse_sc2)

miceBR_nrmse_sc2 <- read.csv("/Users/ash/Desktop/MICE/PY/fin_res_csv/miceBR_nrmse_scen2.csv")
miceBR_nrmse_sc2[which(miceBR_nrmse_sc2$col == 'X^2'), ]$col = 'X'
new2_nrmse_sc2 <- new_nrmse2(new_nrmse_sc2, miceBR_nrmse_sc2)

new_nrmse_sc2_plt <- plott(res = sett(new2_nrmse_sc2), sc_num = 2, metrica = 'nrmse')
new_nrmse_sc2_plt[[1]]  

mean_values(new2_nrmse_sc2[which(new2_nrmse_sc2$col == 'X'), ])

#-------MAR 0.45
nrmse_sc2_R <- read.csv("/Users/ash/Desktop/MICE/R/H-G csv/0.45_miss/nrmse_sc2.csv")
nrmse_sc2_PY <- read.csv("/Users/ash/Desktop/MICE/PY/fin_res_csv/0.45_miss/nrmse_sc2.csv")
nrmse_sc2_PY[which(nrmse_sc2_PY$col == 'X^2'), ]$col = 'X'
nrmse_sc2 <- alltoge(nrmse_sc2_R, nrmse_sc2_PY)

mean_values(nrmse_sc2[which(nrmse_sc2$col == 'X'), ])

#-------MNAR
MNAR_nrmse_sc2_R <- read.csv("/Users/ash/Desktop/MICE/R/H-G csv/MNAR_nrmse_sc2.csv")
MNAR_nrmse_sc2_PY <- read.csv("/Users/ash/Desktop/MICE/PY/fin_res_csv/MNAR_nrmse_sc2.csv")
MNAR_nrmse_sc2_PY[MNAR_nrmse_sc2_PY$col == 'X^2', ]$col = 'X'
MNAR_nrmse_sc2 <- alltoge(MNAR_nrmse_sc2_R, MNAR_nrmse_sc2_PY)

fin_plot <- plott(res = MNAR_nrmse_sc2, sc_num = 2, metrica = 'nrmse')
fin_plot[[1]] #+ xlim(0, 1)

mean_values(MNAR_nrmse_sc2[MNAR_nrmse_sc2$col == 'X', ])


#---------3
nrmse_sc3 <- read.csv("/Users/ash/Desktop/MICE/R/H-G csv/nrmse_sc3_PY_R.csv")
#nrmse_sc3_plot <- plott(res = sett(nrmse_sc3), sc_num = 3, metrica = 'nrmse')
#nrmse_sc3_plot[[1]] + xlim(0, 1)

PY_nrmse_sc3 <- read.csv("/Users/ash/Desktop/MICE/PY/fin_res_csv/mixNorms_nrmse_scen3.csv")
PY_nrmse_sc3[which(PY_nrmse_sc3$col == 'XZ'), ]$col = 'X'
new_nrmse_sc3 <- new_nrmse(nrmse_sc3, PY_nrmse_sc3)

miceBR_nrmse_sc3 <- read.csv("/Users/ash/Desktop/MICE/PY/fin_res_csv/miceBR_nrmse_scen3.csv")
miceBR_nrmse_sc3[which(miceBR_nrmse_sc3$col == 'XZ'), ]$col = 'X'
new2_nrmse_sc3 <- new_nrmse2(new_nrmse_sc3, miceBR_nrmse_sc3)

new_nrmse_sc3_plt <- plott(res = sett(new2_nrmse_sc3), sc_num = 3, metrica = 'nrmse')
new_nrmse_sc3_plt[[1]] + xlim(0, 1)

mean_values(new2_nrmse_sc3[which(new2_nrmse_sc3$col == 'X'), ])

#-------MAR 0.45
nrmse_sc3_R <- read.csv("/Users/ash/Desktop/MICE/R/H-G csv/0.45_miss/nrmse_sc3.csv")
nrmse_sc3_PY <- read.csv("/Users/ash/Desktop/MICE/PY/fin_res_csv/0.45_miss/nrmse_sc3.csv")
nrmse_sc3 <- alltoge(nrmse_sc3_R, nrmse_sc3_PY)

mean_values(nrmse_sc3[which(nrmse_sc3$col == 'X'), ])

#-------MNAR
MNAR_nrmse_sc3_R <- read.csv("/Users/ash/Desktop/MICE/R/H-G csv/MNAR_nrmse_sc3.csv")
MNAR_nrmse_sc3_PY <- read.csv("/Users/ash/Desktop/MICE/PY/fin_res_csv/MNAR_nrmse_sc3.csv")
MNAR_nrmse_sc3_PY[MNAR_nrmse_sc3_PY$col == 'XZ', ]$col = 'X'
MNAR_nrmse_sc3 <- alltoge(MNAR_nrmse_sc3_R, MNAR_nrmse_sc3_PY[MNAR_nrmse_sc3_PY$estimator != 'mice_br',])

fin_plot <- plott(res = MNAR_nrmse_sc3, sc_num = 3, metrica = 'nrmse')
fin_plot[[1]] + xlim(0, 1)

mean_values(MNAR_nrmse_sc3[MNAR_nrmse_sc3$col == 'X', ])



#---------4
nrmse_sc4 <- read.csv("/Users/ash/Desktop/MICE/R/H-G csv/nrmse_sc4_PY_R.csv")
#nrmse_sc4_plot <- plott(res = sett(nrmse_sc4), sc_num = 4, metrica = 'nrmse')
#nrmse_sc4_plot[[1]] + xlim(0.4, 1.4)

PY_nrmse_sc4 <- read.csv("/Users/ash/Desktop/MICE/PY/fin_res_csv/mixNorms_nrmse_scen4.csv")
PY_nrmse_sc4[which(PY_nrmse_sc4$col == 'XZ'), ]$col = 'X'
new_nrmse_sc4 <- new_nrmse(nrmse_sc4, PY_nrmse_sc4)

miceBR_nrmse_sc4 <- read.csv("/Users/ash/Desktop/MICE/PY/fin_res_csv/miceBR_nrmse_scen4.csv")
miceBR_nrmse_sc4[which(miceBR_nrmse_sc4$col == 'XZ'), ]$col = 'X'
new2_nrmse_sc4 <- new_nrmse2(new_nrmse_sc4, miceBR_nrmse_sc4)

new_nrmse_sc4_plt <- plott(res = sett(new2_nrmse_sc4), sc_num = 4, metrica = 'nrmse')
new_nrmse_sc4_plt[[1]] + xlim(0.4, 1.4)

mean_values(new2_nrmse_sc4[which(new2_nrmse_sc4$col == 'X'), ])

#-------MAR 0.45
nrmse_sc4_R <- read.csv("/Users/ash/Desktop/MICE/R/H-G csv/0.45_miss/nrmse_sc4.csv")
nrmse_sc4_PY <- read.csv("/Users/ash/Desktop/MICE/PY/fin_res_csv/0.45_miss/nrmse_sc4.csv")
nrmse_sc4 <- alltoge(nrmse_sc4_R, nrmse_sc4_PY)

mean_values(nrmse_sc4[which(nrmse_sc4$col == 'X'), ])

#-------MNAR
MNAR_nrmse_sc4_R <- read.csv("/Users/ash/Desktop/MICE/R/H-G csv/MNAR_nrmse_sc4.csv")
MNAR_nrmse_sc4_PY <- read.csv("/Users/ash/Desktop/MICE/PY/fin_res_csv/MNAR_nrmse_sc4.csv")
MNAR_nrmse_sc4_PY[MNAR_nrmse_sc4_PY$col == 'XZ', ]$col = 'X'
MNAR_nrmse_sc4 <- alltoge(MNAR_nrmse_sc4_R, MNAR_nrmse_sc4_PY)

fin_plot <- plott(res = MNAR_nrmse_sc4, sc_num = 4, metrica = 'nrmse')
fin_plot[[1]] #+ xlim(0, 1)

mean_values(MNAR_nrmse_sc4[MNAR_nrmse_sc4$col == 'X', ])


#----------------------------------
#----------------------------------
#Rel_bias_coef
#----------
# without outliers
relbcoef_outl_mean <- function(df){
  precentil <- pmin(pmax(df$res, quantile(df$res, .1)), quantile(df$res, .9))
  fin <- mean(abs(precentil))
  return(fin)
}

mean_values_relbcoef <- function(df){
  return(list(
    relbcoef_outl_mean(df[df$estimator == 'missForest',])*100,
    relbcoef_outl_mean(df[df$estimator == 'micePMM',])*100,
    relbcoef_outl_mean(df[df$estimator == 'mice_rf',])*100,
    relbcoef_outl_mean(df[df$estimator == 'mice_br',])*100,
    relbcoef_outl_mean(df[df$estimator == 'knn',])*100 ))
}
#----------------------------------
new_Rel_bias <- function(old_df, new_df_PY){
  colnames(new_df_PY)[4] <- 'res' 
  old <- subset(old_df, !((dist %in% c("norm_mix1", "norm_mix2")) & (estimator %in% c("knn", "mice_rf"))))
  new_df <- rbind( old , new_df_PY )
  
  return(new_df)
}

new_Rel_bias2 <- function(old_df, new_df_PY){
  colnames(new_df_PY)[4] <- 'res' 
  new_df <- rbind( old_df , new_df_PY )
  
  return(new_df)
}
#---------1
relb_coef_sc1 <- read.csv("/Users/ash/Desktop/MICE/R/H-G csv/relb_coef_sc1_PY_R.csv")
# relb_coef_sc1_plot <- plott(res = sett(relb_coef_sc1), sc_num = 1, metrica = 'relb_coef')
# relb_coef_sc1_plot[[1]] + xlim(-7.5, 2.5)
# relb_coef_sc1_plot[[2]] + xlim(-1, 4.5)

mxnorms_relb_coef_scen1 <- read.csv("/Users/ash/Desktop/MICE/PY/fin_res_csv/mixNorms_relb_coef_scen1.csv")
mxnorms_relb_coef_scen1[which(mxnorms_relb_coef_scen1$col == 'X^2'), ]$col = 'X2'
new_relb_coef_sc1 <- new_Rel_bias(relb_coef_sc1, mxnorms_relb_coef_scen1)

miceBR_relb_coef_sc1 <- read.csv("/Users/ash/Desktop/MICE/PY/fin_res_csv/miceBR_relb_coef_scen1.csv")
miceBR_relb_coef_sc1[which(miceBR_relb_coef_sc1$col == 'X^2'), ]$col = 'X2'
new2_relb_coef_sc1 <- new_Rel_bias2(new_relb_coef_sc1, miceBR_relb_coef_sc1)
#new2_relb_coef_sc1[new2_relb_coef_sc1$estimator != 'mice_br',]

new2_relb_coef_sc1[new2_relb_coef_sc1$estimator == 'mice_br',]

#new_relb_sc1_plot <- plott(res = sett(new2_relb_coef_sc1[new2_relb_coef_sc1$estimator != 'mice_br',]), sc_num = 1, metrica = 'relb_coef')
new_relb_sc1_plot <- plott(res = sett(new2_relb_coef_sc1), sc_num = 1, metrica = 'relb_coef')
new_relb_sc1_plot[[1]] + xlim(-7.5, 2.5)
new_relb_sc1_plot[[2]] + xlim(-1, 4.5)

#mean_values(new2_relb_coef_sc1[which(new2_relb_coef_sc1$col == 'X'), ])
#mean_values(new2_relb_coef_sc1[which(new2_relb_coef_sc1$col == 'X2'), ])

mean_values_relbcoef(new2_relb_coef_sc1[which(new2_relb_coef_sc1$col == 'X'), ])
mean_values_relbcoef(new2_relb_coef_sc1[which(new2_relb_coef_sc1$col == 'X2'), ])


#-------MAR 0.45
relb_coef_sc1_R <- read.csv("/Users/ash/Desktop/MICE/R/H-G csv/0.45_miss/relb_coef_sc1.csv")
relb_coef_sc1_PY <- read.csv("/Users/ash/Desktop/MICE/PY/fin_res_csv/0.45_miss/relb_coef_sc1.csv")
relb_coef_sc1_PY[which(relb_coef_sc1_PY$col == 'X^2'), ]$col = 'X2'
relb_coef_sc1 <- alltoge(relb_coef_sc1_R, relb_coef_sc1_PY)

mean_values_relbcoef(relb_coef_sc1[which(relb_coef_sc1$col == 'X'), ])
mean_values_relbcoef(relb_coef_sc1[which(relb_coef_sc1$col == 'X2'), ])


#-------MNAR
MNAR_relb_coef_sc1_R <- read.csv("/Users/ash/Desktop/MICE/R/H-G csv/MNAR_relb_coef_sc1.csv")
MNAR_relb_coef_sc1_PY <- read.csv("/Users/ash/Desktop/MICE/PY/fin_res_csv/MNAR_relb_coef_sc1.csv")
MNAR_relb_coef_sc1_PY[MNAR_relb_coef_sc1_PY$col == 'X^2', ]$col = 'X2'
MNAR_relb_coef_sc1 <- alltoge(MNAR_relb_coef_sc1_R, MNAR_relb_coef_sc1_PY[MNAR_relb_coef_sc1_PY$estimator != 'mice_br',])

fin_plot_4 <- plott(res = MNAR_relb_coef_sc1, sc_num = 1, metrica = 'relb_coef')
fin_plot_4[[1]] + xlim(-7.5, 2.5)
fin_plot_4[[2]] + xlim(-1, 4.5)

mean_values_relbcoef(MNAR_relb_coef_sc1[MNAR_relb_coef_sc1$col == 'X', ])
mean_values_relbcoef(MNAR_relb_coef_sc1[MNAR_relb_coef_sc1$col == 'X2', ])


#---------2
relb_coef_sc2 <- read.csv("/Users/ash/Desktop/MICE/R/H-G csv/relb_coef_sc2_PY_R.csv")
# relb_coef_sc2_plot <- plott(res = sett(relb_coef_sc2), sc_num = 2, metrica = 'relb_coef')
# relb_coef_sc2_plot[[1]] + xlim(-25, 25)
# relb_coef_sc2_plot[[2]] + xlim(-40, 20)

mxnorms_relb_coef_scen2 <- read.csv("/Users/ash/Desktop/MICE/PY/fin_res_csv/mixNorms_relb_coef_scen2.csv")
mxnorms_relb_coef_scen2[which(mxnorms_relb_coef_scen2$col == 'X^2'), ]$col = 'X2'
new_relb_coef_sc2 <- new_Rel_bias(relb_coef_sc2, mxnorms_relb_coef_scen2)

miceBR_relb_coef_sc2 <- read.csv("/Users/ash/Desktop/MICE/PY/fin_res_csv/miceBR_relb_coef_scen2.csv")
miceBR_relb_coef_sc2[which(miceBR_relb_coef_sc2$col == 'X^2'), ]$col = 'X2'
new2_relb_coef_sc2 <- new_Rel_bias2(new_relb_coef_sc2, miceBR_relb_coef_sc2)

new_relb_coef_sc2_plot <- plott(res = sett(new2_relb_coef_sc2), sc_num = 2, metrica = 'relb_coef')
new_relb_coef_sc2_plot[[1]] + xlim(-20, 20)
new_relb_coef_sc2_plot[[2]] + xlim(-40, 20)


#mean_values(new2_relb_coef_sc2[which(new2_relb_coef_sc2$col == 'X'), ])
#mean_values(new2_relb_coef_sc2[which(new2_relb_coef_sc2$col == 'X2'), ])

mean_values_relbcoef(new2_relb_coef_sc2[which(new2_relb_coef_sc2$col == 'X'), ])
mean_values_relbcoef(new2_relb_coef_sc2[which(new2_relb_coef_sc2$col == 'X2'), ])

#-------MAR 0.45
relb_coef_sc2_R <- read.csv("/Users/ash/Desktop/MICE/R/H-G csv/0.45_miss/relb_coef_sc2.csv")
relb_coef_sc2_PY <- read.csv("/Users/ash/Desktop/MICE/PY/fin_res_csv/0.45_miss/relb_coef_sc2.csv")
relb_coef_sc2_PY[which(relb_coef_sc2_PY$col == 'X^2'), ]$col = 'X2'
relb_coef_sc2 <- alltoge(relb_coef_sc2_R, relb_coef_sc2_PY)

mean_values_relbcoef(relb_coef_sc2[which(relb_coef_sc2$col == 'X'), ])
mean_values_relbcoef(relb_coef_sc2[which(relb_coef_sc2$col == 'X2'), ])


#-------MNAR
MNAR_relb_coef_sc2_R <- read.csv("/Users/ash/Desktop/MICE/R/H-G csv/MNAR_relb_coef_sc2.csv")
MNAR_relb_coef_sc2_PY <- read.csv("/Users/ash/Desktop/MICE/PY/fin_res_csv/MNAR_relb_coef_sc2.csv")
MNAR_relb_coef_sc2_PY[MNAR_relb_coef_sc2_PY$col == 'X^2', ]$col = 'X2'
MNAR_relb_coef_sc2 <- alltoge(MNAR_relb_coef_sc2_R, MNAR_relb_coef_sc2_PY)

fin_plot <- plott(res = MNAR_relb_coef_sc2, sc_num = 2, metrica = 'relb_coef')
fin_plot[[1]] + xlim(-25, 20)
fin_plot[[2]] + xlim(-40, 20)

mean_values_relbcoef(MNAR_relb_coef_sc2[MNAR_relb_coef_sc2$col == 'X', ])
mean_values_relbcoef(MNAR_relb_coef_sc2[MNAR_relb_coef_sc2$col == 'X2', ])


#---------3
relb_coef_sc3 <- read.csv("/Users/ash/Desktop/MICE/R/H-G csv/relb_coef_sc3_PY_R.csv")
# relb_coef_sc3_plot <- plott(res = sett(relb_coef_sc3), sc_num = 3, metrica = 'relb_coef')
# relb_coef_sc3_plot[[1]] + xlim(-5, 3)
# relb_coef_sc3_plot[[2]] + xlim(-1, 1.5)

mxnorms_relb_coef_scen3 <- read.csv("/Users/ash/Desktop/MICE/PY/fin_res_csv/mixNorms_relb_coef_scen3.csv")
new_relb_coef_sc3 <- new_Rel_bias(relb_coef_sc3, mxnorms_relb_coef_scen3)

miceBR_relb_coef_sc3 <- read.csv("/Users/ash/Desktop/MICE/PY/fin_res_csv/miceBR_relb_coef_scen3.csv")
new2_relb_coef_sc3 <- new_Rel_bias2(new_relb_coef_sc3, miceBR_relb_coef_sc3)

#new2_relb_coef_sc3[new2_relb_coef_sc3$estimator != 'mice_br',]
#new_relb_coef_sc3_plot <- plott(res = sett(new2_relb_coef_sc3[new2_relb_coef_sc3$estimator != 'mice_br',]), sc_num = 3, metrica = 'relb_coef')
new_relb_coef_sc3_plot <- plott(res = sett(new2_relb_coef_sc3), sc_num = 3, metrica = 'relb_coef')
new_relb_coef_sc3_plot[[1]] + xlim(-5, 3)
new_relb_coef_sc3_plot[[2]] + xlim(-1, 1.5)

#mean_values(new2_relb_coef_sc3[which(new2_relb_coef_sc3$col == 'X'), ])
#mean_values(new2_relb_coef_sc3[which(new2_relb_coef_sc3$col == 'XZ'), ])

mean_values_relbcoef(new2_relb_coef_sc3[which(new2_relb_coef_sc3$col == 'X'), ])
mean_values_relbcoef(new2_relb_coef_sc3[which(new2_relb_coef_sc3$col == 'XZ'), ])

#-------MAR 0.45
relb_coef_sc3_R <- read.csv("/Users/ash/Desktop/MICE/R/H-G csv/0.45_miss/relb_coef_sc3.csv")
relb_coef_sc3_PY <- read.csv("/Users/ash/Desktop/MICE/PY/fin_res_csv/0.45_miss/relb_coef_sc3.csv")
relb_coef_sc3 <- alltoge(relb_coef_sc3_R, relb_coef_sc3_PY)

mean_values_relbcoef(relb_coef_sc3[which(relb_coef_sc3$col == 'X'), ])
mean_values_relbcoef(relb_coef_sc3[which(relb_coef_sc3$col == 'XZ'), ])

#-------MNAR
MNAR_relb_coef_sc3_R <- read.csv("/Users/ash/Desktop/MICE/R/H-G csv/MNAR_relb_coef_sc3.csv")
MNAR_relb_coef_sc3_PY <- read.csv("/Users/ash/Desktop/MICE/PY/fin_res_csv/MNAR_relb_coef_sc3.csv")
MNAR_relb_coef_sc3 <- alltoge(MNAR_relb_coef_sc3_R, MNAR_relb_coef_sc3_PY[MNAR_relb_coef_sc3_PY$estimator != 'mice_br',])

fin_plot <- plott(res = MNAR_relb_coef_sc3, sc_num = 3, metrica = 'relb_coef')
fin_plot[[1]] + xlim(-5, 3)
fin_plot[[2]] + xlim(-1, 1.5)

mean_values_relbcoef(MNAR_relb_coef_sc3[MNAR_relb_coef_sc3$col == 'X', ])
mean_values_relbcoef(MNAR_relb_coef_sc3[MNAR_relb_coef_sc3$col == 'XZ', ])

#---------4
relb_coef_sc4 <- read.csv("/Users/ash/Desktop/MICE/R/H-G csv/relb_coef_sc4_PY_R.csv")
# relb_coef_sc4_plot <- plott(res = sett(relb_coef_sc4), sc_num = 4, metrica = 'relb_coef')
# relb_coef_sc4_plot[[1]] + xlim(-2, 4)
# relb_coef_sc4_plot[[2]] + xlim(-2.5, 7.5)

mxnorms_relb_coef_scen4 <- read.csv("/Users/ash/Desktop/MICE/PY/fin_res_csv/mixNorms_relb_coef_scen4.csv")
new_relb_coef_sc4 <- new_Rel_bias(relb_coef_sc4, mxnorms_relb_coef_scen4)

miceBR_relb_coef_sc4 <- read.csv("/Users/ash/Desktop/MICE/PY/fin_res_csv/miceBR_relb_coef_scen4.csv")
new2_relb_coef_sc4 <- new_Rel_bias2(new_relb_coef_sc4, miceBR_relb_coef_sc4)

new_relb_coef_sc4_plot <- plott(res = sett(new2_relb_coef_sc4), sc_num = 4, metrica = 'relb_coef')
new_relb_coef_sc4_plot[[1]] + xlim(-2, 4)
new_relb_coef_sc4_plot[[2]] + xlim(-2.5, 7.5)

mean_values(new2_relb_coef_sc4[which(new2_relb_coef_sc4$col == 'X'), ])
mean_values(new2_relb_coef_sc4[which(new2_relb_coef_sc4$col == 'XZ'), ])

#-------MAR 0.45
relb_coef_sc4_R <- read.csv("/Users/ash/Desktop/MICE/R/H-G csv/0.45_miss/relb_coef_sc4.csv")
relb_coef_sc4_PY <- read.csv("/Users/ash/Desktop/MICE/PY/fin_res_csv/0.45_miss/relb_coef_sc4.csv")
relb_coef_sc4 <- alltoge(relb_coef_sc4_R, relb_coef_sc4_PY)

mean_values_relbcoef(relb_coef_sc4[which(relb_coef_sc4$col == 'X'), ])
mean_values_relbcoef(relb_coef_sc4[which(relb_coef_sc4$col == 'XZ'), ])

#-------MNAR
MNAR_relb_coef_sc4_R <- read.csv("/Users/ash/Desktop/MICE/R/H-G csv/MNAR_relb_coef_sc4.csv")
MNAR_relb_coef_sc4_PY <- read.csv("/Users/ash/Desktop/MICE/PY/fin_res_csv/MNAR_relb_coef_sc4.csv")
MNAR_relb_coef_sc4 <- alltoge(MNAR_relb_coef_sc4_R, MNAR_relb_coef_sc4_PY)

fin_plot <- plott(res = MNAR_relb_coef_sc4, sc_num = 4, metrica = 'relb_coef')
fin_plot[[1]] + xlim(-2, 4)
fin_plot[[2]] + xlim(-2.5, 7.5)

mean_values_relbcoef(MNAR_relb_coef_sc4[MNAR_relb_coef_sc4$col == 'X', ])
mean_values_relbcoef(MNAR_relb_coef_sc4[MNAR_relb_coef_sc4$col == 'XZ', ])
