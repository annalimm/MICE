#----nrmse
#-----------R
nmrse_scen1_R <- read.csv("/Users/ash/Desktop/MICE/R/H-G csv/nmrse_scen1_100cy.csv")
mean(nmrse_scen1_R[which(nmrse_scen1_R$estimator == 'missForest'), ]$nrmse)
mean(nmrse_scen1_R[which(nmrse_scen1_R$estimator == 'rfcont10'), ]$nrmse)
mean(nmrse_scen1_R[which(nmrse_scen1_R$estimator == 'micePMM'), ]$nrmse)

nmrse_scen2_R <- read.csv("/Users/ash/Desktop/MICE/R/H-G csv/nrmse_scen2.csv")
mean(nmrse_scen2_R[which(nmrse_scen2_R$estimator == 'missForest'), ]$nrmse)
mean(nmrse_scen2_R[which(nmrse_scen2_R$estimator == 'rfcont10'), ]$nrmse)
mean(nmrse_scen2_R[which(nmrse_scen2_R$estimator == 'micePMM'), ]$nrmse)

nmrse_scen3_R <- read.csv("/Users/ash/Desktop/MICE/R/H-G csv/nmrse_scen3_100cy.csv")
mean(nmrse_scen3_R[which(nmrse_scen3_R$estimator == 'missForest'), ]$nrmse)
mean(nmrse_scen3_R[which(nmrse_scen3_R$estimator == 'rfcont10'), ]$nrmse)
mean(nmrse_scen3_R[which(nmrse_scen3_R$estimator == 'micePMM'), ]$nrmse)

nmrse_scen4_R <- read.csv("/Users/ash/Desktop/MICE/R/H-G csv/nrmse_scen4.csv")
mean(nmrse_scen4_R[which(nmrse_scen4_R$estimator == 'missForest'), ]$nrmse)
mean(nmrse_scen4_R[which(nmrse_scen4_R$estimator == 'rfcont10'), ]$nrmse)
mean(nmrse_scen4_R[which(nmrse_scen4_R$estimator == 'micePMM'), ]$nrmse)

#-----------PY
nmrse_scen1_PY <- read.csv("/Users/ash/Desktop/MICE/PY/fin_res_csv/nrmse_scen1_100cy.csv")
#mean(nmrse_scen1_PY[which(nmrse_scen1_PY$estimator == 'ice_rf'), ]$nrmse)
mean(nmrse_scen1_PY[which(nmrse_scen1_PY$estimator == 'mice_rf'), ]$nrmse)
mean(nmrse_scen1_PY[which(nmrse_scen1_PY$estimator == 'knn'), ]$nrmse)

nmrse_scen2_PY <- read.csv("/Users/ash/Desktop/MICE/PY/fin_res_csv/nrmse_scen2.csv")
#mean(nmrse_scen2_PY[which(nmrse_scen2_PY$estimator == 'ice_rf'), ]$nrmse)
mean(nmrse_scen2_PY[which(nmrse_scen2_PY$estimator == 'mice_rf'), ]$nrmse)
mean(nmrse_scen2_PY[which(nmrse_scen2_PY$estimator == 'knn'), ]$nrmse)

nmrse_scen3_PY <- read.csv("/Users/ash/Desktop/MICE/PY/fin_res_csv/nrmse_scen3_100cy.csv")
#mean(nmrse_scen3_PY[which(nmrse_scen3_PY$estimator == 'ice_rf'), ]$nrmse)
mean(nmrse_scen3_PY[which(nmrse_scen3_PY$estimator == 'mice_rf'), ]$nrmse)
mean(nmrse_scen3_PY[which(nmrse_scen3_PY$estimator == 'knn'), ]$nrmse)

nmrse_scen4_PY <- read.csv("/Users/ash/Desktop/MICE/PY/fin_res_csv/nrmse_scen4.csv")
#mean(nmrse_scen4_PY[which(nmrse_scen4_PY$estimator == 'ice_rf'), ]$nrmse)
mean(nmrse_scen4_PY[which(nmrse_scen4_PY$estimator == 'mice_rf'), ]$nrmse)
mean(nmrse_scen4_PY[which(nmrse_scen4_PY$estimator == 'knn'), ]$nrmse)




#----rel_bias_X
#-----------R
relb_mean_res <- function(df, estimator){
  res <- mean(abs(df[which(df$estimator == estimator), ]$rel_bias))
  return(res)
}

rel_bias_scen1_R <- read.csv("/Users/ash/Desktop/MICE/R/H-G csv/rel_bias_scen1_100cy.csv")
X_rel_bias_scen1_R <- rel_bias_scen1_R[which(rel_bias_scen1_R$col == 'X'), ]
rel_bias_scen2_R <- read.csv("/Users/ash/Desktop/MICE/R/H-G csv/rel_bias_scen2.csv")
X_rel_bias_scen2_R <- rel_bias_scen2_R[which(rel_bias_scen2_R$col == 'X'), ]
rel_bias_scen3_R <- read.csv("/Users/ash/Desktop/MICE/R/H-G csv/rel_bias_scen3_100cy.csv")
X_rel_bias_scen3_R <- rel_bias_scen3_R[which(rel_bias_scen3_R$col == 'X'), ]
rel_bias_scen4_R <- read.csv("/Users/ash/Desktop/MICE/R/H-G csv/rel_bias_scen4.csv")
X_rel_bias_scen4_R <- rel_bias_scen4_R[which(rel_bias_scen4_R$col == 'X'), ]

relb_mean_res(X_rel_bias_scen1_R, 'missForest')
relb_mean_res(X_rel_bias_scen2_R, 'missForest')
relb_mean_res(X_rel_bias_scen3_R, 'missForest')
relb_mean_res(X_rel_bias_scen4_R, 'missForest')

relb_mean_res(X_rel_bias_scen1_R, 'rfcont10')
relb_mean_res(X_rel_bias_scen2_R, 'rfcont10')
relb_mean_res(X_rel_bias_scen3_R, 'rfcont10')
relb_mean_res(X_rel_bias_scen4_R, 'rfcont10')

relb_mean_res(X_rel_bias_scen1_R, 'micePMM')
relb_mean_res(X_rel_bias_scen2_R, 'micePMM')
relb_mean_res(X_rel_bias_scen3_R, 'micePMM')
relb_mean_res(X_rel_bias_scen4_R, 'micePMM')

#-----------PY
rel_bias_scen1_PY <- read.csv("/Users/ash/Desktop/MICE/PY/fin_res_csv/rel_bias_scen1_100cy.csv")
X_rel_bias_scen1_PY <- rel_bias_scen1_PY[which(rel_bias_scen1_PY$col == 'X'), ]
rel_bias_scen2_PY <- read.csv("/Users/ash/Desktop/MICE/PY/fin_res_csv/rel_bias_scen2.csv")
X_rel_bias_scen2_PY <- rel_bias_scen2_PY[which(rel_bias_scen2_PY$col == 'X'), ]
rel_bias_scen3_PY <- read.csv("/Users/ash/Desktop/MICE/PY/fin_res_csv/rel_bias_scen3_100cy.csv")
X_rel_bias_scen3_PY <- rel_bias_scen3_PY[which(rel_bias_scen3_PY$col == 'X'), ]
rel_bias_scen4_PY <- read.csv("/Users/ash/Desktop/MICE/PY/fin_res_csv/rel_bias_scen4.csv")
X_rel_bias_scen4_PY <- rel_bias_scen4_PY[which(rel_bias_scen4_PY$col == 'X'), ]

relb_mean_res(X_rel_bias_scen1_PY, 'mice_rf')
relb_mean_res(X_rel_bias_scen2_PY, 'mice_rf')
relb_mean_res(X_rel_bias_scen3_PY, 'mice_rf')
relb_mean_res(X_rel_bias_scen4_PY, 'mice_rf')

relb_mean_res(X_rel_bias_scen1_PY, 'knn')
relb_mean_res(X_rel_bias_scen2_PY, 'knn')
relb_mean_res(X_rel_bias_scen3_PY, 'knn')
relb_mean_res(X_rel_bias_scen4_PY, 'knn')



#----rel_bias_X2/XZ
#-----------R
X2_rel_bias_scen1_R <- rel_bias_scen1_R[which(rel_bias_scen1_R$col == 'X2'), ]
X2_rel_bias_scen2_R <- rel_bias_scen2_R[which(rel_bias_scen2_R$col == 'X2'), ]
XZ_rel_bias_scen3_R <- rel_bias_scen3_R[which(rel_bias_scen3_R$col == 'XZ'), ]
XZ_rel_bias_scen4_R <- rel_bias_scen4_R[which(rel_bias_scen4_R$col == 'XZ'), ]

relb_mean_res(X2_rel_bias_scen1_R, 'missForest')
relb_mean_res(X2_rel_bias_scen2_R, 'missForest')
relb_mean_res(XZ_rel_bias_scen3_R, 'missForest')
relb_mean_res(XZ_rel_bias_scen4_R, 'missForest')

relb_mean_res(X2_rel_bias_scen1_R, 'rfcont10')
relb_mean_res(X2_rel_bias_scen2_R, 'rfcont10')
relb_mean_res(XZ_rel_bias_scen3_R, 'rfcont10')
relb_mean_res(XZ_rel_bias_scen4_R, 'rfcont10')

relb_mean_res(X2_rel_bias_scen1_R, 'micePMM')
relb_mean_res(X2_rel_bias_scen2_R, 'micePMM')
relb_mean_res(XZ_rel_bias_scen3_R, 'micePMM')
relb_mean_res(XZ_rel_bias_scen4_R, 'micePMM')

#-----------PY
X2_rel_bias_scen1_PY <- rel_bias_scen1_PY[which(rel_bias_scen1_PY$col == 'X^2'), ]
X2_rel_bias_scen2_PY <- rel_bias_scen2_PY[which(rel_bias_scen2_PY$col == 'X^2'), ]
XZ_rel_bias_scen3_PY <- rel_bias_scen3_PY[which(rel_bias_scen3_PY$col == 'XZ'), ]
XZ_rel_bias_scen4_PY <- rel_bias_scen4_PY[which(rel_bias_scen4_PY$col == 'XZ'), ]

relb_mean_res(X2_rel_bias_scen1_PY, 'mice_rf')
relb_mean_res(X2_rel_bias_scen2_PY, 'mice_rf')
relb_mean_res(XZ_rel_bias_scen3_PY, 'mice_rf')
relb_mean_res(XZ_rel_bias_scen4_PY, 'mice_rf')

relb_mean_res(X2_rel_bias_scen1_PY, 'knn')
relb_mean_res(X2_rel_bias_scen2_PY, 'knn')
relb_mean_res(XZ_rel_bias_scen3_PY, 'knn')
relb_mean_res(XZ_rel_bias_scen4_PY, 'knn')



#----relb_coef__X
#-----------R
X_df <- function(df, col){
  res <- df[which(df$col == col), ]
  return(res)
}

relbcoef_mean_res <- function(df, estimator){
  ress <- mean(abs(df[which(df$estimator == estimator), ]$res))
  return(ress)
}

relb_coef_sc1 <- read.csv("/Users/ash/Desktop/MICE/R/H-G csv/relb_coef_sc1_PY_R.csv")
X_relb_coef_sc1 <- X_df(relb_coef_sc1, 'X')
relb_coef_sc2 <- read.csv("/Users/ash/Desktop/MICE/R/H-G csv/relb_coef_sc2_PY_R.csv")
X_relb_coef_sc2 <- X_df(relb_coef_sc2, 'X')
relb_coef_sc3 <- read.csv("/Users/ash/Desktop/MICE/R/H-G csv/relb_coef_sc3_PY_R.csv")
X_relb_coef_sc3 <- X_df(relb_coef_sc3, 'X')
relb_coef_sc4 <- read.csv("/Users/ash/Desktop/MICE/R/H-G csv/relb_coef_sc4_PY_R.csv")
X_relb_coef_sc4 <- X_df(relb_coef_sc4, 'X')

relbcoef_mean_res(X_relb_coef_sc1, 'missForest')
relbcoef_mean_res(X_relb_coef_sc2, 'missForest')
relbcoef_mean_res(X_relb_coef_sc3, 'missForest')
relbcoef_mean_res(X_relb_coef_sc4, 'missForest')

relbcoef_mean_res(X_relb_coef_sc1, 'rfcont10')
relbcoef_mean_res(X_relb_coef_sc2, 'rfcont10')
relbcoef_mean_res(X_relb_coef_sc3, 'rfcont10')
relbcoef_mean_res(X_relb_coef_sc4, 'rfcont10')

relbcoef_mean_res(X_relb_coef_sc1, 'micePMM')
relbcoef_mean_res(X_relb_coef_sc2, 'micePMM')
relbcoef_mean_res(X_relb_coef_sc3, 'micePMM')
relbcoef_mean_res(X_relb_coef_sc4, 'micePMM')

relbcoef_mean_res(X_relb_coef_sc1, 'mice_rf')
relbcoef_mean_res(X_relb_coef_sc2, 'mice_rf')
relbcoef_mean_res(X_relb_coef_sc3, 'mice_rf')
relbcoef_mean_res(X_relb_coef_sc4, 'mice_rf')

relbcoef_mean_res(X_relb_coef_sc1, 'knn')
relbcoef_mean_res(X_relb_coef_sc2, 'knn')
relbcoef_mean_res(X_relb_coef_sc3, 'knn')
relbcoef_mean_res(X_relb_coef_sc4, 'knn')

X_relb_coef_sc2[which(X_relb_coef_sc2$estimator == 'knn'), ]


#----relb_coef_X2/XZ
#-----------R
X2_relb_coef_sc1 <- X_df(relb_coef_sc1, 'X2')
X2_relb_coef_sc2 <- X_df(relb_coef_sc2, 'X2')
XZ_relb_coef_sc3 <- X_df(relb_coef_sc3, 'XZ')
XZ_relb_coef_sc4 <- X_df(relb_coef_sc4, 'XZ')

relbcoef_mean_res(X2_relb_coef_sc1, 'missForest')
relbcoef_mean_res(X2_relb_coef_sc2, 'missForest')
relbcoef_mean_res(XZ_relb_coef_sc3, 'missForest')
relbcoef_mean_res(XZ_relb_coef_sc4, 'missForest')

relbcoef_mean_res(X2_relb_coef_sc1, 'rfcont10')
relbcoef_mean_res(X2_relb_coef_sc2, 'rfcont10')
relbcoef_mean_res(XZ_relb_coef_sc3, 'rfcont10')
relbcoef_mean_res(XZ_relb_coef_sc4, 'rfcont10')

relbcoef_mean_res(X2_relb_coef_sc1, 'micePMM')
relbcoef_mean_res(X2_relb_coef_sc2, 'micePMM')
relbcoef_mean_res(XZ_relb_coef_sc3, 'micePMM')
relbcoef_mean_res(XZ_relb_coef_sc4, 'micePMM')

relbcoef_mean_res(X2_relb_coef_sc1, 'mice_rf')
relbcoef_mean_res(X2_relb_coef_sc2, 'mice_rf')
relbcoef_mean_res(XZ_relb_coef_sc3, 'mice_rf')
relbcoef_mean_res(XZ_relb_coef_sc4, 'mice_rf')

relbcoef_mean_res(X2_relb_coef_sc1, 'knn')
relbcoef_mean_res(X2_relb_coef_sc2, 'knn')
relbcoef_mean_res(XZ_relb_coef_sc3, 'knn')
relbcoef_mean_res(XZ_relb_coef_sc4, 'knn')
