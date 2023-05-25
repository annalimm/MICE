source("/Users/ash/Desktop/MICE/R/ampt.R")
source("/Users/ash/Desktop/MICE/R/gener_dt.R")
source("/Users/ash/Desktop/MICE/R/imputers.R")
source("/Users/ash/Desktop/MICE/R/metrics.R")
source("/Users/ash/Desktop/MICE/R/H-G_all.R")


df <- gen_dt(X_distr = 'norm', scenar = 'lr_inter')
X_miss = amp(df)
X_miss

mice_imp <- domice(X_miss, 'rfdoove10', reps = 5)
length(mice_imp)


df_mean <- function(imp_mice){
  df_shape = dim(imp_mice[[1]])
  res <- data.frame(matrix(0, ncol = df_shape[2], nrow = df_shape[1]))
  
  for (imp in imp_mice){
    res <- res + imp
  }
  fin_df <- res / length(mice_imp)
  colnames(fin_df) <- colnames(imp_mice[[1]])
  return(fin_df)
}


df_imp <- df_mean(mice_imp)
df_imp


rel_b_100 <- read.csv("/Users/ash/Desktop/MICE/R/H-G csv/old_version/nrmse_scen3_100cy.csv")
rel_b_100$rel_bias <- as.numeric(rel_b_100$rel_bias)
rel_b_100$dist <- fct_rev(factor(rel_b_100$dist, levels = c('norm', 'unif', 'lognorm1', 'lognorm2', 'gamma1', 'gamma2', 'norm_mix1', 'norm_mix2')))
rel_b_100

byX <- rel_b_100[which(rel_b_100$col == 'X'), ]
ggplot(rel_b_100, aes(x = nrmse, y = dist, color = estimator)) + geom_boxplot()+ ggtitle("Scen.") + xlim(0, 1)


byX2 <- rel_b_100[which(rel_b_100$col == 'XZ'), ]
ggplot(byX2, aes(x = rel_bias, y = dist, color = estimator)) + geom_boxplot()+ ggtitle("Scen.") + xlim(-0.18, 0.14)


typeof(list("d", "ss"))

for(x in list("d", "ss")){
  print(x)
}


relb_sc1_R <- read.csv("/Users/ash/Desktop/MICE/R/H-G csv/rel_bias_scen1_100cy.csv")
relb_sc1_PY <- read.csv("/Users/ash/Desktop/MICE/PY/fin_res_csv/rel_bias_scen1_100cy.csv")


df_togg <- puki(nmrse_scen4_R[,-1], nmrse_scen4_PY)
write.csv(df_togg, "/Users/ash/Desktop/MICE/R/H-G csv/nmrse_scen4_PY_R.csv", row.names = FALSE)


distrs <- c('norm', 'unif', 'lognorm1', 'lognorm2', 'gamma1', 'gamma2', 'norm_mix1', 'norm_mix2')

df <- gen_dt(X_distr = 'norm_mix2', scenar = 'lr_quadr')
ggplot(df, aes(x=X, y=Y)) + 
  geom_point()


# tmp <- rel_bias_scen1_100cy_PY[which(rel_bias_scen1_100cy_PY$dist == 'norm_mix1'), ]$rel_bias
# ppp <- (pmin(pmax(tmp, quantile(tmp, .05)), quantile(tmp, .95)))
# length(tmp)
# 
# rel_bias_scen1_100cy_PY[which(rel_bias_scen1_100cy_PY$dist == 'norm_mix1'), ]








