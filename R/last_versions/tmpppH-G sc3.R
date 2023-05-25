source("/Users/ash/Desktop/MICE/R/gener_dt.R")
library("dplyr")
library("mice")
library("CALIBERrfimpute")
library("locfit")
# install.packages("missForest")
library("missForest")
library(ggplot2)
library(forcats)


# distrs <- c('norm', 'unif', 'lognorm1', 'lognorm2', 'gamma1', 'gamma2', 'norm_mix1', 'norm_mix2')
distrs <- c('norm', 'unif', 'gamma1', 'gamma2', 'norm_mix1', 'norm_mix2')

# df <- gen_dt(X_distr = 'lognorm1', scenar = 'lr_quadr')
# X_miss = ampute(df, prop = 0.25, mech = "MAR", type = "RIGHT", weights = c(0, 0, 1), patterns = c(0, 1, 1))$amp
# 
# df_miss <- missForest(X_miss)$ximp
# 
# nrmse(df, df_miss, na.rm=TRUE, norm="sd", ...)


imp_bias <- function(n_cycle, scenar){
  
  df_bias_imp <- data.frame(matrix(ncol = 3, nrow = 0))
  colnames(df_bias_imp) <- c('dist', 'estimator', 'rel_bias')

  
  for (dist in distrs){
    print(dist)
    for (n_c in seq(from = 1, to = n_cycle, by = 1)){
    
      df <- gen_dt(X_distr = dist, scenar = scenar)
      df_col_mean <- mean(df$X)
      X_miss = ampute(df, prop = 0.25, mech = "MAR", type = "RIGHT", weights = c(0, 0, 1), patterns = c(0, 1, 1))$amp
  
      missFor_col_mean <- mean( missForest(X_miss)$ximp $X) 
      missFor_rel_bias <- (missFor_col_mean / df_col_mean - 1)
      # df_bias_imp <- rbind(df_bias_imp, c(dist = dist, estimator = 'missFor', rel_bias = missFor_rel_bias)) 
      df_bias_imp[nrow(df_bias_imp) + 1,] = c(dist, 'missFor', missFor_rel_bias)
      
      miceRf_col_mean <- mean( complete(mice(X_miss, maxit = 5, meth = 'rfcont', m = 5)) $X) 
      miceRf_rel_bias <- (miceRf_col_mean / df_col_mean - 1)
      df_bias_imp[nrow(df_bias_imp) + 1,] = c(dist, 'miceRf', miceRf_rel_bias)
      
      # micePmm_col_mean <- mean( complete(mice(X_miss, maxit = 5, m = 5)) $X) 
      # micePmm_rel_bias <- (micePmm_col_mean / df_col_mean - 1)
      # df_bias_imp[nrow(df_bias_imp) + 1,] = c(dist, 'micePmm', micePmm_rel_bias)
    }
  }
  return(df_bias_imp)
}


relb_scen1_X_100cy <- imp_bias(n_cycle = 100, scenar = 'lr_quadr')
relb_scen1_X_100cy$rel_bias <- as.numeric(relb_scen1_X_100cy$rel_bias)
relb_scen1_X_100cy$dist <- fct_rev(factor(relb_scen1_X_100cy$dist, levels = c('norm', 'unif', 'gamma1', 'gamma2', 'norm_mix1', 'norm_mix2')))
ggplot(relb_scen1_X_100cy, aes(x = rel_bias, y = dist, color = estimator)) + geom_boxplot() + xlim(-0.18, 0.1)


# ggplot(relb_scen2_X_100cy, aes(x = rel_bias, y = dist, color = estimator)) + geom_boxplot() + xlim(-0.18, 0.1)


relb_scen3_X_500cy <- imp_bias(n_cycle = 500, scenar = 'lr_inter')
relb_scen3_X_500cy$rel_bias <- as.numeric(relb_scen3_X_500cy$rel_bias)
relb_scen3_X_500cy$dist <- fct_rev(factor(relb_scen3_X_500cy$dist, levels = c('norm', 'unif', 'gamma1', 'gamma2', 'norm_mix1', 'norm_mix2')))
ggplot(relb_scen3_X_500cy, aes(x = rel_bias, y = dist, color = estimator)) + geom_boxplot() + xlim(-0.18, 0.1)




write.csv(relb_scen1_X_100cy, "/Users/ash/Desktop/MICE/R/H-G/rel_bias/scen1_X_100cy.csv", row.names = FALSE)


