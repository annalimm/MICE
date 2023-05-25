source("/Users/ash/Desktop/MICE/R/gener_dt.R")
library("dplyr")
library("mice")
# library(CALIBERrfimpute)
library("locfit")
#install.packages("hydroGOF")
library("missForest")
library(ggplot2)
library(forcats)
library("lattice")
library("hydroGOF")

distrs <- c('norm', 'unif', 'lognorm1', 'lognorm2', 'gamma1', 'gamma2', 'norm_mix1', 'norm_mix2')

# df <- gen_dt(X_distr = 'lognorm1', scenar = 'lr_quadr')
# X_miss = ampute(df, prop = 0.25, mech = "MAR", type = "RIGHT", weights = c(0, 0, 1), patterns = c(0, 1, 1))$amp
# complete(mice(X_miss, maxit = 5, m = 5, meth = 'rf'))
# df_miss <- missForest(X_miss)$ximp
# nrmse(df, df_miss, na.rm=TRUE, norm="sd", ...)


imp_bias <- function(n_cycle, scenar){
  df_bias_imp <- data.frame(matrix(ncol = 4, nrow = 0))
  colnames(df_bias_imp) <- c('dist', 'estimator', 'col', 'rel_bias')
  for (dist in distrs){
    print(dist)
    for (n_c in seq(from = 1, to = n_cycle, by = 1)){
    
      df <- gen_dt(X_distr = dist, scenar = scenar)
      df_X_mean <- mean(df$X)
      df_X2_mean <- mean(df$'X2')
      X_miss = ampute(df, prop = 0.25, mech = "MAR", type = "RIGHT", weights = c(0, 1, 0), patterns = c(0, 1, 0))$amp
  
      #missForest
      #----------------
      missFor_imp <- missForest(X_miss)$ximp
      missFor_X_mean <- mean( missFor_imp $X)
      missFor_X_rel_bias <- (missFor_X_mean / df_X_mean - 1)
      df_bias_imp[nrow(df_bias_imp) + 1,] = c(dist, 'missFor', 'X', missFor_X_rel_bias)
      # nrmse(sim = missFor_imp $X, obs = )

      missFor_X2_mean <- mean( missFor_imp $'X2')
      missFor_X2_rel_bias <- (missFor_X2_mean / df_X2_mean - 1)
      df_bias_imp[nrow(df_bias_imp) + 1,] = c(dist, 'missFor', 'X2', missFor_X2_rel_bias)

      #miceRF
      #----------------
      miceRf_imp <- complete(mice(X_miss, m = 5, meth = 'rf', ntree = 10))
      miceRf_X_mean <- mean( miceRf_imp $X)
      miceRf_X_rel_bias <- (miceRf_X_mean / df_X_mean - 1)
      df_bias_imp[nrow(df_bias_imp) + 1,] = c(dist, 'miceRf', 'X', miceRf_X_rel_bias)

      miceRf_X2_mean <- mean( miceRf_imp $'X2')
      miceRf_X2_rel_bias <- (miceRf_X2_mean / df_X2_mean - 1)
      df_bias_imp[nrow(df_bias_imp) + 1,] = c(dist, 'miceRf', 'X2', miceRf_X2_rel_bias)

      #micePMM
      #----------------
      micePmm_imp <- complete(mice(X_miss, maxit = 5, m = 5, meth = 'pmm'))
      micePmm_X_mean <- mean( micePmm_imp $X)
      micePmm_X_rel_bias <- (micePmm_X_mean / df_X_mean - 1)
      df_bias_imp[nrow(df_bias_imp) + 1,] = c(dist, 'micePmm', 'X', micePmm_X_rel_bias)
      
      micePmm_X2_mean <- mean( micePmm_imp $'X2')
      micePmm_X2_rel_bias <- (micePmm_X2_mean / df_X2_mean - 1)
      df_bias_imp[nrow(df_bias_imp) + 1,] = c(dist, 'micePmm', 'X2', micePmm_X2_rel_bias)
    }
  }
  return(df_bias_imp)
}

#-------------------------------------------
#-------------------------------------------
relb_scen1_100cy <- imp_bias(n_cycle = 100, scenar = 'lr_quadr')
relb_scen1_100cy$rel_bias <- as.numeric(relb_scen1_100cy$rel_bias)
relb_scen1_100cy$dist <- fct_rev(factor(relb_scen1_100cy$dist, levels = c('norm', 'unif', 'lognorm1', 'lognorm2', 'gamma1', 'gamma2', 'norm_mix1', 'norm_mix2')))
relb_scen1_100cy

byX <- relb_scen1_100cy[which(relb_scen1_100cy$col == 'X'), ] 
ggplot(byX, aes(x = rel_bias, y = dist, color = estimator)) + geom_boxplot() + ggtitle("Scen.1, rel.bias by X, 100 cy.")  + xlim(-0.18, 0.15)

byX2 <- relb_scen1_100cy[which(relb_scen1_100cy$col == 'X2'), ] 
ggplot(byX2, aes(x = rel_bias, y = dist, color = estimator)) + geom_boxplot() + ggtitle("Scen.1, rel.bias by X^2, 100 cy.")  + xlim(-0.35, 0.35)


# write.csv(relb_scen1_100cy, "/Users/ash/Desktop/MICE/R/H-G/rel_bias/scen1_100cy.csv", row.names = FALSE)



