source("/Users/ash/Desktop/MICE/R/gener_dt.R")
library("dplyr")
library("mice")
# library(CALIBERrfimpute)
library("locfit")
# install.packages("missForest")
library("missForest")
library(ggplot2)
library(forcats)
library("lattice")

distrs <- c('norm', 'unif', 'lognorm1', 'lognorm2', 'gamma1', 'gamma2', 'norm_mix1', 'norm_mix2')

#10 trees in rf!! check dif cases
imp_bias <- function(n_cycle, scenar){
  df_bias_imp <- data.frame(matrix(ncol = 4, nrow = 0))
  colnames(df_bias_imp) <- c('dist', 'estimator', 'col', 'rel_bias')
  for (dist in distrs){
    print(dist)
    for (n_c in seq(from = 1, to = n_cycle, by = 1)){
    
      df <- gen_dt(X_distr = dist, scenar = scenar)
      df_X_mean <- mean(df$X)
      df_XZ_mean <- mean(df$'XZ')
      X_miss = ampute(df, prop = 0.25, mech = "MAR", type = "RIGHT", weights = c(0, 1, 0, 0), patterns = c(0, 1, 1, 0))$amp
  
      #missForest
      #----------------
      missFor_imp <- missForest(X_miss)$ximp
      missFor_X_mean <- mean( missFor_imp $X) 
      missFor_X_rel_bias <- (missFor_X_mean / df_X_mean - 1)
      df_bias_imp[nrow(df_bias_imp) + 1,] = c(dist, 'missFor', 'X', missFor_X_rel_bias)
      
      missFor_XZ_mean <- mean( missFor_imp $'XZ')
      missFor_XZ_rel_bias <- (missFor_XZ_mean / df_XZ_mean - 1)
      df_bias_imp[nrow(df_bias_imp) + 1,] = c(dist, 'missFor', 'XZ', missFor_XZ_rel_bias)
      
      #miceRF
      #----------------
      miceRf_imp <- complete(mice(X_miss, m = 5, meth = 'rf', ntree = 10))
      miceRf_X_mean <- mean( miceRf_imp $X)
      miceRf_X_rel_bias <- (miceRf_X_mean / df_X_mean - 1)
      df_bias_imp[nrow(df_bias_imp) + 1,] = c(dist, 'miceRf', 'X', miceRf_X_rel_bias)

      miceRf_XZ_mean <- mean( miceRf_imp $'XZ')
      miceRf_XZ_rel_bias <- (miceRf_XZ_mean / df_XZ_mean - 1)
      df_bias_imp[nrow(df_bias_imp) + 1,] = c(dist, 'miceRf', 'XZ', miceRf_XZ_rel_bias)

      #micePMM
      #----------------
      micePmm_imp <- complete(mice(X_miss, maxit = 5, m = 5, meth = 'pmm'))
      micePmm_X_mean <- mean( micePmm_imp $X)
      micePmm_X_rel_bias <- (micePmm_X_mean / df_X_mean - 1)
      df_bias_imp[nrow(df_bias_imp) + 1,] = c(dist, 'micePmm', 'X', micePmm_X_rel_bias)
      
      micePmm_XZ_mean <- mean( micePmm_imp $'XZ')
      micePmm_XZ_rel_bias <- (micePmm_XZ_mean / df_XZ_mean - 1)
      df_bias_imp[nrow(df_bias_imp) + 1,] = c(dist, 'micePmm', 'XZ', micePmm_XZ_rel_bias)
    }
  }
  return(df_bias_imp)
}

#-------------------------------------------
#-------------------------------------------
relb_scen3_100cy <- imp_bias(n_cycle = 100, scenar = 'lr_inter')
relb_scen3_100cy$rel_bias <- as.numeric(relb_scen3_100cy$rel_bias)
relb_scen3_100cy$dist <- fct_rev(factor(relb_scen3_100cy$dist, levels = c('norm', 'unif', 'lognorm1', 'lognorm2', 'gamma1', 'gamma2', 'norm_mix1', 'norm_mix2')))
relb_scen3_100cy


byX <- relb_scen3_100cy[which(relb_scen3_100cy$col == 'X'), ] 
ggplot(byX, aes(x = rel_bias, y = dist, color = estimator)) + geom_boxplot() + ggtitle("Scen.3, rel.bias by X, 100 cy.")  + xlim(-0.2, 0.15)

byXZ <- relb_scen3_100cy[which(relb_scen3_100cy$col == 'XZ'), ] 
ggplot(byXZ, aes(x = rel_bias, y = dist, color = estimator)) + geom_boxplot() + ggtitle("Scen.3, rel.bias by XZ, 100 cy.")  + xlim(-0.18, 0.15)


write.csv(relb_scen3_100cy, "/Users/ash/Desktop/MICE/R/H-G/rel_bias/scen3_100cy.csv", row.names = FALSE)



