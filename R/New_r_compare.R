source("/Users/ash/Desktop/MICE/R/gener_dt.R")
source("/Users/ash/Desktop/MICE/R/ampt.R")
source("/Users/ash/Desktop/MICE/R/metrics.R")
source("/Users/ash/Desktop/MICE/R/imputers.R")

library("dplyr")
library("locfit")
library(ggplot2)
library(forcats)
library("lattice")


distrs <- c('norm', 'unif', 'lognorm1', 'lognorm2', 'gamma1', 'gamma2', 'norm_mix1', 'norm_mix2')


metr_func <- function(X_miss, imp_name, df, metrica, col2, df_fin, dist, scenar){
  
  meth_imp <- imputer(X_miss, imp_name)
  meth_res <- metr(df, X_miss, meth_imp, metrica, col2, scenar)
  
  if (meth_res[2] == 'NULL'){
    df_fin[nrow(df_fin) + 1,] = c(dist, imp_name, 'X', meth_res[1])
  } else {
    df_fin[nrow(df_fin) + 1,] = c(dist, imp_name, 'X', meth_res[1])
    df_fin[nrow(df_fin) + 1,] = c(dist, imp_name, col2, meth_res[2])
  }
  
  return(df_fin)
}


imp <- function(n_cycle, scenar, metrica){
  
  if (scenar == 'lr_quadr' || scenar == 'logr_quadr'){
    col2 <- "X2"
  } else {
    col2 <- "XZ"
  }
  
  df_fin <- data.frame(matrix(ncol = 4, nrow = 0))
  colnames(df_fin) <- c('dist', 'estimator', 'col', metrica)
  for (dist in distrs){
    print(dist)
    for (n_c in seq(from = 1, to = n_cycle, by = 1)){
      
      df <- gen_dt(X_distr = dist, scenar = scenar)
      X_miss <- amp(df)
      #X_miss <- amp_MNAR(df)
      
      #mean
      #----------------
      df_fin <- metr_func(X_miss, 'mean', df, metrica, col2, df_fin, dist, scenar)      
      
      #missForest
      #----------------
      df_fin <- metr_func(X_miss, 'missForest', df, metrica, col2, df_fin, dist, scenar)
      
      # #micePMM
      # #----------------
      df_fin <- metr_func(X_miss, 'micePMM', df, metrica, col2, df_fin, dist, scenar)
      
    }
  }
  return(df_fin)
}


print_res <- function(n_cy, scenar, metrica){
  res <- imp(n_cy, scenar, metrica)
  print(res)
  res[[metrica]] <- as.numeric(res[[metrica]])
  res$dist <- fct_rev(factor(res$dist, levels = c('norm', 'unif', 'lognorm1', 'lognorm2', 'gamma1', 'gamma2', 'norm_mix1', 'norm_mix2')))
  res$estimator <- fct_rev(factor(res$estimator, levels = c('missForest', 'micePMM', 'mean')))
  
  return(res)
}


plott <- function(res, sc_num, metrica){
  
  n_cy <- 100
  g_Col2 <- NULL
  
  if (sc_num == 1 || sc_num == 2){
    col2 <- "X2"
  } else {
    col2 <- "XZ"
  }
  
  byX <- res[which(res$col == 'X'), ]
  byCol2 <- res[which(res$col == col2), ]
  
  if (metrica == 'nrmse'){
    
    g_X <- ggplot(byX, aes(x = res, y = dist, color = estimator)) + geom_boxplot() + ggtitle(paste("Scen.", sc_num, ", ", metrica, ", ", n_cy, " cy.",  sep = "")) + xlab("nrmse")+ theme_bw()
    
  } else {
    g_X <- ggplot(byX, aes(x = res, y = dist, color = estimator)) + geom_boxplot() + ggtitle(paste("Scen.", sc_num, ", ", metrica, " by X, ", n_cy, " cy.",  sep = ""))  + xlab(metrica) + theme_bw()
    g_Col2 <- ggplot(byCol2, aes(x = res, y = dist, color = estimator)) + geom_boxplot() + ggtitle(paste("Scen.", sc_num, ", ", metrica, " by ", col2, ", ", n_cy, " cy.",  sep = "")) + xlab(metrica)+ theme_bw()
  }
  
  return(list(g_X, g_Col2))
}



#-------------------------------------------
#['lr_quadr', 'logr_quadr', 'lr_inter', 'logr_inter']
# #-------------------------------------------

i <- 1
for(scenar in list('lr_quadr', 'logr_quadr', 'lr_inter', 'logr_inter')){
  for (metric in list('rel_bias', 'nrmse', 'relb_coef')){
    print(paste(scenar, "+", metric, sep = ""))
    res <- print_res(100, scenar, metric)
    write.csv(res, paste("/Users/ash/Desktop/MICE/R/H-G csv/New_EB/R_", metric, "_sc", i,".csv" , sep = ""), row.names = FALSE)
  }
  i <- i+1
}


# res_relb <- print_res(100, 'lr_quadr', 'rel_bias')
# colnames(res_relb)[4] <- 'res' 
# fin_plot <- plott(res = res_relb, sc_num = 1, metrica = 'rel_bias')
# fin_plot[[1]] 
 
# write.csv(relb_coef_scen3_100cy, "/Users/ash/Desktop/MICE/R/H-G csv/relb_coef_scen3_100cy.csv", row.names = FALSE)
