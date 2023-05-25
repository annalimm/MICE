#library("hydroGOF")

# nrmse <- function(ximp, xmis, xtrue, col){
#   mis <- is.na(xmis[[col]])
#   res <- sqrt(mean((ximp[[col]][mis] - xtrue[[col]][mis])^{2}) / stats::var(xtrue[[col]][mis]))
#   
#   return(res)
# }

nrmse_func <- function(ximp, xmis, xtrue){
  mis <- is.na(xmis)
  res <- sqrt(mean((ximp[mis] - xtrue[mis])^{2}) / stats::var(xtrue[mis]))
  
  return(res)
}


metr <- function(df_tr, X_miss, df_imp, metr_name, col_2, scenar){
  
  metr_col2 <- NULL
  
  if (metr_name == 'rel_bias'){
    
    df_X_mean <- mean(df_tr$X)
    df_col2_mean <- mean(df_tr[[col_2]])
    
    df_imp_X_mean <- mean( df_imp $X)
    df_imp_col2_mean <- mean(df_imp[[col_2]])
    
    metr <- (df_imp_X_mean / df_X_mean - 1)
    metr_col2 <- (df_imp_col2_mean / df_col2_mean - 1)
    
    
  } else if (metr_name == 'nrmse'){
    metr <- nrmse_func(ximp = df_imp, xmis = X_miss, xtrue = df_tr)
    # metr <- nrmse(ximp = df_imp, xmis = X_miss, xtrue = df_tr, col = 'X')
    # metr_col2 <- nrmse(ximp = df_imp, xmis = X_miss, xtrue = df_tr, col = col_2)

  } else if (metr_name == 'relb_coef'){
    
    if (scenar == 'lr_quadr' || scenar == 'lr_inter'){
      beta_imp_X <- (summary(lm(Y ~ ., df_imp))$coefficients)[ , 1]["X"]
      beta_tr_X <- (summary(lm(Y ~ ., df_tr))$coefficients)[ , 1]["X"]
      metr <- (beta_imp_X - beta_tr_X) / beta_tr_X
      
      beta_imp_col2 <- (summary(lm(Y ~ ., df_imp))$coefficients)[ , 1][col_2]
      beta_tr_col2 <- (summary(lm(Y ~ ., df_tr))$coefficients)[ , 1][col_2] 
      metr_col2 <- (beta_imp_col2 - beta_tr_col2) / beta_tr_col2
    } else {
      beta_imp_X <- (summary(glm(Y ~ ., df_imp, family = 'binomial'))$coefficients)[ , 1]["X"]
      beta_tr_X <- (summary(glm(Y ~ ., df_tr, family = 'binomial'))$coefficients)[ , 1]["X"]
      metr <- (beta_imp_X - beta_tr_X) / beta_tr_X
      
      beta_imp_col2 <- (summary(glm(Y ~ ., df_imp, family = 'binomial'))$coefficients)[ , 1][col_2]
      beta_tr_col2 <- (summary(glm(Y ~ ., df_tr, family = 'binomial'))$coefficients)[ , 1][col_2] 
      metr_col2 <- (beta_imp_col2 - beta_tr_col2) / beta_tr_col2
    }
  }
  
  
  return(list(metr, metr_col2))

}


# source("/Users/ash/Desktop/MICE/R/ampt.R")
# source("/Users/ash/Desktop/MICE/R/gener_dt.R")
# df <- gen_dt(X_distr = 'norm', scenar = 'lr_inter')
# df
# (summary(lm(Y ~ X, df))$coefficients)[ , 1]
# library("car")
# scatterplot(Y ~ X, data = df, 
#             smoother = FALSE, grid = FALSE, frame = FALSE)
# 
# X_miss = amp(df)
# X_miss
# 
# df_imp <- missForest(X_miss, maxiter = 10, ntree = 10)$ximp


# mean(df$'X2')
# mean(df[[col2]])
