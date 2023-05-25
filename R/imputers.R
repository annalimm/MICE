library("missForest")
library(CALIBERrfimpute)
library("mice")
library("missMethods")



# mean of m mice datasets
#---------------------------
df_mean <- function(imp_mice){
  df_shape = dim(imp_mice[[1]])
  res <- data.frame(matrix(0, ncol = df_shape[2], nrow = df_shape[1]))
  
  for (imp in imp_mice){
    res <- res + imp
  }
  fin_df <- res / length(imp_mice)
  colnames(fin_df) <- colnames(imp_mice[[1]])
  return(fin_df)
}


# R RF methods
#---------------------------
mice.impute.rfdoove10 <- function(y, ry, x, ...){
  mice::mice.impute.rf(y = y, ry = ry, x = x, ntrees = 10)
}

mice.impute.rfcont10 <- function(y, ry, x, ...){
  CALIBERrfimpute::mice.impute.rfcont(
    y = y, ry = ry, x = x, ntree_cont = 10)
}

# domice5 <- function(missdata, functions, reps = 5){
#   mids <- mice(missdata, defaultMethod = functions,
#                m = reps, visitSequence = 'monotone',
#                printFlag = FALSE, maxit = 5)
#   
#   lapply(1:reps, function(x) complete(mids, x))
# }

domice10 <- function(missdata, functions, reps = 5){
  mids <- mice(missdata, defaultMethod = functions,
               m = reps, visitSequence = 'monotone',
               printFlag = FALSE, maxit = 10)
  
  lapply(1:reps, function(x) complete(mids, x))
}



# dif. imputers
#---------------
# ntree = 10
# mice m = 5
# maxit = 10(missFor), 10(mice, but in article they wrote 5. but 10 is closer to article plots)
#---------------------------
imputer <- function(X_miss, imputer_name){
  
  if (imputer_name == 'missForest'){
    imp <- missForest(X_miss, maxiter = 10, ntree = 10)$ximp
    
  # } else if (imputer_name == 'miceRF'){
  #   imp <- complete(mice(X_miss, m = 5, meth = 'rf', ntree = 10))
  #   
  # } else if (imputer_name == 'miceRFcont'){
  #   imp <- complete(mice(X_miss, m = 5, meth = 'rfcont'))
  
  # ntrees = 10, m = 5, maxit = 5
  # } else if (imputer_name == 'rfcont10_5it'){
  #   imp <- df_mean( domice5(X_miss, 'rfcont10') )
  #   
  # } else if (imputer_name == 'rfdoove10_5it'){
  #   imp <- df_mean( domice5(X_miss, 'rfdoove10') )
    
  # maxit = 10
  } else if (imputer_name == 'rfcont10'){
    imp <- df_mean( domice10(X_miss, 'rfcont10') )
    
  # } else if (imputer_name == 'rfdoove10_10it'){
  #   imp <- df_mean( domice10(X_miss, 'rfdoove10') )

    
  } else if (imputer_name == 'mean'){
    imp <- impute_mean(X_miss, type = "total")
    
  } else if (imputer_name == 'micePMM'){
    imp <- complete(mice(X_miss, m = 5, maxit = 5, meth = 'pmm',
                         printFlag = FALSE))
  }
  
  return(imp)
}

