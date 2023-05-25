source("/Users/ash/Desktop/MICE/R/gener_dt.R")
source("/Users/ash/Desktop/MICE/R/ampt.R")
source("/Users/ash/Desktop/MICE/R/metrics.R")
source("/Users/ash/Desktop/MICE/R/imputers.R")

library("dplyr")
library("locfit")
library(ggplot2)
library(forcats)
library("lattice")

Iv_dropna <- read.csv("/Users/ash/Desktop/MICE/Ivanovo_dropna.csv")
df <- Iv_dropna



metr_func <- function(X_miss, imp_name, df, metrica, col2, df_fin, scenar){
  
  meth_imp <- imputer(X_miss, imp_name)
  meth_res <- metr(df, X_miss, meth_imp, metrica, col2, scenar)
  
  df_fin[nrow(df_fin) + 1,] = c(imp_name, col2, meth_res[2])
  
  return(df_fin)
}


imp <- function(metrica, varrs, col2){
  
  df_fin <- data.frame(matrix(ncol = 3, nrow = 0))
  colnames(df_fin) <- c('estimator', 'col', metrica)
  
  for (n_c in seq(from = 1, to = 20, by = 1)){
    
    scenar = 'lr_quadr'
    
    
    X_miss = ampute(df, prop = 0.25, mech = "MAR", type = "RIGHT", patterns = varrs)$amp
    
    #missForest
    #----------------
    print("missForest")
    df_fin <- metr_func(X_miss, 'missForest', df, metrica, col2, df_fin, scenar)
    
    #rfcont10_10
    #----------------
    print("rfcont10")
    df_fin <- metr_func(X_miss, 'rfcont10', df, metrica, col2, df_fin, scenar)
    
    # #micePMM
    # #----------------
    print("micePMM")
    df_fin <- metr_func(X_miss, 'micePMM', df, metrica, col2, df_fin, scenar)
    
  }
  
  return(df_fin)
}
alltoge <- function(df_R, df_PY){
  df_tog <- rbind(df_R, df_PY)
  df_tog$estimator <- fct_rev(factor(df_tog$estimator, levels = c('mice_br', 'missForest', 'rfcont10', 'mice_rf', 'micePMM', 'knn')))
  return(df_tog)
}

plott <- function(res, coll, metrica){
  
  byCol <- res[which(res$col == coll), ]
  g_X <- ggplot(byCol, aes(x = res, y = estimator, color = estimator)) + geom_boxplot() + ggtitle(paste(metrica, ". MAR, by ", coll, "th column", sep = "")) + xlab(metrica)+ theme_bw()
  
  return(g_X)
}

 
#vars_all <- c(5, 12, 13, 14, 15, 16, 17)
pat_all <- c(1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0)

imp_all_12 = imp('rel_bias', pat_all, "X12")
colnames(imp_all_12)[3] <- 'res' 
imp_all_12
plott(imp_all_12, "X12", 'rel_bias')
#write.csv(imp_all_12, "/Users/ash/Desktop/MICE/R/H-G csv/Iv_relb_all12_R_20cy.csv", row.names = FALSE)


imp_all_13 = imp('rel_bias', pat_all, "X13")
colnames(imp_all_13)[3] <- 'res' 
imp_all_13
plott(imp_all_13, "X13", 'rel_bias')
#write.csv(imp_all_13, "/Users/ash/Desktop/MICE/R/H-G csv/Iv_relb_all13_R_20cy.csv", row.names = FALSE)


imp_all_14 = imp('rel_bias', pat_all, "X14")
colnames(imp_all_14)[3] <- 'res' 
plott(imp_all_14, "X14", 'rel_bias')
#write.csv(imp_all_14, "/Users/ash/Desktop/MICE/R/H-G csv/Iv_relb_all14_R_20cy.csv", row.names = FALSE)




#Ivanovo PY+R
#--------12
Iv_relb_all12_R <- read.csv("/Users/ash/Desktop/MICE/R/H-G csv/Iv_relb_all12_R_20cy.csv")
Iv_relb_all12_R[Iv_relb_all12_R$col == 'X12',]$col = '12'
Iv_relb_all12_PY <- read.csv("/Users/ash/Desktop/MICE/PY/fin_res_csv/Iv_relb_all12_PY_20cy.csv")
colnames(Iv_relb_all12_PY)[3] <- 'res' 
Iv_relb_all12 <- alltoge(Iv_relb_all12_R, Iv_relb_all12_PY)

plott(Iv_relb_all12, "12", 'rel_bias')


#--------13
Iv_relb_all13_R <- read.csv("/Users/ash/Desktop/MICE/R/H-G csv/Iv_relb_all13_R_20cy.csv")
Iv_relb_all13_R[Iv_relb_all13_R$col == 'X13',]$col = '13'
Iv_relb_all13_PY <- read.csv("/Users/ash/Desktop/MICE/PY/fin_res_csv/Iv_relb_all13_PY_20cy.csv")
colnames(Iv_relb_all13_PY)[3] <- 'res' 
Iv_relb_all13 <- alltoge(Iv_relb_all13_R, Iv_relb_all13_PY)

plott(Iv_relb_all13, "13", 'rel_bias')


#--------14
Iv_relb_all14_R <- read.csv("/Users/ash/Desktop/MICE/R/H-G csv/Iv_relb_all14_R_20cy.csv")
Iv_relb_all14_R[Iv_relb_all14_R$col == 'X14',]$col = '14'
Iv_relb_all14_PY <- read.csv("/Users/ash/Desktop/MICE/PY/fin_res_csv/Iv_relb_all14_PY_20cy.csv")
colnames(Iv_relb_all14_PY)[3] <- 'res' 
Iv_relb_all14 <- alltoge(Iv_relb_all14_R, Iv_relb_all14_PY[Iv_relb_all14_PY$col == '14',])

plott(Iv_relb_all14, "14", 'rel_bias')
