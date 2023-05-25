library(mice)
library("CALIBERrfimpute")

impute_mice <- function(X_miss, maxit, m, seed, meth=NULL){
#     X_miss <- droplevels(X_miss)
  imp <- mice(X_miss, maxit = maxit, meth = meth, m=m, seed = seed, print = FALSE)
  return(complete(imp))
}


ampute_mice <- function(dt, prop, mech, type_=NULL){
    
    result <- ampute(dt, prop = prop, mech = mech, type = type_, weights = c(0, 0, 1), patterns = c(0, 1, 1))$amp
    
    return(result)
}