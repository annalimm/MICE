library(missForest)

impute_missForest <- function(X_miss){
    imp <- missForest(X_miss)
    return(imp[["ximp"]])
}
# impute_missF <- function(X_miss, maxit, m, seed, meth=NULL){
# #     X_miss <- droplevels(X_miss)
#   imp <- mice(X_miss, maxit = maxit, meth = meth, m=m, seed = seed, print = FALSE)
#   return(complete(imp))
# }