library("dplyr")
library("locfit")

# rmix.norm <- function (n,alpha,mu,sigma=rep(1,length(alpha))) 
#     #n:      sample size.
#     #alpha:  vector of mixture probabilities.
#     #mu:     vector of means of each component.
#     #sigma:  vector of standard deviation of each component.
#   {
#     m=length(alpha)
#     alpha=alpha/sum(alpha)
#     data=c()
#     
#     nindex=rmultinom(1,n,alpha)
#     
#     for( i in 1:m)
#       data=c(data,rnorm(nindex[i],mu[i],sigma[i]))
#     data
# }

# df <- data.frame(X1, X2)
# ggplot() + geom_density(data = df, aes(x = X1)) + geom_density(data = df, aes(x = X2)) + xlim(0, 8)



gen_dt <- function(X_distr, scenar){
  z <- NULL
  
  if (scenar == 'logr_quadr' || scenar == 'logr_inter'){
    n <- 1000
  } else {
    n <- 500}
  
  if (X_distr == 'norm'){
    X <- rnorm(n = n, mean = 4, sd = 1)
    
  }else if (X_distr == 'unif'){
      X <- runif(n = n, min = 0, max = 8)
    
  }else if (X_distr == 'lognorm1'){
      X <- rlnorm(n = n, meanlog = 0, sdlog = sqrt(0.25))
    
  }else if (X_distr == 'lognorm2'){
      X <- rlnorm(n = n, meanlog = 0, sdlog = sqrt(0.625))
    
  }else if (X_distr == 'gamma1'){
      X <- rgamma(n = n, shape = 1, rate = 1)
    
  }else if (X_distr == 'gamma2'){
      X <- rgamma(n = n, shape = 2, rate = 0.5)
    
  }else if (X_distr == 'norm_mix1'){
    #7 N(1, 1), N(6, 3)
    # X <- rmix.norm(n = n, c(0.5, 0.5), c(1, 6), c(1, 3))
    components <- sample(1:2, prob = c(0.5, 0.5), size = n, replace=TRUE)
    mus <- c(1, 6)
    sds <- sqrt(c(1, 3))
    X <- rnorm(n = n, mean = mus[components], sd = sds[components])
    #X <- pmin(pmax(mixnorm_1, quantile(mixnorm_1, .025)), quantile(mixnorm_1, .975))
    
  }else if (X_distr == 'norm_mix2'){
    #8  N(1, 1), N(6, 10)
    components <- sample(1:2, prob = c(0.5, 0.5), size = n, replace=TRUE)
    mus <- c(1, 6)
    sds <- sqrt(c(1, 10))
    X <- rnorm(n = n, mean = mus[components], sd = sds[components])
    #X <- pmin(pmax(mixnorm_2, quantile(mixnorm_2, .025)), quantile(mixnorm_2, .975))
  }
    
    
  #['lr_quadr', 'logr_quadr', 'lr_inter', 'logr_inter']
  #sc1
  if (scenar == 'lr_quadr'){
    eps <- rnorm(n = n, mean = 0, sd = 1)
    Y <- (2 + 2*X + X*X + eps)
  
  #sc2
  } else if (scenar == 'logr_quadr'){
    p <- expit(-1.2 + 0.1*X + 0.05*X*X)
    Y <- rbinom(n = n, size = 1, p = p)
  #sc3
  } else if (scenar == 'lr_inter'){
    z <- rnorm(n = n, mean = 4, sd = 2)
    eps <- rnorm(n = n, mean = 0, sd = 1)
    Y <- (2 + X + X*z + z + eps)
  #sc4
  } else if (scenar == 'logr_inter'){
    z <- rnorm(n = n, mean = 4, sd = 2)
    p <- expit(-2 + 0.5*X - 0.0625*X*z + 0.25*z)
    Y <- rbinom(n = n, size = 1, p = p)
  }
  

  if (is.null(z)){
    df <- data.frame(X, Y, X*X)
    colnames(df) <- c("X", "Y", "X2")
  } else {
    df <- data.frame(X, Y, z, X*z)
    colnames(df) <- c("X", "Y", "Z", "XZ")
  }

  return(df)
}
