library("dplyr")
library("locfit")
library("xkcdcolors")
library(ggplot2)
theme_set(theme_bw())

n <- 1000000

pltt <- function(dist_1, dist_2){
  col = '#015482'
  df <- data.frame(dist_1, dist_2)
  ggplot() + geom_density(data = df, aes(x = dist_1), color = col, size = 1.15) + geom_density(data = df, aes(x = dist_2), linetype = "twodash", size = 1.15, color = col) + theme(axis.title = element_blank(), axis.text = element_text(size = 14))
}

norm <- rnorm(n = n, mean = 4, sd = 1)
unif<- runif(n = n, min = 0, max = 8)
pltt(norm, unif) + xlim(0, 8) + theme(axis.title = element_blank())
  

lognorm1 <- rlnorm(n = n, meanlog = 0, sdlog = sqrt(0.25))
lognorm2 <- rlnorm(n = n, meanlog = 0, sdlog = sqrt(0.625))
pltt(lognorm1, lognorm2) + xlim(0, 8)                           


gamma1 <- rgamma(n = n, shape = 1, rate = 1)
gamma2 <- rgamma(n = n, shape = 2, rate = 0.5)
pltt(gamma1, gamma2) + xlim(0, 8) 


components <- sample(1:2, prob = c(0.5, 0.5), size = n, replace=TRUE)
mus <- c(1, 6)
sds1 <- sqrt(c(1, 3))
sds2 <- sqrt(c(1, 10))

mixnorm_1 <- rnorm(n = n, mean = mus[components], sd = sds1[components])
mixnorm_1 <- pmin(pmax(mixnorm_1, quantile(mixnorm_1, .025)), quantile(mixnorm_1, .975))

mixnorm_2 <- rnorm(n = n, mean = mus[components], sd = sds2[components])
mixnorm_2 <- pmin(pmax(mixnorm_2, quantile(mixnorm_2, .025)), quantile(mixnorm_2, .975))
pltt(mixnorm_1, mixnorm_2) #+ xlim(0, 8) #  + ylim(0, 0.24) 





