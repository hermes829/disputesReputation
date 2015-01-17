# # http://people.su.se/~ma/clustering.pdf

# # Cluster-robust standard errors
# clx <- function(fm, dfcw, cluster){
# 	library(sandwich)
# 	library(lmtest)
# 	M <- length(unique(cluster))
# 	N <- length(cluster)
# 	dfc <- (M/(M-1))*((N-1)/(N-fm$rank))
# 	u <- apply(estfun(fm), 2, function(x) tapply(x, cluster, sum))
# 	vcovCL <- dfc*sandwich(fm, meat=crossprod(u)/N)*dfcw
# 	coef_se <- coeftest(fm, vcovCL)
# 	list(vcovCL, coef_se)
# }

# Alternative Formulations with same result, in latter two dfcw=1

clx <- function(fm, time, cluster){
 library(sandwich)
 library(lmtest)
 T <- length(unique(time))
 N <- length(cluster)
 dfc <- (N)/(N - T)
 u <- apply(estfun(fm),2,
           function(x) tapply(x, cluster, sum))
 vcovCL <- dfc*sandwich(fm, meat=crossprod(u)/N)
 coef_se <- coeftest(fm, vcovCL)
 list(vcovCL, coef_se)
}
  
  
# cl   <- function(dat,fm, cluster){
           # require(sandwich, quietly = TRUE)
           # require(lmtest, quietly = TRUE)
           # M <- length(unique(cluster))
           # N <- length(cluster)
           # K <- fm$rank
           # dfc <- (M/(M-1))*((N-1)/(N-K))
           # uj  <- apply(estfun(fm),2, function(x) tapply(x, cluster, sum));
           # vcovCL <- dfc*sandwich(fm, meat=crossprod(uj)/N)
           # coeftest(fm, vcovCL) }


robust.se <- function(model, cluster){
 require(sandwich)
 require(lmtest)
 M <- length(unique(cluster))
 N <- length(cluster)           
 K <- model$rank
 dfc <- (M/(M - 1)) * ((N - 1)/(N - K))
 uj  <- apply(estfun(model), 2, function(x) tapply(x, cluster, sum));
 rcse.cov <- dfc * sandwich(model, meat = crossprod(uj)/N)
 rcse.se <- coeftest(model, rcse.cov)
 list(rcse.cov, rcse.se)
 }
