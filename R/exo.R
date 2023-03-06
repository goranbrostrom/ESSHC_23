## The 'exo' distribution, given by a model for exogeneous
## infant mortality.

tc <- 365 / (log(366))^2 # scaling constant

g <- function(x, tc = 365 / (log(366))^2){
    (log(1 + x))^3 * tc
}
dg <- function(x, tc = 365 / (log(366))^2){
   3 * (log(1 + x))^2 * tc / (1 + x)
}
##
## Survival function:
Sexo <- function(x, lambda){
   exp(-lambda * g(x))
}
## Density function:
fexo <- function(x, lambda){
   lambda * dg(x) * exp(-lambda * g(x))
}
## Hazard function:
hexo <- function(x, lambda){
   lambda * dg(x)
}
## Cumulative hazard function:
Hexo <- function(x, lambda){
   lambda * g(x)
}
