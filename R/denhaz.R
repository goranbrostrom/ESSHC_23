denhaz <- function(p, header = ""){
   ## p = Prob(T <= 365 days)
   ##
   ## Uniform:
   ## S(365) = 1 - p; 
   ## S(t) = p * t / 365
   ## f * 365 = p, f = p / 365
   ##
   ## Same as in GBTB23:
   g <- function(x) (log(1 + x))^3 * 365 / (log(366))^3
   ##
   funif <- function(x) rep(p / 365, length(x))
   Sunif <- function(x) 1 - p * x / 365
   hunif <- function(x) p / (365 - p * x)
   ## Exponential:
   ## S(365) = exp(-b*365) = 1 - p, b = - log(1 - p) / 365
   ## f(t) = b * exp(-t * b), 0 < t < 365
   ##
   b <- -log(1 - p) / 365
   fexp <- function(x) b * exp(-x * b)
   Sexp <- function(x) exp(-x * b)
   hexp <- function(x) rep(b, length(x))
   ##
   expneofrac <- (1 - Sexp(g(30))) / (1 - Sexp(g(365)))  
   unifneofrac <- (1 - Sunif(g(30))) / (1 - Sunif(g(365)))
   t <- seq(0, 365, length = 1000)
   if (TRUE){
   plot(t, 1000 * funif(t), type = "l", ylim = c(0, 1.5 * 1000 * max(fexp(t))), col = "blue",
        axes = FALSE, xlab = "g(Days)", ylab = "1000 x density", main = header)
   }
   text(g(200), 1200 * max(fexp(t)), "Uniform", col = "blue")
   text(g(200), 500 * max(fexp(t)), "Exponential", col = "red")
  ## plot(t, Sunif(t), type = "l", ylim = c(1 - p, 1), col = "blue",
    ##    axes = FALSE, xlab = "g(Days)", ylab = "")
   axis(1, at = c(0, g(30), 365), labels = c("0", "g(30)", "365"))
   axis(2, las = 1)
   box()
   lines(t, 1000 * fexp(t), col = "red")
   ##lines(t, Sexp(t), col = "red")
   abline(h = 0, v = 0)
   abline(v = g(30), lty = 3, col = "darkgreen")
   list(expneofrac = expneofrac, unifneofrac = unifneofrac)
}