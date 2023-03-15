drawBP <- function(rs, main = "", ylim){
   # rs is the output from eha::risksets
   # Assumed that age data are on the "g" scale (in days)
   
   oldpar <- par(cex = 0.7)
   n <- rs$size[1]
   x <- c(0, rs$risktimes)
   y <- c(0, cumsum(rs$n.events) / rs$size[1])
   if (missing(ylim)){
      plot(x, y, type = "l", col = "blue", axes = FALSE, xlab = "Day", 
           ylab = "Cum. death fraction", main = main, lwd = 2)
   }else{
      plot(x, y, type = "l", col = "blue", axes = FALSE, xlab = "Day",
           ylab = "Cum. death fraction", main = main, ylim = ylim, lwd = 2)
   }
   att <- c(0, 28, 91, 183, 274, 365)
   axis(1, at = g(att), labels = att)
   
   abline(h = 0, v = 0)
   ##abline(v = g(28), lty = 3, col = "red")
   
   abline(v = g(28), lty = 2, col = "magenta", lwd = 1.5)
   abline(v = c(g(91), g(183), g(274), g(365)), lty = 3, col = "red")
   n2 <- length(x)
   x2 <- x[n2]
   y2 <- y[n2]
   n1 <- min(which(x > g(28)))
   x1 <- x[n1]
   y1 <- y[n1]
   x0 <- 0
   y0 <- y1 - x1 * (y2 - y1) / (x2 - x1)
   axis(2, at = round(c(0, y0, y1, y2), 3), las = 1)
   box()
   lines(c(0, x2), c(y0, y2), type = "l", lty = 2, col = "darkgreen")
   par(oldpar)
}