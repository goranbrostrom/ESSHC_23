drawBB2 <- function(dat, main = "", ylim, C = 365 / (log(366))^3, 
                    printLegend = TRUE){
    
    ## dat is a "survival data frame with enter, exit (natural scale) and event.
    g <- function(x) {
        C * (log(x + 1))^3
    }
    dat$enter <- g(dat$enter)
    dat$exit <- g(dat$exit)
    post <- eha::age.window(dat, c(g(28), g(365)))
    rate <- with(post, sum(event) / sum(exit - enter))
    ## Only change wrt drawBB is that all Cumulative hazards start at (0, 0).    
    
   # rs is the output from eha::risksets
   # Assumed that age data are on the "g" scale (in days)
   # Error on line 11 ("cumsum  ...") fixed
   oldpar = par(cex = 0.7)
   fit <- coxreg(Surv(enter, exit, event) ~ 1, data = dat)
   xxx <- hazards(fit)
   x <- c(0, xxx[[1]][, 1])
   y <- cumsum(c(0, xxx[[1]][, 2]))
   if (missing(ylim)){
       plot(x, y, type = "s", main = main, xlab = "Day", axes = FALSE, 
            ylab = "Cumulative hazards", lwd = 2)
   }else{
       plot(x, y, type = "s", main = main, xlab = "Day", axes = FALSE, 
            ylab = "Cumulative hazards", lwd = 2, ylim = ylim)
   }
   lines(c(0, g(365)), c(0, rate * g(365)), lty = 2, col = "blue", lwd = 2)
   att <- c(0, 28, 91, 183, 274, 365)
   axis(1, at = g(att), labels = att)
   abline(v = g(28), lty = 2, col = "darkgreen", lwd = 1.5)
   y0 <- rate * g(28)
   ##cat("rate = ", rate, "g(28) = ", g(28), "y0 = ", y0, "\n")
   ##xxx <- max(xx[[1]])
   n <- max(which(x <= g(28)))
   y1 <- y[n]
   red <- y[1:n] - x[1:n] * rate
   redall <- y - x * rate
   lines(x[1:n], red, col = "red", type = "s", lty = 4, lwd = 2)
   lines(x, redall, col ="magenta", type = "s", lty = 3, lwd = 2)
   y2 <- red[n]
   y3 <- max(y)
   y4 <- rate * g(365)
   axis(2, at = round(c(0, y0, y1, y2, y3, y4), 3), las = 1)
   box()
   lines(c(0, g(28)), c(y0, y0), lty = 3, lwd = 0.5, col = "darkgreen")
   lines(c(0, g(28)), c(y1, y1), lty = 3, lwd = 0.5, col = "darkgreen")
   lines(c(0, g(365)), c(y2, y2), lty = 3, lwd = 1.5, col = "darkgreen")
   abline(h = c(y3, y4), lty = 3, lwd = 0.5, col = "darkgreen")
   abline(v = 0, lwd = 0.5)
   lines(c(0, g(28)), c(0, 0), lwd = 0.5)
   if (printLegend){
        legend("bottomright", legend = c("Total", "Exogenous", "Endogenous"),
           lty = c(1, 2, 4), col = c("black", "blue", "red"))
   }
###########################################################################   
     if (FALSE){
   n <- rs$size[1]
   x <- c(0, rs$risktimes)
   y <- c(0, cumsum(rs$n.events / rs$size)) # Nominator differ from "BP"
   if (missing(ylim)){
      plot(x, y, type = "l", col = "blue", axes = FALSE, xlab = "Day", 
           ylab = "Cumulative hazards", main = main)
   }else{
      plot(x, y, type = "l", col = "blue", axes = FALSE, xlab = "Day",
           ylab = "Cumulative hazards", main = main, ylim = ylim)
   }
   att <- c(0, 28, 91, 183, 274, 365)
   axis(1, at = g(att), labels = att)
   
   abline(h = 0, v = 0)
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
   }
}