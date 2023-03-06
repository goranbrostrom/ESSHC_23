biometric <- function(surv, timescale = "day", graph = FALSE, C = 1){
   ## timescale can be "year", otherwise "day".
   library(eha)
   ## surv is a data frame, containing a survival object in three columns:  
   ## enter, exit, event.
   ##
   ## NOTE that indata is NOT "g-transformed"!
   ####
   g <- function(x) C * (log(x + 1))^3
   ginv <- function(x) exp((x/C)^(1/3)) - 1
   dg <- function(x) C * (log(x + 1))^2 /(x + 1)
   #####
   
   hexo <- function(x, lambda) {
      lambda * dg(x)
   }
   Hexo <- function(x, lambda){
      lambda * g(x)
   }
   Sexo <- function(x, lambda){
      exp(-Hexo(x, lambda))
   }
   ######################################
   
   ##### All calculations are done on "day" time-scale, then translated back to
   ##### the "original" time-scale ("year" or "day")
   if (timescale == "year") {
      ##surv <- age.window(surv, c(0, 1))
      surv[, "enter"]  <- 365 * surv[, "enter"]
      surv[, "exit"] <- 365 * surv[, "exit"]
   }
   nulls <- surv$exit <= 0
   surv$exit[nulls] <- 0.25 # Note: 'age.window' would remove deaths at age 0.
   surv <- age.window(surv, c(0, 365)) # Time scale in days.

   ## Estimation of lambda = rate in exogenous mortality.
   
   exorate <- function(surv){
      ## Use "postneo" data:
      surv <- age.window(surv, c(29.5, 365))
      ##pgrs <- with(surv, risksets(Surv(enter, exit, event), members = FALSE))
      ## g- transformed data is only used in "postneo" estimation (exogenous mort).
      ##gsurv <- surv
      surv[, "enter"] <- g(surv[, "enter"])
      surv[, "exit"] <- g(surv[, "exit"]) 
      ######
      lambda <- with(surv, sum(event) / sum(exit - enter)) # occ. / exp. rate
      pgrs <- with(surv, risksets(Surv(enter, exit, event), members = FALSE))
      list(lambda = lambda, pgrs = pgrs)
   }   
   ##
   rex <- exorate(surv)
   ## 
   ## Now, to the neonatal period, note: NATURAL time scale.
   ##
   neo <- age.window(surv, c(0, 30))
   neosurv <- with(neo, cbind(enter, exit, event))
   tot <- risksets(neosurv, members = FALSE)
   ##return(tot)
   prob.ex <- -diff(Sexo(c(0, tot$risktimes), rex$lambda)) 
   exo.deaths <- tot$size[1] * prob.ex
   endo.deaths <- tot$n.events - exo.deaths
   cumhaz <- cumsum(c(0, tot$n.events / tot$size))

   ##return(cumhaz)
   ##return(prob.ex)
   if (graph){
      par(lwd = 2, cex = 0.7)
      oldpar <- par(mfrow = c(1, 2))
      x <- c(0, tot$risktimes)
      
      ## Plot (1, 1):
      y <- cumhaz * 1000
      plot(x, y, type = "s", xlab = "Day", main = "Neonatal",
           ylab = "Cum. hazards per 1000", lty = 2, las = 1, ylim = c(0, 1.1 * max(y)))
      abline(h = 0)
      y <- Hexo(x, rex$lambda)
      lines(x, y * 1000, col = "blue")
      lines(x, cumhaz * 1000 - y * 1000, col = "red", type = "s")
      ##lines(c(0, 40), c(0, gprate * 40))
      legend("topleft", legend = c("Total", "Exogenous", "Endogenous"), 
             lty = c(2, 1, 1), col = c("black", "blue", "red"), cex = 0.7)
      
      
      ## Plot (1, 2):
      if (FALSE){
      plot(tot$risktimes, endo.deaths, type = "s", col = "red", las = 1,
           ylab = "No. of deaths per day", xlab = "Day", main = "Neonatal")
      lines(tot$risktimes, exo.deaths, type = "s", col = "blue")
      lines(tot$risktimes, endo.deaths + exo.deaths, lty = 3, col = "black", type = "s")
      legend("topright", legend = c("Endogenous", "Exogenous", "Total"), lty = c(1, 1, 2), 
             col = c("red", "blue", "black"), cex = 0.7)
      abline(h = 0)
      }
      ## Plot (2, 1):
      
      post <- rex$pgrs
      x <- ginv(post$risktimes)
      y <- Hexo(x, 1000 * rex$lambda)
      y <- y - y[1] # Start from zero
      plot(x, y, type = "l", col = "blue", 
           ylim = c(0, 1.1 * max(y)), main = "Post-neonatal", xlab = "Day", 
           ylab = "Cum hazards per 1000", axes = FALSE)
      axis(1, at = c(31, 100, 200, 300, 365))
      axis(2, las = 1)
      box()
      Hpost <- with(post, cumsum(n.events / size)) * 1000
      Hpost <- Hpost + 1000 * Hexo(30, rex$lambda)
      ##return(list(Hpost = Hpost, x = x))
      Hpost <- Hpost - Hpost[1]
      lines(x, Hpost, lty = 2, col = "black", type = "s")
      legend("topleft", legend = c("Exogenous", "Total"), 
             lty = c(1, 2), col = c("black", "blue"), cex = 0.7)
      abline(h = 0)
      
      ## Plot (2, 2):
      if (FALSE){
      ## 5-30 days
      x <- tot$risktimes[-(1:5)]
      
      plot(x, endo.deaths[-(1:5)], type = "s", col = "red", las = 1, 
           ylim = c(0, max(endo.deaths[-(1:5)] + exo.deaths[-(1:5)])), 
           ylab = "No. of deaths per day", xlab = "Day", main = "Neonatal, days 6-30")
      lines(x, exo.deaths[-(1:5)], type = "s", col = "blue")
      lines(x, endo.deaths[-(1:5)] + exo.deaths[-(1:5)], lty = 3, col = "black", type = "s")
      legend("topright", legend = c("Endogenous", "Exogenous", "Total"), lty = c(1, 1, 2), col = c("red", "blue", "black"))
      abline(h = 0)
      }
      ##
      
      par(oldpar)
   }
   ##
   ##endocum <- cbind(x$risktimes, cumhaz)
   ##colnames(endocum) <- c("age", "cumhaz")
   ##list(exorate = gprate, endocum = endocum)
}