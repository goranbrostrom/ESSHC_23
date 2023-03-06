bio <- function(surv, timescale = "year", graph = FALSE){
   library(eha)
   ## surv is a data frame, containing a survival object in three columns:  
   ## enter, exit, event.
   ##
   g <- function(x) (log(x + 1))^3
   ginv <- function(x) exp(x^(1/3)) - 1
   #####
   
   #####
   if (timescale == "year") {
      surv <- age.window(surv, c(0, 1))
      surv[, "enter"]  <- 365 * surv[, "enter"]
      surv[, "exit"] <- 365 * surv[, "exit"]
   }else{
      surv <- age.window(surv, c(0, 365)) # Time scale in days
   }
   g <- function(x) (log(x + 1))^3
   ginv <- function(x) exp(x^(1/3)) - 1
   #####
  
   gsurv <- surv
   gsurv[, "enter"] <- g(surv[, "enter"])
   gsurv[, "exit"] <- g(surv[, "exit"]) 
   ######
  
   post <- age.window(surv, c(30, 365))
   gpost <- age.window(gsurv, c(g(30), g(365)))
   ##
   neo <- age.window(surv, c(0, 30))
   gneo <- age.window(gsurv, c(0, g(30)))
   #####
   ## postneo mortality, g-scale;
   gprate <- with(gpost, sum(event) / sum(exit - enter))
   gprate
   ## neo cum. hazards, g-scale
   x <- with(gneo, risksets(Surv(enter, exit, event), members = FALSE))
   x$risktimes <- c(0, x$risktimes)
   x$n.events <- c(0, x$n.events)
   x$size <- c(NROW(gneo), x$size)
   cumhaz <- cumsum(x$n.events / x$size)
   if (graph){
      plot(x$risktimes, cumhaz, type = "s", xlab = "Day", ylab = "Cum. hazards")
      lines(c(0, 40), c(0, gprate * 40))
   }
   ##
   endocum <- cbind(x$risktimes, cumhaz)
   colnames(endocum) <- c("age", "cumhas")
   list(exorate = gprate, endocum = endocum)
}