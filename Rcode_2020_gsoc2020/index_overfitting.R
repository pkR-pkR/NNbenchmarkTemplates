

newcars <- cars
newcars$speed <- cars$speed + rnorm(NROW(cars), 0, 1/5)
newcars <- newcars[order(newcars$speed), ]


smoothfit <- lm(dist ~ 0+ speed + I(speed^2), data=newcars)

fitlocalreg <- loess(dist ~ speed, data=newcars, degree=0, span=0.25)
fitlocalreg2 <- loess(dist ~ speed, data=newcars, degree=1, span=0.25)
fitlocalreg3 <- loess(dist ~ speed, data=newcars, degree=2, span=0.25)


plot(newcars, main="Loess - span = 0.25", ylab="distance", xlab="speed")
lines(newcars$speed, fitted(fitlocalreg), col="red")
lines(newcars$speed, fitted(fitlocalreg2), col="blue", lty=2, lwd=1.5)
lines(newcars$speed, fitted(fitlocalreg3), col="green", lty=3, lwd=1.5)
lines(newcars$speed, fitted(smoothfit), col="orange", lty=4, lwd=1.5)



abslag <- function(object)
{
  res <- sapply(1:10, function(l) c("lag"=l, "L1"=mean(abs(diff(fitted(object), lag=l)))))
  colnames(res) <- paste0("lag=", 1:NCOL(res))
  t(res)
}

#L1 norm of lagged values
matplot(abslag(fitlocalreg)[,2], type="l", ylim=c(2, 18))
lines(abslag(fitlocalreg2)[,2], lty=2)
lines(abslag(fitlocalreg3)[,2], lty=3)
lines(abslag(smoothfit)[,2], lty=4)

#relative L1 norm
sum(abs(fitted(fitlocalreg) - fitted(smoothfit)))
sum(abs(fitted(fitlocalreg2) - fitted(smoothfit)))
sum(abs(fitted(fitlocalreg3) - fitted(smoothfit)))
