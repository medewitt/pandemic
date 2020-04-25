#http://web.stanford.edu/class/earthsys214/notes/fit.html
require(deSolve)
sir <- function(t,x,parms){
	S <- x[1]
	I <- x[2]
	R <- x[3]
	with(as.list(parms),
			 {
			 	dS <- -beta*S*I
			 	dI <- beta*S*I - nu*I
			 	dR <- nu*I
			 	res <- c(dS,dI,dR)
			 	list(res)
			 })
}
N <- 1e7
parms <- c(N=N,beta=0.001, nu = 1/7)
times <- seq(0,30,0.1)
x0 <- c(N,1,0)
stateMatrix <- ode(y=x0, times, sir, parms)
colnames(stateMatrix) <- c("time","S","I","R")
plot(stateMatrix[,"time"], stateMatrix[,"S"], type="l", lwd=2, 
		 xlab="Time", ylab="Population Size")
lines(stateMatrix[,"time"], stateMatrix[,"I"], col="red", lwd=2)
lines(stateMatrix[,"time"], stateMatrix[,"R"], col="green", lwd=2)
legend("right", c("S","I","R"), col=c("black","red","green"), lwd=2)

bombay <- dat_red$cases
cumbombay <- cumsum(bombay)
weeks <- 0:(length(bombay)-1)
plot(weeks, cumbombay, pch=16, xlab="Weeks", ylab="Cumulative Deaths")

require(bbmle)
# likelihood function
sirLL <- function(lbeta, lnu, logN, logI0) {
	parms <- c(beta=plogis(lbeta), nu=plogis(lnu))
	x0 <- c(S=exp(logN), I=exp(logI0), R=0)
	out <- ode(y=x0, weeks, sir, parms)
	SD <- sqrt(sum( (cumbombay-out[,4])^2)/length(weeks) )
	-sum(dnorm(cumbombay, mean=out[,4], sd=SD, log=TRUE))
}
# minimize negative-log-likelihood
fit <- mle2(sirLL, 
						start=list(lbeta=qlogis(1e-5), 
											 lnu=qlogis(.2), 
											 logN=log(1e7), logI0=log(1) ),  
						method="Nelder-Mead",
						control=list(maxit=1E5,trace=0),
						trace=FALSE)

summary(fit)

theta <- as.numeric(c(plogis(coef(fit)[1:2]),
											exp(coef(fit)[3:4])) )
parms <- c(beta=theta[1], nu = theta[2])
times <- seq(0,60,0.1)
x0 <- c(theta[3],theta[4],0)
stateMatrix1 <- ode(y=x0, times, sir, parms)
colnames(stateMatrix1) <- c("time","S","I","R")
plot(stateMatrix1[,"time"], stateMatrix1[,"R"], type="l", lwd=2, 
		 xaxs="i", xlab="Time", ylab="Cumulative Deaths")
points(weeks, cumbombay, pch=16, col="red")
