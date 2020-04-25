library(deSolve)
sirmod = function(t, y, parms) {
	# Pull state variables from y vector
	S = y[1]
	I = y[2]
	R = y[3]
	# Pull parameter values from parms vector
	beta = parms["beta"]
	mu = parms["mu"]
	gamma = parms["gamma"]
	N = parms["N"]
	# Define equations
	dS = mu * (N - S) - beta * S * I/N
	dI = beta * S * I/N - (mu + gamma) * I
	dR = gamma * I - mu * R
	res = c(dS, dI, dR)
	# Return list of gradients
	list(res)
}
times = seq(0, 26, by = 1/10)
parms = c(mu = 0, # No Deaths or Births
					N = 1,  # For Fraction
					beta = 2, # Tranmission Rate
					gamma = 1/2 # Two Weeks
					)
start = c(S = 0.999, I = 0.001, R = 0)
out = ode(y = start, times = times, func = sirmod, 
					parms = parms)
out=as.data.frame(out)
head(round(out, 3))
plot(x = out$time, y = out$S, ylab = "Fraction", 
		 xlab = "Time", type = "l")
lines(x = out$time, y = out$I, col = "red")
lines(x = out$time, y = out$R, col = "green")
#Calculate R0
R0 = parms["beta"]/(parms["gamma"]+parms["mu"])

#Adjust margins to accommodate a second right axis
par(mar = c(5,5,2,5))
#Plot state variables
plot(x = out$time, y = out$S, ylab = "Fraction",
		 xlab = "Time",  type = "l")
lines(x = out$time, y = out$I, col = "red")
lines(x = out$time, y = out$R, col = "green")

#Add vertical line at turnover point
xx = out$time[which.max(out$I)]
lines(c(xx,xx), c(1/R0,max(out$I)), lty = 3)

#prepare to superimpose 2nd plot
par(new = TRUE)
#plot effective reproductive ratio (w/o axes)
plot(x = out$time, y = R0*out$S, type = "l", lty = 2,
		 lwd = 2, col = "black", axes = FALSE, xlab = NA, 
		 ylab = NA, ylim = c(-.5, 4.5))
lines(c(xx, 26), c(1,1), lty = 3)
#Add right-hand axis for RE
axis(side = 4)
mtext(side = 4, line = 4, expression(R[E]))
#Add legend
legend("right", legend = c("S", "I", "R", 
													 expression(R[E])), lty = c(1,1,1, 2),  
			 col = c("black", "red", "green", "black"))
library(rootSolve)
equil=runsteady(y=c(S=1-1E-5, I=1E-5, R=0), 
								times=c(0,1E5), func=sirmod, parms=parms)
round(equil$y, 3)


#Candidate values for R0 and beta
R0 = seq(0.1, 5, length=50)
betas= R0 * 1/2
#Vector of NAs to be filled with numbers
f = rep(NA, 50)
#Loop over i from 1, 2, ..., 50
for(i in seq(from=1, to=50, by=1)){
	equil=runsteady(y=c(S=1-1E-5, I=1E-5, 
											R=0), times=c(0,1E5), func=sirmod, 
									parms=c(mu=0, N=1, beta=betas[i], gamma=1/2))
	f[i]=equil$y["R"]
}
plot(R0, f, type="l", xlab=expression(R[0]))
curve(1-exp(-x), from=1, to=5, add=TRUE, col="red")
#Define function
fn=function(x, R0){
	exp(-(R0*(1-x))) - x
}
1-uniroot(fn, lower = 0, upper = 1-1E-9, 
					tol = 1e-9, R0=2)$root
#check accuracy of approximation:
exp(-2)-uniroot(fn, lower = 0, upper = 1-1E-9, 
								tol = 1e-9, R0=2)$root

