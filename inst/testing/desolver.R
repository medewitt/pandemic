library(deSolve)
library(RColorBrewer)

#https://en.wikipedia.org/wiki/Timeline_of_the_2019%E2%80%9320_Wuhan_coronavirus_outbreak#Cases_Chronology_in_Mainland_China
Infected <- cumsum(dat_red$cases)/.3
#Infected <- c(45,62,121,198,291,440,571,830,1287,1975,
#              2744,4515,5974,7711,9692,11791,14380,17205,20440,
#              24324,28018,31161,34546,37198,40171,42638,44653)
day <- 0:(length(Infected)-1)
N <- 11000000 

init <- c(S = N-Infected[1], I = Infected[1], R = 0)

# model function
SIR2 <- function(time, state, parameters) {
	par <- as.list(c(state, parameters))
	with(par, { 
		beta  <- const/(1-1/R0)  
		gamma <- const/(R0-1)  
		dS <- -(beta * (S/N)      ) * I 
		dI <-  (beta * (S/N)-gamma) * I 
		dR <-  (             gamma) * I
		list(c(dS, dI, dR))
	})
}

### Two functions RSS to do the optimization in a nested way
RSS.SIRMC2 <- function(R0,const) {
	parameters <- c(const=const, R0=R0)
	out <- ode(y = init, times = day, func = SIR2, parms = parameters)
	fit <- out[ , 3]
	RSS <- sum((Infected_MC - fit)^2)
	return(RSS)
}

RSS.SIRMC <- function(const) {
	optimize(RSS.SIRMC2, lower=1,upper=10^5,const=const)$objective
}

# wrapper to optimize and return estimated values
getOptim <- function() {
	opt1 <- optimize(RSS.SIRMC,lower=0,upper=1)
	opt2 <- optimize(RSS.SIRMC2, lower=1,upper=10^5,const=opt1$minimum)
	return(list(RSS=opt2$objective,const=opt1$minimum,R0=opt2$minimum))
}

# doing the nested model to get RSS
Infected_MC <- Infected
modnested <- getOptim()

rss <- sapply(seq(0.3,0.5,0.01), 
							FUN = function(x) optimize(RSS.SIRMC2, lower=1,upper=10^5,const=x)$objective)

plot(seq(0.3,0.5,0.01),rss)

optimize(RSS.SIRMC2, lower=1,upper=10^5,const=0.35)


# view
modnested

### plotting different values R0

const <- modnested$const
R0 <- modnested$R0

# graph
plot(-100,-100, xlim=c(0,80), ylim = c(1,6*10^4), log="", 
		 ylab = "infected", xlab = "days")
title(bquote(paste("scenario's for different ", R[0])), cex.main = 1)

### this is what your beta and gamma from the blog
beta = 0.6746089
gamma = 0.3253912
fit <- data.frame(ode(y = init, times = t, func = SIR, parms = c(beta,gamma)))$I
lines(t,fit,col=3)

# plot model with different R0
t <- seq(0,60,1)
for (R0 in c(modnested$R0,1.05, 1.06, 1.07,1.08,1.09,1.1)) {
	fit <- data.frame(ode(y = init, times = t, func = SIR2, parms = c(const,R0)))$I
	lines(t,fit,col=1+(modnested$R0==R0))
	text(t[501],fit[501],
			 bquote(paste(R[0], " = ",.(R0))),
			 cex=0.7,pos=4,col=1+(modnested$R0==R0))
}
const <- modnested$const
R0 <- modnested$R0
full_fit <- data.frame(ode(y = init, times = t, func = SIR2, parms = c(const,R0)))

combined <- full_fit %>% 
	mutate(time = row_number()-1) %>% 
	left_join(data.frame(time = 0:(length(Infected)-1), infected = Infected))
# plot observations
points(day,Infected, cex = 0.7)

combined %>% 
ggplot(aes(time))+
	geom_point(aes(y =infected))+
	geom_line(aes(y = I))
