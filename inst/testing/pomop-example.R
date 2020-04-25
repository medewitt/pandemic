library(pomp)
library(tidyverse)
niamey <- read.csv("http://kingaa.github.io/clim-dis/parest/niamey.csv")
ggplot(niamey,mapping=aes(x=biweek,y=measles,color=community))+
	geom_line()+geom_point()
fit1 <- lm(log(measles)~biweek,data=subset(niamey,biweek<=8&community=="A"))
summary(fit1)
slope <- coef(fit1)[2]; slope
slope.se <- coef(summary(fit1))[2,2]
1*slope.se

pomp(
	data=subset(niamey,community=="A",select=-community),
	times="biweek",t0=0,
	skeleton=vectorfield(
		Csnippet("
      DS = -Beta*S*I/N;
      DI = Beta*S*I/N-gamma*I;
      DR = gamma*I;")),
	rinit=Csnippet("
      S = S_0;
      I = I_0;
      R = N-S_0-I_0;"),
	statenames=c("S","I","R"),
	paramnames=c("Beta","gamma","N","S_0","I_0")) -> niameyA

sse <- function (params) {
	x <- trajectory(niameyA,params=params)
	discrep <- x["I",,]-obs(niameyA)
	sum(discrep^2)
}
f1 <- function (beta) {
	params <- c(Beta=beta,gamma=1,N=50000,S_0=10000,I_0=10)
	sse(params)
}
beta <- seq(from=30,to=40,by=0.5)
SSE <- sapply(beta,f1)
beta.hat <- beta[which.min(SSE)]
plot(beta,SSE,type='l')
abline(v=beta.hat,lty=2)


coef(niameyA) <- c(Beta=beta.hat,gamma=1,N=50000,S_0=10000,I_0=10)
x <- trajectory(niameyA,format="data.frame")
ggplot(data=left_join(as.data.frame(niameyA),x,by='biweek'),
			 mapping=aes(x=biweek))+
	geom_line(aes(y=measles),color='black')+
	geom_line(aes(y=I),color='red')

beta <- seq(from=0,to=40,by=0.5)
SSE <- sapply(beta,f1)

plot(beta,SSE,type='l')
beta.hat <- beta[which.min(SSE)]
abline(v=beta.hat,lty=2)

coef(niameyA,"Beta") <- beta.hat
x <- trajectory(niameyA,format="data.frame")
dat <- left_join(as.data.frame(niameyA),x,by='biweek')

ggplot(dat,aes(x=biweek))+
	geom_line(aes(y=measles),color='black')+
	geom_line(aes(y=I),color='red')
