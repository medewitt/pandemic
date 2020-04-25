library(nccovid)
library(tidyverse)
dat <- nccovid::get_covid_state()

dat_red <- dat %>% 
	group_by(date) %>% 
	summarise(cases = sum(cases_daily)) %>% 
	mutate(date = lubridate::mdy(date)) %>% 
	dplyr::filter(date> as.Date("2020-03-11")) %>% 
	mutate(day = row_number()) %>% 
	select(day, cases)

plot(dat_red$cases, type = "b")
plot(log(dat_red$cases))
fit1 <- lm(log(cases+1)~day,data=subset(dat_red, subset = day<=12))
summary(fit1)
slope <- coef(fit1)[2]; slope
slope.se <- coef(summary(fit1))[2,2]

pomp(
	data=dat_red,
	times="day",t0=0,
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
	paramnames=c("Beta","gamma","N","S_0","I_0")) -> nc

sse <- function (params) {
	x <- trajectory(nc,params=params)
	discrep <- x["I",,]-obs(nc)
	sum(discrep^2)
}
f1 <- function (beta) {
	params <- c(Beta=beta,gamma=1,N=50000,S_0=10000,I_0=10)
	sse(params)
}
beta <- seq(from=5,to=40,by=0.5)
SSE <- sapply(beta,f1)
beta.hat <- beta[which.min(SSE)]
plot(beta,SSE,type='l')
abline(v=beta.hat,lty=2)

coef(nc) <- c(Beta=beta.hat,gamma=1,N=50000,S_0=10000,I_0=10)
coef(nc,"Beta") <- beta.hat
x <- trajectory(nc,format="data.frame")
dat <- left_join(as.data.frame(nc),x,by='day')

ggplot(dat,aes(x=day))+
	geom_line(aes(y=cases),color='black')+
	geom_line(aes(y=I),color='red')
