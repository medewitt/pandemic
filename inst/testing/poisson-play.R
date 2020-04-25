y <- c(100, 200, 500, 900, 1000, 1200, 1800, 6800)
t <- 1:length(y)

dat <- data.frame(y, t)

coef(lm(log(y)~t, data = subset(dat, t >=2 )))[["t"]]
coef(lm(log(y)~t, data = subset(dat, t >=3 )))[["t"]]
coef(lm(log(y)~t, data = subset(dat, t >=4 )))[["t"]]
coef(lm(log(y)~t, data = subset(dat, t >=5 )))[["t"]]
coef(lm(log(y)~t, data = subset(dat, t >=6 )))[["t"]]
coef(lm(log(y)~t, data = data.frame(y = dat$y[6:8], t = 6:8)))[["t"]]

log(2)/.86
log(2)/.04

rpois(100, exp(.22+5.83))
