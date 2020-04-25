# Load some required packages
##############
library(deSolve)
library(dplyr)
library(ggplot2)
library(rstan)
library(data.table)
library(brms)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
##############

# To simulate the data, we need to assign initial conditions.
# In practice, these will likely be unknown, but can be estimated from the data.

I0 <- 0.02    # initial fraction infected
S0 <- 1 - I0 # initial fraction susceptible
R0 <- 0

# Assign transmission and pathogen-induced death rates:
beta <- 0.60
gamma <- 0.10

# We will use the package deSolve to integrate, which requires certain data structures.
# Store parameters and initial values
# Parameters must be stored in a named list.
params <- list(beta = beta,
							 gamma = gamma)

# Initial conditions are stored in a vector
inits <- c(S0, I0, R0)

# Create a time series over which to integrate.
# Here we have an epidemic that is observed over t_max number of days (or weeks or etc).
t_min <- 0
t_max <- 50
times <- t_min:t_max

# We must create a function for the system of ODEs.
# See the 'ode' function documentation for further insights.
SIR <- function(t, y, params) {
	with(as.list(c(params, y)), {
		
		dS <- - beta * y[1] * y[2]
		
		dI <- beta * y[1] * y[2] - gamma * y[2]
		
		dR <- gamma * y[2]
		
		res <- c(dS,dI,dR)
		list(res)
	})
}

# Run the integration:
out <- ode(inits, times, SIR, params, method="ode45")

# Store the output in a data frame:
out <- as.data.table(out)
colnames(out) <- c("time", "S", "I", "R")

# quick plot of the epidemic
ggplot(out) +
	geom_line(aes(x=time,y=S), color = "black") +
	geom_line(aes(x=time,y=I), color = "red") +
	geom_line(aes(x=time,y=R), color = "blue")

sample_days <- 20 # number of days sampled throughout the epidemic
sample_n    <- 25 # number of host individuals sampled per day

# Choose which days the samples were taken. 
# Ideally this would be daily, but we all know that is difficult.
sample_time <- out[time > 0,][sample(.N,sample_days)][order(time)][,time]

# Extract the "true" fraction of the population that is infected on each of the sampled days:
sample_propinf <- out[time %in% sample_time,]

# Generate binomially distributed data.
# So, on each day we sample a given number of people (sample_n), and measure how many are infected.
# We expect binomially distributed error in this estimate, hence the random number generation.
sample_propinf[,sample_y := rbinom(sample_days,sample_n,I)]

# Attempt brms model
sample_propinf[,init_pop := as.integer(sample_n)]

myFuns <- "
real[] ode_SIR(real t,          // time
               real [] y,       // state
               real [] theta,   // parameters
               real [] x_r,     // real data
               int [] x_i) {    // integer data
               
                             real dydt[3];
                             
                             dydt[1] = - theta[1] * y[1] * y[2];                 // dS/dt = -beta*I*S
                             dydt[2] = theta[1] * y[1] * y[2] - theta[2] * y[2]; // dI/dt = beta*S*I - gamma*I
                             dydt[3] = theta[2] * y[2];                          // dR/dt = gamma*I
                             
                             return dydt;
                          }

real sir_process(real t,
                 real beta, 
                 real gamma,
                 real initfrac) {
  
                             real y[1,3];  
                             real y0[3]; // 3 initial conditions
                             real theta[2];
                             
                             y0[1] = 1 - initfrac;
                             y0[2] = initfrac;
                             y0[3] = 0;
                             
                             theta[1] = beta;
                             theta[2] = gamma;
                             
                             y = integrate_ode_rk45(ode_SIR,
                                                    y0,
                                                    0,
                                                    rep_array(t,1),
                                                    theta,
                                                    rep_array(0.0,0),
                                                    rep_array(1,1),
                                                    1.0E-10, 1.0E-10, 1.0E3);
                             
                             return y[1,2];
}"


sir_brms <- brm(
	bf(sample_y | trials(init_pop) ~ sir_process(time,
																							 exp(beta),
																							 exp(gamma),
																							 inv_logit(initfrac)),
		 beta ~ 1,
		 gamma ~ 1,
		 initfrac ~ 1,
		 nl = TRUE), 
	stanvars = stanvar(scode = myFuns, block = "functions"),
	data = sample_propinf,
	family = binomial(link = "identity"),
	prior = c(prior(normal(-2,0.5), nlpar = "beta"),
						prior(normal(-2,0.5), nlpar = "gamma"),
						prior(normal(0,4), nlpar = "initfrac")),
	chains = 4,
	cores = 4,
	inits = 0,
	iter = 1000,
	warmup = 500)

expose_functions(sir_brms,vectorize=TRUE)

pp_check(sir_brms,"intervals")

summary(sir_brms)
