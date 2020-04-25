library(deSolve)
library(dplyr)
library(rstan)
library(outbreaks)
library(bayesplot)
rstan_options(auto_write = TRUE)           
options(mc.cores = parallel::detectCores())
# https://github.com/anastasiachtz/COMMAND_stan/blob/master/SingleStrainStan.Rmd
onset <- influenza_england_1978_school$date    
onset <- dat_red$day    
cases <- dat_red$cases      #Number of students in bed
N = length(onset) # Number of days observed throughout the outbreak
pop = 10000000         # Population 
sample_time=1:N
# Modify data into a form suitable for Stan
flu_data = list(n_obs = N,
								n_theta = 2,
								n_difeq = 3,
								n_pop = pop,
								y = cases,
								t0 = 0,
								ts = sample_time,
								ts_2 = 1:60,
								N_pred = 60)
# Specify parameters to monitor
#deterministic models (Model 1, Model 2)
parameters = c("y_hat", "y_init", "theta",  "R_0")

#stochastic model (Model 3)

parameters_stoch = c("y_hat", "y_init", "theta", "kappa", 
										 "lambda", "phi", "s_sq", "sigma", "R_0")  


# compile the models ------------------------------------------------------
flu_model <- stan_model("flu.stan")
flu_model_2 <- stan_model("flu-2.stan")

# run it ---n_chains=5
#n_warmups=500
n_iter=1000
n_thin=50
set.seed(1234)
n_chains =2
# Set initial values:
ini_1 = function(){
	list(theta=c(runif(1,0,5), runif(1,0.2,0.4)), 
			 S0=runif(1,(pop-3)/pop,(pop-1)/pop))  
}
time.start_nuts1 <- Sys.time()
nuts_fit_1 = sampling(flu_model, data = flu_data, 
											pars = parameters, 
											init = ini_1, 
											chains = n_chains, 
											warmup = n_warmups, 
											iter = n_iter, thin=n_thin, seed=13219)
nuts_fit_2 = sampling(flu_model_2, data = flu_data, 
											#pars = parameters, 
											init = ini_1, 
											chains = n_chains, 
											#warmup = n_warmups, 
											cores = 2,
											iter = n_iter, thin=n_thin, seed=19)
time.end_nuts1 <- Sys.time()
duration_nuts1<- time.end_nuts1 - time.start_nuts1
nuts_fit_1_summary <- summary(nuts_fit_1, 
															pars = c("lp__", "theta[1]", "theta[2]", 
																			 "y_init[1]", "R_0"))$summary
print(nuts_fit_1_summary,scientific=FALSE,digits=2)
posts_1 <-  rstan::extract(nuts_fit_1)
posts_2 <-  rstan::extract(nuts_fit_2)
summary(nuts_fit_2)
summary(nuts_fit_2$y_hat_, 
				pars = c("lp__", "theta[1]", "theta[2]", 
								 "y_init[1]", "R_0"))$summary
# ---------------------------------------------------------------
rstan::check_divergences(nuts_fit_1)
posterior_1 <- as.array(nuts_fit_1)
color_scheme_set("viridis")
# Markov chain traceplots
mcmc_trace(posterior_1, pars="lp__")
mcmc_trace(posterior_1, pars=c("theta[1]", "theta[2]", "y_init[1]"))

# Kernel density estimates of each Markov chain separately, overlaid
mcmc_dens_overlay(posterior_1, pars=c("theta[1]", "theta[2]", "y_init[1]"))
#Central posterior uncertainty intervals
mcmc_intervals(posterior_1,pars = c("theta[1]", "theta[2]", "y_init[1]"))
mcmc_trace(posterior_1, pars="R_0")

mcmc_trace(posterior_1, pars="lambda")


# Model fitted values across the observed time period
fit_I_1 <- posts_1$y_hat[,,2]    # Fitted fraction of infected 
fit_SIR_1 <- fit_I_1*pop         # Fitted number of infected
median_I_1 = apply(fit_SIR_1, 2, median)
low_I_1 = apply(fit_SIR_1, 2, quantile, probs=c(0.1))
high_I_1 = apply(fit_SIR_1, 2, quantile, probs=c(0.9))
df_sample_N = data.frame(cases, sample_time)
df_fit_I_1 = data.frame(median_I_1, low_I_1, high_I_1, sample_time)
#save(df_sample_N,file="data.Rda")
#save(df_fit_I_1,file="df_I_det_Poiss.Rda")

# forecast ----------------------------------------------------------------
fit_forecast <- posts_2$y_hat_2[,,2]    # Fitted fraction of infected 
fit_forecast <- fit_forecast*pop 
med_forcast = apply(fit_forecast, 2, median)
low_forcast = apply(fit_forecast, 2, quantile, probs=c(0.1))
high_forcast = apply(fit_forecast, 2, quantile, probs=c(0.9))

forecast_out <- data.frame(med = med_forcast,
													 low = low_forcast,
													 high = high_forcast)
# plot --------------------------------------------------------------------

ggplot(df_sample_N, aes(x=sample_time, y=cases)) +
	geom_ribbon(aes(x=sample_time, ymin = low_I_1, ymax = high_I_1), fill = "orange", alpha = 0.6) +
	geom_line(data = df_fit_I_1, aes(x=sample_time, y=median_I_1, color = "Median"), size = 1.3) +
	geom_point(shape = 19, size = 3, (aes(color="Data"))) +
	scale_colour_manual(name='', values=c('Data'='black', 'Median'='darkorange3'))+
	guides(colour = guide_legend(override.aes = list(shape=c(16,NA),  linetype=c(0,1))))+
	labs(x = "Time (days)", y = "Number of Infected students") + 
	scale_x_continuous(limits=c(0, 60), breaks=c(0,7,60)) +
	#scale_y_continuous(limits=c(0,1500), breaks=c(0,100,200,300,400)) +
	theme_bw()+ theme(text = element_text(size=20))+
	geom_ribbon(data = forecast_out, 
							aes(x = 1:60, ymax = high, ymin = low), 
							inherit.aes = FALSE, color = "gray90", alpha = .5)+
	geom_line(data = forecast_out, 
							aes(x = 1:60, y = med), 
							inherit.aes = FALSE, color = "blue")+
	scale_y_log10()




plot(posts_1$theta[,1], type = "b")
