library(tidyverse)
library(viridis)
library(rstan)

# LOAD RDAS!
#   lc_female_model.rda
#   ln_female_model.rda


#
# model_ln = "
# data {
# int<lower=0> N; //number of age groups
# int<lower=0> F; //forecast horizon
# int<lower=0> T; //number of years (periods)
# real d[N, T]; //data on death counts age by time
# }
# parameters {
# vector[N] A; //average age profile alpha
# vector[T-1] k; //time effect - unnormalised
# real phi; //random walk drift parameter
# real<lower=0> sig[2]; //standard deviations
# // constrained >0
# }
#
# transformed parameters {
# }
# model {
# // Priors
# phi ~ normal(0,2);
# A ~ normal(0,5);
# sig[1] ~ normal(0,1); //remember it is >0 when defined
# sig[2] ~ normal(0,1); //remember it is >0 when defined
# // Model for time effect
# k[1] ~ normal(phi, sig[2]);
# k[2:(T-1)] ~ normal(phi + k[1:(T-2)], sig[2]);
# // Likelihood
# // first period T=1: we don't have k[1]
# for (a in 1:N){
# d[a,1] ~ normal(A[a],sig[1]);
# }
# // now model with k[2],k[3],..., k[T]
# for (a in 1:N){
# for (t in 2:T){
# d[a,t] ~ normal(A[a] + k[t-1],sig[1]);
# }
# }
# }
# // #Forecasting
# generated quantities{
# //set up forecasted variables
# vector[F] kf; //temportal effect
# real mdf[N, (T+F)]; //mortality rates
# real log_lik[N*T]; //log-likelihood which we may need later on
# //first year of forecast - we use last k[T-1]
# kf[1] = normal_rng(phi + k[T-1], sig[2]);
# for (t in 2:F) kf[t] = normal_rng(phi + kf[t-1], sig[2]);
#  //fitted values (predictive distributions)
# for (a in 1:N){
# // for the first year (1921, k[1]=0)
# mdf[a,1] = normal_rng(A[a], sig[1]);
# // for 1921-2012
# for (t in 2:T){
# mdf[a,t] = normal_rng(A[a] +k[t-1], sig[1]);
# }
# // forecasts for 2013-2022
# for (t in 1:F){
# mdf[a,T+t] = normal_rng(A[a] + kf[t],sig[1]);
# }
# }
# // generating log density - we will need it later
# for (a in 1:N){
# // for the first year (1921, k[1]=0)
# log_lik[a] = normal_lpdf(d[a,1] | A[a], sig[1]);
# // for 1921-2012
# for (t in 2:T){
# log_lik[N*(t-1)+a] = normal_lpdf(d[a,t] | A[a] +k[t-1], sig[1]);
# }
# }
# }"
# # this will save the code in the current working directory in a stan file:
# writeLines(model_ln, con = "model_ln.stan" )
#
#
# # FEMALE DATA
temp_aux = ltf %>% filter(Year>1920 & Year<2013) %>%
  #log rates
  mutate(lmx=log(mx),
         Age=as_factor(Age))
data.inp1<-list(d= temp_aux %>%
                  pull(lmx) %>% #taking this as a vector
                  matrix(24,length(1921:2012)), #into matrix
                # N - number of age groups
                # T - number of years 1946-2009
                N=length(levels(temp_aux$Age)),
                T=length(unique(temp_aux$Year)),
                # F - forecast horizon
                F=11)
# # see the data
# # head(data.inp1$d)
# # initial values
# inits01 <- list(A=rep(-1,data.inp1$N),
#                 k=2:data.inp1$T,
#                 sig=c(1,1))
#
#
# fit.mor01 <- stan(file = "model_ln.stan", # remember to specify correct path
#                   data = data.inp1,
#                   iter = 6000, thin=1,warmup = 2000,
#                   verbose = FALSE,
#                   control = list(adapt_delta=0.9999,max_treedepth=12),
#                   init = list(inits01,inits01,inits01,inits01), # two chains
#                   chains=4, cores = 4,
#                   seed=26) #seed is optional
#
# save(fit.mor01, file = "ln_female_model.rda")

# plotting age profile alpha
plot(fit.mor01, plotfun="plot", pars=c("A")) +
  coord_flip() +
  scale_y_reverse(breaks=1:24,
                  labels=rev(levels(temp_aux$Age)))+
  theme(axis.text.x = element_text(angle=90,vjust = 0.5))

plot(fit.mor01, plotfun="plot", pars=c("k", "kf")) +
  coord_flip() +
  scale_y_reverse(breaks=1:102,
                  labels=c(2022:1921)
  ) +
  theme(axis.text.x = element_text(angle=90,vjust = 0.5))

#################################
#
# # Extracting posterior summaries
# sum1=summary(fit.mor01,pars="mdf")$summary %>%
#   as.data.frame() %>%
#   rownames_to_column("Parameter") %>%
#   # ... and giving columns meaningful names
#   separate(Parameter, c("Age","Year"), ",") %>%
#   mutate(Age=str_match(Age,"\\d+") %>%
#            as_factor() %>%
#            recode(!!!setNames(levels(temp_aux$Age),1:24)),
#          Year = 1920+as.integer(str_match(Year,"\\d+")))
# # plotting results
# fore_f_2022 <- ltf %>%
#   filter(Year==2022) %>%
#   mutate(lmx=log(mx),
#          Year=as.integer(Year),
#          Age=as_factor(Age)) %>%
#   # combining data with estimation results
#   left_join(sum1) %>%
#   ggplot() +
#   # note that below we are using percentiles of the posterior
#   geom_ribbon(aes(x=Age,ymin=`2.5%`,ymax=`97.5%`,fill="95% PI", group=1),alpha=0.25) +
#   # below, we are using median of the posterior (50%)
#   geom_line(aes(x=Age,y=`50%`, group=1, colour="forecast"), size=1.2) +
#   geom_line(aes(x=Age,y=lmx, group=1, colour="observed"), size=1.2) +
#   theme_bw() +
#   theme(axis.text.x = element_text(angle=90, vjust=0.5),
#         legend.position = "right") +
#   labs(y="log(mortality rate)", title="Forecasted and observed mortality 2022", fill="",
#        colour="")
#
#
#
# ###################################
# fore_f_age <- ltf %>%
#   # filter(Age%in%c("0","10-14","50-54")) %>%
#   mutate(lmx=log(mx),
#          Year=as.integer(Year),
#          Age=as_factor(Age)) %>%
#   filter(Year>2012) %>%
#   left_join(sum1) %>%
#   ggplot() +
#   geom_ribbon(aes(x=Year,ymin=`2.5%`,ymax=`97.5%`,fill="95% PI", group=1),alpha=0.25) +
#   geom_line(aes(x=Year,y=`50%`, group=1, colour="forecast"), size=1.2) +
#   geom_line(aes(x=Year,y=lmx, group=1, colour="observed"), size=1.2) +
#   facet_grid(.~Age,) +
#   theme_bw() +
#   theme(axis.text.x = element_text(angle=90, vjust=0.5),
#         legend.position = "right") +
#   labs(y="log(mortality rate)",
#        title="Forecasted and Observed Female Mortality for All Age Groups",
#        fill="",
#        colour="")
#
# fore_f_age
#


##########################LEE  CARTER
# model_LC = "
# data {
# int<lower=0> N; //number of age groups
# int<lower=0> F; //forecast horizon
# int<lower=0> T; //number of years (periods)
# real d[N, T]; //data on death rate age (N) by time (T)
# }
# parameters {
# vector[N] A; //average age profile alpha
# vector[N] ben; //changes of age profile - unnormalised
# vector[T-1] k; //time effect - unnormalised
# real phi; //autoregression (random walk) parameter
# real<lower=0> sig[2]; //standard deviations
# }
# transformed parameters {
# vector[N] B; //normalised changes of age profile
# //normalising beta and k
# for (i in 1:N) B[i] = ben[i]/sum(ben[1:N]);
# }
# model {
# // Priors
# phi ~ normal(0,2);
# A ~ normal(0,5);
# ben ~ normal(1.0/N,1); //important to have 1.0 as 1 would be treated as integer
# sig[1] ~ normal(0,1); //already >0
# sig[2] ~ normal(0,1); //already >0
# // Likelihood
# // Model for time effect
# //k[1] corresponds to year 1947; k for 1946 is fixed to 0
# k[1] ~ normal(phi, sig[2]);
# //k[T-1] corresponds to year 2009
# k[2:(T-1)] ~ normal(phi + k[1:(T-2)], sig[2]);
# for (a in 1:N){
# d[a,1] ~ normal(A[a],sig[1]); // for 1st year in sample
# for (t in 2:T){
# //for further years
# d[a,t] ~ normal(A[a] + B[a]*k[t-1],sig[1]);
# }
# }
# }
# //Forecasting
# generated quantities{
# // forecasted time effects
# vector[F] kf;
# real mdf[N, T+F];
# real log_lik[N*T];
# //first year of forecast - we use last k[T-1]
# kf[1] = normal_rng(phi+k[T-1], sig[2]);
# for (t in 2:F) kf[t] = normal_rng(phi + kf[t-1], sig[2]);
# for (a in 1:N){
# mdf[a,1] = normal_rng(A[a], sig[1]);
# for (t in 2:T){
# mdf[a,t] = normal_rng(A[a] + B[a]*k[t-1], sig[1]);
# }
# for (t in 1:F){
# mdf[a,T+t] = normal_rng(A[a] + B[a]*kf[t],sig[1]);
# }
# }
# // generating log density
# for (a in 1:N){
# // for the first year (1946, k[1]=0)
# log_lik[a] = normal_lpdf(d[a,1] | A[a], sig[1]);
# // for 1947-2009
# for (t in 2:T){
# log_lik[N*(t-1)+a] = normal_lpdf(d[a,t] | A[a] + B[a]*k[t-1], sig[1]);
# }
# }
# }"
# writeLines(model_LC, con = "model_LC.stan" )
#
#
#
#
#
# temp_auxT = ltf %>%
#   filter(Year>1920 & Year<2013) %>%
#   #log rates
#   mutate(lmx=log(mx),
#          Age=as_factor(Age))
#
# data.inp<-list(d= temp_auxT %>%
#                  pull(lmx) %>% #taking log-rates
#                  matrix(24,length(1921:2012)),
#                # NB: T and F are not the best names
#                # (but they are short and handy)
#                N=length(levels(temp_auxT$Age)),
#                T=length(unique(temp_auxT$Year)),
#                F=10)
#
#
# inits01 <- list(A=rep(-1,data.inp$N),
#                 k=rep(0, data.inp$T-1),
#                 sig=c(1,1))
# inits02 <- list(A=rep(1,data.inp$N),
#                 k=rep(0, data.inp$T-1),
#                 sig=c(.1,.1))
# inits03 <- list(A=rep(0,data.inp$N),
#                 k=rep(0, data.inp$T-1),
#                 sig=c(0.4,0.4))
# inits04 <- list(A=rep(0,data.inp$N),
#                 k=rep(0, data.inp$T-1),
#                 sig=c(0.7,0.7))


# fit.lc01 = stan(file = "model_LC.stan",
#                 data = data.inp,
#                 iter = 8000,
#                 warmup = 4000,
#                 thin=1,
#                 verbose = FALSE,
#                 control = list(adapt_delta=0.98,max_treedepth=14),
#                 init = list(inits01,inits01,inits01,inits01),
#                 chains=4, cores = 4,
#                 seed=26)
#
# save(fit.lc01, file = "lc_female_model.rda")

# # all plots converge except ben
# trace_sig <- plot(fit.lc01, plotfun = "trace", pars = c("sig"), inc_warmup = F)
# trace_A <- plot(fit.lc01, plotfun = "trace", pars = c("A"), inc_warmup = F)
# trace_B <- plot(fit.lc01, plotfun = "trace", pars = c("B"), inc_warmup = F)
# trace_ben <- plot(fit.lc01, plotfun = "trace", pars = c("ben"), inc_warmup = F)
# trace_mdf <- plot(fit.lc01, plotfun = "trace", pars = c("mdf"), inc_warmup = F)
# trace_k <- plot(fit.lc01, plotfun = "trace", pars = c("k"), inc_warmup = F)
# trace_kf <- plot(fit.lc01, plotfun = "trace", pars = c("kf"), inc_warmup = F)
# trace_phi <- plot(fit.lc01, plotfun = "trace", pars = c("phi"), inc_warmup = F)
#
#
# rhat_all <- plot(fit.lc01, plotfun = "rhat", pars = c("mdf","k","kf","A","B","sig","phi"))
# plot(fit.lc01, plotfun = "rhat", pars = c("k"))
# plot(fit.lc01, plotfun = "rhat", pars = c("ben"))
#
# plot(fit.lc01, plotfun = "ess", pars = c("mdf","k","kf","A","B","sig","phi"))
