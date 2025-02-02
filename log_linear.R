library(tidyverse)
library(viridis)
library(rstan)

model_ln = "
data {
int<lower=0> N; //number of age groups
int<lower=0> F; //forecast horizon
int<lower=0> T; //number of years (periods)
real d[N, T]; //data on death counts age by time
}
parameters {
vector[N] A; //average age profile alpha
vector[T-1] k; //time effect - unnormalised
real phi; //random walk drift parameter
real<lower=0> sig[2]; //standard deviations
// constrained >0
}
transformed parameters {
}
model {
// Priors
phi ~ normal(0,2);
A ~ normal(0,5);
sig[1] ~ normal(0,1); //remember it is >0 when defined
sig[2] ~ normal(0,1); //remember it is >0 when defined
// Model for time effect
k[1] ~ normal(phi, sig[2]);
k[2:(T-1)] ~ normal(phi + k[1:(T-2)], sig[2]);
// Likelihood
// first period T=1: we don't have k[1]
for (a in 1:N){
d[a,1] ~ normal(A[a],sig[1]);
}
// now model with k[2],k[3],..., k[T]
for (a in 1:N){
for (t in 2:T){
d[a,t] ~ normal(A[a] + k[t-1],sig[1]);
}
}
}
// #Forecasting
generated quantities{
//set up forecasted variables
vector[F] kf; //temportal effect
real mdf[N, (T+F)]; //mortality rates
real log_lik[N*T]; //log-likelihood which we may need later on
//first year of forecast - we use last k[T-1]
kf[1] = normal_rng(phi + k[T-1], sig[2]);
for (t in 2:F) kf[t] = normal_rng(phi + kf[t-1], sig[2]);
 //fitted values (predictive distributions)
for (a in 1:N){
// for the first year (1921, k[1]=0)
mdf[a,1] = normal_rng(A[a], sig[1]);
// for 1921-2012
for (t in 2:T){
mdf[a,t] = normal_rng(A[a] +k[t-1], sig[1]);
}
// forecasts for 2012-2022
for (t in 1:F){
mdf[a,T+t] = normal_rng(A[a] + kf[t],sig[1]);
}
}
// generating log density - we will need it later
for (a in 1:N){
// for the first year (1921, k[1]=0)
log_lik[a] = normal_lpdf(d[a,1] | A[a], sig[1]);
// for 1921-2012
for (t in 2:T){
log_lik[N*(t-1)+a] = normal_lpdf(d[a,t] | A[a] +k[t-1], sig[1]);
}
}
}"
# this will save the code in the current working directory in a stan file:
writeLines(model_ln, con = "model_ln.stan" )
