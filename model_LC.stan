
data {
int<lower=0> N; //number of age groups
int<lower=0> F; //forecast horizon
int<lower=0> T; //number of years (periods)
real d[N, T]; //data on death rate age (N) by time (T)
}
parameters {
vector[N] A; //average age profile alpha
vector[N] ben; //changes of age profile - unnormalised
vector[T-1] k; //time effect - unnormalised
real phi; //autoregression (random walk) parameter
real<lower=0> sig[2]; //standard deviations
}
transformed parameters {
vector[N] B; //normalised changes of age profile
//normalising beta and k
for (i in 1:N) B[i] = ben[i]/sum(ben[1:N]);
}
model {
// Priors
phi ~ normal(0,2);
A ~ normal(0,5);
ben ~ normal(1.0/N,1); //important to have 1.0 as 1 would be treated as integer
sig[1] ~ normal(0,1); //already >0
sig[2] ~ normal(0,1); //already >0
// Likelihood
// Model for time effect
//k[1] corresponds to year 1947; k for 1946 is fixed to 0
k[1] ~ normal(phi, sig[2]);
//k[T-1] corresponds to year 2009
k[2:(T-1)] ~ normal(phi + k[1:(T-2)], sig[2]);
for (a in 1:N){
d[a,1] ~ normal(A[a],sig[1]); // for 1st year in sample
for (t in 2:T){
//for further years
d[a,t] ~ normal(A[a] + B[a]*k[t-1],sig[1]);
}
}
}
//Forecasting
generated quantities{
// forecasted time effects
vector[F] kf;
real mdf[N, T+F];
real log_lik[N*T];
//first year of forecast - we use last k[T-1]
kf[1] = normal_rng(phi+k[T-1], sig[2]);
for (t in 2:F) kf[t] = normal_rng(phi + kf[t-1], sig[2]);
for (a in 1:N){
mdf[a,1] = normal_rng(A[a], sig[1]);
for (t in 2:T){
mdf[a,t] = normal_rng(A[a] + B[a]*k[t-1], sig[1]);
}
for (t in 1:F){
mdf[a,T+t] = normal_rng(A[a] + B[a]*kf[t],sig[1]);
}
}
// generating log density
for (a in 1:N){
// for the first year (1946, k[1]=0)
log_lik[a] = normal_lpdf(d[a,1] | A[a], sig[1]);
// for 1947-2009
for (t in 2:T){
log_lik[N*(t-1)+a] = normal_lpdf(d[a,t] | A[a] + B[a]*k[t-1], sig[1]);
}
}
}
