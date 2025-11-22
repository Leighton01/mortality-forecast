## Mortality Forecasting

This project compares the performance of two widely used mortality forecasting models—the log-linear (LN) model and the Lee-Carter (LC) model—using Canadian mortality data from the Human Mortality Database. The analysis evaluates each model’s ability to capture mortality trends for females and males, examining error rates and forecast accuracy to highlight their respective strengths and limitations.

File guide:

load_tab.R, female.R, male.R contain data loading and preparation
overview.R contains data visualization and exploratory plots
models.R, lee_carter_f.R contain model fitting and forecasting
.stan files contain model specifications for Bayesian estimation
