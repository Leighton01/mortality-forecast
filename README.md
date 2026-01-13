## Overview 

This project compares the performance of two widely used mortality forecasting models—the log-linear (LN) model and the Lee-Carter (LC) model—using Canadian mortality data from the Human Mortality Database. The analysis evaluates each model’s ability to capture mortality trends for females and males, examining error rates and forecast accuracy to highlight their respective strengths and limitations.

# File guide:

[Report](https://github.com/Leighton01/mortality-forecast/blob/main/11484265_Essay.pdf) - final report containing analysis and visualizations.

[overview.R](https://github.com/Leighton01/mortality-forecast/blob/main/overview.R) - code for data visualization and exploratory plots
[models.R](https://github.com/Leighton01/mortality-forecast/blob/main/models.R), [lee_carter_f.R](https://github.com/Leighton01/mortality-forecast/blob/main/lee_carter_f.R) - code for model fitting and forecasting
[load_tab.R](https://github.com/Leighton01/mortality-forecast/blob/main/load_tab.R), [female.R](https://github.com/Leighton01/mortality-forecast/blob/main/female.R), [male.R](https://github.com/Leighton01/mortality-forecast/blob/main/male.R) - code for data loading and preparation
.stan files contain model specifications for Bayesian estimation
