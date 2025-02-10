# library and rda-----------------------------------------------------------

library(tidyverse)
library(viridis)
library(rstan)

## Load model RDAs
load(file = "ln_female_model.rda") #fit.mor01
load(file = "ln_male_model.rda")  #fit.mor01m
load(file = "lc_female_model.rda") #fit.lc01
load(file = "lc_male_model.rda") #fit.lc01_m

# Load model posteriors/summaries
## only include mdf after 1920
load(file = "sum_ln_mdf_f.rda") # sum1
load(file = "sum_ln_mdf_m.rda") # sum1_m
load(file = "sum_lc_f.rda") # sum.lc01
load(file = "sum_lc_m.rda") # sum.lc01_m

# Load model with life table
load(file="lt_lc_f.rda")
load(file="lt_ln_f.rda")
load(file="lt_lc_m.rda")
load(file="lt_ln_m.rda")

# rhat
load(file="lc_rhat_f_values.rda")
load(file="lc_rhat_m_values_rda")

# LN Convergence ----------------------------------------------------------

# Female
# Male

# LC Convergence ----------------------------------------------------------
# lc_rhat_f <- summary(fit.lc01)$summary[, "Rhat"]
# save(lc_rhat_f, file="lc_rhat_f_values.rda")
# Find parameters with Rhat values larger than 1.1
lc_rhat_f_bad <- lc_rhat_f[lc_rhat_f >= 1.1]

# lc_rhat_m <- summary(fit.lc01_m)$summary[, "Rhat"]
# save(lc_rhat_m, file="lc_rhat_m_values_rda")
# Find parameters with Rhat values larger than 1.1
lc_rhat_m_bad <- lc_rhat_m[lc_rhat_m >= 1.1]

# lc_ESS_f <- summary(fit.lc01)$summary[, "n_eff"]
# # Find parameters with Rhat values larger than 1.1
# lc_ESS_f_bad <- rhat_values[lc_ESS_f < 1000]



#
# # Female
# lc_trace_ben_f <- plot(fit.lc01, plotfun = "trace", pars = c("ben"), inc_warmup = F)
# lc_trace_B_f <- plot(fit.lc01, plotfun = "trace", pars = c("B"), inc_warmup = F)
# lc_trace_mdf_f <- plot(fit.lc01, plotfun = "trace", pars = c("mdf"), inc_warmup = F)
# lc_trace_mdf_f <- plot(fit.lc01, plotfun = "trace", pars = c("mdf[1,24]","mdf[3,21]",
#                                                                "mdf[6,18]","mdf[9,15]",
#                                                                "mdf[12,12]","mdf[15,9]",
#                                                                "mdf[18,6]","mdf[21,3]",
#                                                                "mdf[24,1]"), inc_warmup = F)

# lc_dens_kf_f <- stan_dens(fit.lc01, pars = c("kf"))
# lc_dens_mdf_f <- stan_dens(fit.lc01, pars = c("mdf"))

# lc_dens_mdf_f <- stan_dens(fit.lc01, pars = c("mdf[1,24]","mdf[3,21]",
# "mdf[6,18]","mdf[9,15]",
# "mdf[12,12]","mdf[15,9]",
# "mdf[18,6]","mdf[21,3]",
# "mdf[24,1]"
# ))
#
# lc_rhat_all_f <- plot(fit.lc01, plotfun = "rhat", pars = c("mdf","k","kf","A","B","sig","phi"))
# lc_rhat_mdf_f <- plot(fit.lc01, plotfun = "rhat", pars = c("mdf"))
# lc_rhat_kf_f <- plot(fit.lc01, plotfun = "rhat", pars = c("kf"))
# lc_rhat_ben_f <- plot(fit.lc01, plotfun = "rhat", pars = c("ben"))
#
# lc_ess_f <- plot(fit.lc01, plotfun = "ess",
#                  pars = c("mdf","k","kf","A","B","sig","phi"))
#
#
#
# # male
# lc_trace_ben_m <- plot(fit.lc01_m, plotfun = "trace", pars = c("ben"), inc_warmup = F)
# lc_trace_B_m <- plot(fit.lc01_m, plotfun = "trace", pars = c("B"), inc_warmup = F)
# lc_trace_mdf_m <- plot(fit.lc01_m, plotfun = "trace", pars = c("mdf"), inc_warmup = F)
# lc_trace_mdf_m <- plot(fit.lc01_m, plotfun = "trace", pars = c("mdf[1,24]","mdf[3,21]",
#                                                                "mdf[6,18]","mdf[9,15]",
#                                                                "mdf[12,12]","mdf[15,9]",
#                                                                "mdf[18,6]","mdf[21,3]",
#                                                                "mdf[24,1]"), inc_warmup = F)
#
# lc_dens_kf_m <- stan_dens(fit.lc01_m, pars = c("kf"))
# # lc_dens_mdf_m <- stan_dens(fit.lc01_m, pars = c("mdf"))
# lc_dens_mdf_m <- stan_dens(fit.lc01_m, pars = c("mdf[1,24]","mdf[3,21]",
                                                # "mdf[6,18]","mdf[9,15]",
                                                # "mdf[12,12]","mdf[15,9]",
                                                # "mdf[18,6]","mdf[21,3]",
                                                # "mdf[24,1]"
                                                # ))
#
# lc_rhat_all_m <- plot(fit.lc01_m, plotfun = "rhat", pars = c("mdf","k","kf","A","B","sig","phi"))
# lc_rhat_mdf_m <- plot(fit.lc01_m, plotfun = "rhat", pars = c("mdf"))
# lc_rhat_ben_m <- plot(fit.lc01_m, plotfun = "rhat", pars = c("ben"))
#
# lc_ess_m <- plot(fit.lc01_m, plotfun = "ess",
#                  pars = c("mdf","k","kf","A","B","sig","phi"))
#
#




# All Age Mortality Forecast -------------------------------------------

mort_f <- ltf %>%
  # filter(Age%in%c("0","10-14","50-54")) %>%
  mutate(lmx=log(mx),
         Year=as.integer(Year),
         Age=as_factor(Age)) %>%
  filter(Year>2012) %>%
  left_join(sum1) %>%
  left_join(sum.lc01,by = join_by(Year, Age))


gg_mort_f <- mort_f %>%
  ggplot() +
  # geom_ribbon(aes(x=Year,ymin=`2.5%`,ymax=`97.5%`,fill="95% PI", group=1),alpha=0.25) +
  geom_line(aes(x=Year,y=`50%.x`, group=1, colour="LN Forecast"), size=1.2) +
  geom_line(aes(x=Year,y=`50%.y`, group=1, colour="LC Forecast"), size=1.2) +
  geom_line(aes(x=Year,y=lmx, group=1, colour="Observed"), size=1.2) +
  facet_grid(.~Age,) +
  theme_bw() +
  theme(axis.text.x = element_text(angle=90, vjust=0.5),
        legend.position = "right") +
  labs(y="log10(Mortality Rate)",
       title="Forecasted and Observed Female Mortality for All Age Groups",
       fill="",
       colour="")

mort_f
gg_mort_f


mort_m <- ltm %>%
  # filter(Age%in%c("0","10-14","50-54")) %>%
  mutate(lmx=log(mx),
         Year=as.integer(Year),
         Age=as_factor(Age)) %>%
  filter(Year>2012) %>%
  left_join(sum1_m) %>%
  left_join(sum.lc01_m,by = join_by(Year, Age))

gg_mort_m <- mort_m %>%
  ggplot() +
  # geom_ribbon(aes(x=Year,ymin=`2.5%`,ymax=`97.5%`,fill="95% PI", group=1),alpha=0.25) +
  geom_line(aes(x=Year,y=`50%.x`, group=1, colour="LN Forecast"), size=1.2) +
  geom_line(aes(x=Year,y=`50%.y`, group=1, colour="LC Forecast"), size=1.2) +
  geom_line(aes(x=Year,y=lmx, group=1, colour="Observed"), size=1.2) +
  facet_grid(.~Age,) +
  theme_bw() +
  theme(axis.text.x = element_text(angle=90, vjust=0.5),
        legend.position = "right") +
  labs(y="log10(Mortality Rate)",
       title="Forecasted and Observed Male Mortality for All Age Groups",
       fill="",
       colour="")

mort_m
gg_mort_m



# Life Expectancy (Median) ---------------------------------------------
# Female
reported_data = filter(ltf, Year>1920) %>%
  group_by(Year) %>%
  mutate(ax = case_when(
    Age=="0" ~ 0.053 + 2.8*mx,
    Age=="1–4" ~ 1.522-1.518*mx,
    Age=="110+" ~ 1/mx,
    TRUE ~ 2.5),
    n = case_when(
      Age=="0" ~ 1,
      Age=="1–4" ~ 4,
      Age=="110+" ~ 30,
      TRUE ~ 5),
    lx = lag(cumprod((1-ax*mx)/(1+(n-ax)*mx)), default=1),
    Lx = n * lead(lx, default = 0) + (ax*(lx - lead(lx, default = 0))),
    Lx = ifelse(Age=="110+",lx/mx,Lx),
    ex1 = ifelse(Age=="0",rev(cumsum(rev(Lx))) / lx,0)
  ) %>%
  ungroup()
reported_data$Age <- as_factor(reported_data$Age)

# temp_aux = ltf %>% filter(Year>1920 & Year<2013) %>%
#   #log rates
#   mutate(lmx=log(mx),
#          Age=as_factor(Age))


# lt_lc = fit.lc01 %>%
#   as.data.frame() %>%
#   select(matches("mdf")) %>%
#   pivot_longer(cols=everything(),
#                names_to="Parameter",
#                values_to="Value") %>%
#   group_by(Parameter) %>%
#   mutate(Iteration=row_number(Parameter)) %>%
#   ungroup() %>%
#   #some renaming going on here with stringr function str_match
#   separate(col = Parameter,
#            into = c("Age","Year"),
#            sep=",") %>%
#   mutate(Age=str_match(Age,"\\d+") %>% as_factor() %>% recode(!!!setNames(levels(temp_aux
#                                                                                  $Age),1:24)),
#          Year = 1920+as.integer(str_match(Year,"\\d+")),
#          mx=exp(Value)) %>%
#   filter(Iteration%in%seq(2,1000,2)) %>%
#   # and calculating the life table
#   group_by(Year,Iteration) %>%
#   mutate(ax = case_when(
#     Age=="0" ~ 0.053 + 2.8*mx,
#     Age=="1–4" ~ 1.522-1.518*mx,
#     Age=="110+" ~ 1/mx,
#     TRUE ~ 2.5),
#     n = case_when(
#       Age=="0" ~ 1,
#       Age=="1–4" ~ 4,
#       Age=="110+" ~ 30,
#       TRUE ~ 5),
#     lx = lag(cumprod((1-ax*mx)/(1+(n-ax)*mx)), default=1),
#     Lx = n * lead(lx, default = 0) + (ax*(lx - lead(lx, default = 0))),
#     Lx = ifelse(Age=="110+",lx/mx,Lx),
#     ex2 = ifelse(Age=="0",rev(cumsum(rev(Lx))) / lx,0)
#   ) %>%
#   ungroup() %>%
#   group_by(Age,Year) %>%
#   summarise(q10=quantile(ex2,0.1),q50=quantile(ex2,.5),q90=quantile(ex2,.9)) %>%
#   ungroup()



# lt_ln = fit.mor01 %>%
#   as.data.frame() %>%
#   select(matches("mdf")) %>%
#   pivot_longer(cols=everything(),
#                names_to="Parameter",
#                values_to="Value") %>%
#   group_by(Parameter) %>%
#   mutate(Iteration=row_number(Parameter)) %>%
#   ungroup() %>%
#   #some renaming going on here with stringr function str_match
#   separate(col = Parameter,
#            into = c("Age","Year"),
#            sep=",") %>%
#   mutate(Age=str_match(Age,"\\d+") %>% as_factor() %>% recode(!!!setNames(levels(temp_aux
#                                                                                  $Age),1:24)),
#          Year = 1920+as.integer(str_match(Year,"\\d+")),
#          mx=exp(Value)) %>%
#   filter(Iteration%in%seq(2,1000,2)) %>%
#   # and calculating the life table
#   group_by(Year,Iteration) %>%
#   mutate(ax = case_when(
#     Age=="0" ~ 0.053 + 2.8*mx,
#     Age=="1–4" ~ 1.522-1.518*mx,
#     Age=="110+" ~ 1/mx,
#     TRUE ~ 2.5),
#     n = case_when(
#       Age=="0" ~ 1,
#       Age=="1–4" ~ 4,
#       Age=="110+" ~ 30,
#       TRUE ~ 5),
#     lx = lag(cumprod((1-ax*mx)/(1+(n-ax)*mx)), default=1),
#     Lx = n * lead(lx, default = 0) + (ax*(lx - lead(lx, default = 0))),
#     Lx = ifelse(Age=="110+",lx/mx,Lx),
#     ex2 = ifelse(Age=="0",rev(cumsum(rev(Lx))) / lx,0)
#   ) %>%
#   ungroup() %>%
#   group_by(Age,Year) %>%
#   summarise(q10=quantile(ex2,0.1),q50=quantile(ex2,.5),q90=quantile(ex2,.9)) %>%
#   ungroup()



# x is lc, y is ln values
data = left_join(lt_lc, reported_data)
data <- left_join(lt_ln, data, join_by(Age, Year))

gg_le_f <- ggplot(filter(data, Age=="0")) +
  geom_line(aes(x=Year,y=q50.x, colour="LC Median"), size=0.8) +
  geom_line(aes(x=Year,y=q50.y, colour="LN Median"), size=0.8) +
  geom_line(aes(x=Year,y=ex1, colour="Reported"), size=0.8) +
  labs(color = "", fill = "")+
  geom_ribbon(aes(x=Year,ymin=q10.x,ymax=q90.x, fill="LC 80% CI"),alpha=0.3) +
  geom_ribbon(aes(x=Year,ymin=q10.y,ymax=q90.y, fill="LN 80% CI"),alpha=0.3) +
  geom_vline(xintercept = 2013, linetype="dashed", colour="gray18") +
  annotate("text", x = 2008, y = 70, label = "2013", angle = 90)+
  scale_x_continuous(expand = c(0,0)) +
  ylim(50, 90)+
  xlim(1921, 2022)+
  scale_color_manual("", values = c("tomato","dodgerblue3","gray18")) +
  scale_fill_manual("",values=c("tomato", "dodgerblue3")) +
  labs(y="Age", title = "Female Life Expectancy, Canada")


# Male

temp_aux = ltm %>% filter(Year>1920 & Year<2013) %>%
  #log rates
  mutate(lmx=log(mx),
         Age=as_factor(Age))

reported_data_m = filter(ltm, Year>1920) %>%
  group_by(Year) %>%
  mutate(ax = case_when(
    Age=="0" ~ 0.053 + 2.8*mx,
    Age=="1–4" ~ 1.522-1.518*mx,
    Age=="110+" ~ 1/mx,
    TRUE ~ 2.5),
    n = case_when(
      Age=="0" ~ 1,
      Age=="1–4" ~ 4,
      Age=="110+" ~ 30,
      TRUE ~ 5),
    lx = lag(cumprod((1-ax*mx)/(1+(n-ax)*mx)), default=1),
    Lx = n * lead(lx, default = 0) + (ax*(lx - lead(lx, default = 0))),
    Lx = ifelse(Age=="110+",lx/mx,Lx),
    ex1 = ifelse(Age=="0",rev(cumsum(rev(Lx))) / lx,0)
  ) %>%
  ungroup()

reported_data_m$Age <- as_factor(reported_data_m$Age)

# lt_lc_m = fit.lc01_m %>%
#   as.data.frame() %>%
#   select(matches("mdf")) %>%
#   pivot_longer(cols=everything(),
#                names_to="Parameter",
#                values_to="Value") %>%
#   group_by(Parameter) %>%
#   mutate(Iteration=row_number(Parameter)) %>%
#   ungroup() %>%
#   #some renaming going on here with stringr function str_match
#   separate(col = Parameter,
#            into = c("Age","Year"),
#            sep=",") %>%
#   mutate(Age=str_match(Age,"\\d+") %>% as_factor() %>% recode(!!!setNames(levels(temp_aux
#                                                                                  $Age),1:24)),
#          Year = 1920+as.integer(str_match(Year,"\\d+")),
#          mx=exp(Value)) %>%
#   filter(Iteration%in%seq(2,1000,2)) %>%
#   # and calculating the life table
#   group_by(Year,Iteration) %>%
#   mutate(ax = case_when(
#     Age=="0" ~ 0.053 + 2.8*mx,
#     Age=="1–4" ~ 1.522-1.518*mx,
#     Age=="110+" ~ 1/mx,
#     TRUE ~ 2.5),
#     n = case_when(
#       Age=="0" ~ 1,
#       Age=="1–4" ~ 4,
#       Age=="110+" ~ 30,
#       TRUE ~ 5),
#     lx = lag(cumprod((1-ax*mx)/(1+(n-ax)*mx)), default=1),
#     Lx = n * lead(lx, default = 0) + (ax*(lx - lead(lx, default = 0))),
#     Lx = ifelse(Age=="110+",lx/mx,Lx),
#     ex2 = ifelse(Age=="0",rev(cumsum(rev(Lx))) / lx,0)
#   ) %>%
#   ungroup() %>%
#   group_by(Age,Year) %>%
#   summarise(q10=quantile(ex2,0.1),q50=quantile(ex2,.5),q90=quantile(ex2,.9)) %>%
#   ungroup()

#
# lt_ln_m = fit.mor01m %>%
#   as.data.frame() %>%
#   select(matches("mdf")) %>%
#   pivot_longer(cols=everything(),
#                names_to="Parameter",
#                values_to="Value") %>%
#   group_by(Parameter) %>%
#   mutate(Iteration=row_number(Parameter)) %>%
#   ungroup() %>%
#   #some renaming going on here with stringr function str_match
#   separate(col = Parameter,
#            into = c("Age","Year"),
#            sep=",") %>%
#   mutate(Age=str_match(Age,"\\d+") %>% as_factor() %>% recode(!!!setNames(levels(temp_aux
#                                                                                  $Age),1:24)),
#          Year = 1920+as.integer(str_match(Year,"\\d+")),
#          mx=exp(Value)) %>%
#   filter(Iteration%in%seq(2,1000,2)) %>%
#   # and calculating the life table
#   group_by(Year,Iteration) %>%
#   mutate(ax = case_when(
#     Age=="0" ~ 0.053 + 2.8*mx,
#     Age=="1–4" ~ 1.522-1.518*mx,
#     Age=="110+" ~ 1/mx,
#     TRUE ~ 2.5),
#     n = case_when(
#       Age=="0" ~ 1,
#       Age=="1–4" ~ 4,
#       Age=="110+" ~ 30,
#       TRUE ~ 5),
#     lx = lag(cumprod((1-ax*mx)/(1+(n-ax)*mx)), default=1),
#     Lx = n * lead(lx, default = 0) + (ax*(lx - lead(lx, default = 0))),
#     Lx = ifelse(Age=="110+",lx/mx,Lx),
#     ex2 = ifelse(Age=="0",rev(cumsum(rev(Lx))) / lx,0)
#   ) %>%
#   ungroup() %>%
#   group_by(Age,Year) %>%
#   summarise(q10=quantile(ex2,0.1),q50=quantile(ex2,.5),q90=quantile(ex2,.9)) %>%
#   ungroup()






data_m = left_join(lt_lc_m, reported_data_m)
data_m <- left_join(lt_ln_m, data, join_by(Age, Year))

gg_le_m <- ggplot(filter(data_m, Age=="0")) +
  geom_line(aes(x=Year,y=q50.x, colour="LC Median"), size=0.8) +
  geom_line(aes(x=Year,y=q50.y, colour="LN Median"), size=0.8) +
  geom_line(aes(x=Year,y=ex1, colour="Reported"), size=0.8) +
  labs(color = "", fill = "")+
  geom_ribbon(aes(x=Year,ymin=q10.x,ymax=q90.x, fill="LC 80% CI"),alpha=0.3) +
  geom_ribbon(aes(x=Year,ymin=q10.y,ymax=q90.y, fill="LN 80% CI"),alpha=0.3) +
  geom_vline(xintercept = 2013, linetype="dashed", colour="gray18") +
  annotate("text", x = 2008, y = 70, label = "2013", angle = 90)+
  scale_x_continuous(expand = c(0,0)) +
  ylim(50, 90)+
  xlim(1921, 2022)+
  scale_color_manual("", values = c("tomato","dodgerblue3","gray18")) +
  scale_fill_manual("",values=c("tomato", "dodgerblue3")) +
  labs(y="Age", title = "Male Life Expectancy, Canada")

# Life Expectancy Error -------------------------------------------------------------------

# Mean Error (ME)
# valid_f <- data[!is.na(data$ex1) & !is.na(data$q50.y) & !is.na(data$q50.x) &
#                      data$ex1 != 0 & data$q50.y != 0 & data$q50.x != 0 &
#                   data$Year > 2012, ]

valid_f <- mort_f[!is.na(mort_f$lmx) & !is.na(mort_f$mean.y) & !is.na(mort_f$mean.x) &
                    mort_f$lmx != 0 & mort_f$mean.y != 0 & mort_f$mean.x != 0 &
                    mort_f$Year > 2012, ]

ln_me_f <- mean(valid_f$lmx - valid_f$mean.x)
lc_me_f <- mean(valid_f$lmx - valid_f$mean.y)


valid_m <- mort_m[!is.na(mort_m$lmx) & !is.na(mort_m$mean.y) & !is.na(mort_m$mean.x) &
                    mort_m$lmx != 0 & mort_m$mean.y != 0 & mort_m$mean.x != 0 &
                    mort_m$Year > 2012, ]

ln_me_m <- mean(valid_m$lmx - valid_m$mean.x)
lc_me_m <- mean(valid_m$lmx - valid_m$mean.y)



# Mean Percentage Error (MPE)

ln_mpe_f <- mean(((valid_f$lmx - valid_f$mean.x) / valid_f$lmx) * 100)
lc_mpe_f <- mean(((valid_f$lmx - valid_f$mean.y) / valid_f$lmx) * 100)

ln_mpe_m <- mean(((valid_m$lmx - valid_m$mean.x) / valid_m$lmx) * 100)
lc_mpe_m <- mean(((valid_m$lmx - valid_m$mean.y) / valid_m$lmx) * 100)


# Mean Absolute Error (MAE)

ln_mae_f <- mean(abs(valid_f$lmx - valid_f$mean.x))
lc_mae_f <- mean(abs(valid_f$lmx - valid_f$mean.y))

ln_mae_m <- mean(abs(valid_m$lmx - valid_m$mean.x))
lc_mae_m <- mean(abs(valid_m$lmx - valid_m$mean.y))

# Root Mean Square Error (RMSE)

ln_rmse_f <- sqrt(mean((valid_f$lmx - valid_f$mean.x)^2))
lc_rmse_f <- sqrt(mean((valid_f$lmx - valid_f$mean.y)^2))

ln_rmse_m <- sqrt(mean((valid_m$lmx - valid_m$mean.x)^2))
lc_rmse_m <- sqrt(mean((valid_m$lmx - valid_m$mean.y)^2))

# Mean Absolute Percentage Error(MAPE)

ln_mape_f <- mean(abs((valid_f$lmx - valid_f$mean.x) / valid_f$lmx)) * 100
lc_mape_f <- mean(abs((valid_f$lmx - valid_f$mean.y) / valid_f$lmx)) * 100

ln_mape_m <- mean(abs((valid_m$lmx - valid_m$mean.x) / valid_m$lmx)) * 100
lc_mape_m <- mean(abs((valid_m$lmx - valid_m$mean.y) / valid_m$lmx)) * 100


# Make tables of errors
# FIX AFTER MALE IS DONE!
tab_me <- tibble("Model" = c("LN", "LC"),
                 "Female" = c(ln_me_f, lc_me_f),
                 "Male" = c(ln_me_m,lc_me_m))


tab_mpe <- tibble("Model" = c("LN", "LC"),
                  "Female" = c(ln_mpe_f, lc_mpe_f),
                  "Male" = c(ln_mpe_m,lc_mpe_m))

tab_mae <- tibble("Model" = c("LN", "LC"),
                  "Female" = c(ln_mae_f, lc_mae_f),
                  "Male" = c(ln_mae_m,lc_mae_m))

tab_rmse <- tibble("Model" = c("LN", "LC"),
                   "Female" = c(ln_rmse_f, lc_rmse_f),
                   "Male" = c(ln_rmse_m,lc_rmse_m))

tab_mape <- tibble("Model" = c("LN", "LC"),
                   "Female" = c(ln_mape_f, lc_mape_f),
                   "Male" = c(ln_mape_m,lc_mape_m))


# using the 95% credible interval (columns 2.5% and 97.5% ), calculate the proportion of observed
# mortality rates in 2013-2022 that fall within the predictive intervals of the forecasts. Compare these
# measures between log-linear and Lee-Carter models.

# female
prop_within_ln_f <- mean(between(mort_f$`lmx`, mort_f$`2.5%.x`, mort_f$`97.5%.x`))
prop_within_lc_f <- mean(between(mort_f$`lmx`, mort_f$`2.5%.y`, mort_f$`97.5%.y`))

# male
prop_within_ln_m <- mean(between(mort_m$`lmx`, mort_m$`2.5%.x`, mort_m$`97.5%.x`))
prop_within_lc_m <- mean(between(mort_m$`lmx`, mort_m$`2.5%.y`, mort_m$`97.5%.y`))

tab_prop <- tibble("Model" = c("LN", "LC"),
                   "Female" = c(prop_within_ln_f, prop_within_lc_f),
                   "Male" = c(prop_within_ln_m,prop_within_lc_m))
