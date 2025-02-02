library(tidyverse)
library(HMDHFDplus)
library(viridis)

options(scipen = 999)

#female lifetable per 5x1, every year in 5y age groups
ltf <- readHMDweb(CNTRY = "CAN",
                      item = "fltper_5x1",
                      username = "leighton.d@live.com",
                      password = "mFAT^L9^es34",
                      fixup = FALSE)


#male lifetable per 5x1
ltm <- readHMDweb(CNTRY = "CAN",
                      item = "mltper_5x1",
                      username = "leighton.d@live.com",
                      password = "mFAT^L9^es34",
                      fixup = FALSE)


ltf %>%
  mutate(lmx1=log10(mx),
         Age=as_factor(Age)) %>%
  ggplot() +
  geom_line(aes(x=Age,y=lmx1, colour=Year,group=Year), size=1.05) +
  scale_color_viridis(option = "B", end=0.9) + #this is only for pretty colours
  theme_bw() +
  theme(axis.text.x = element_text(angle=90, vjust=0.5),
        legend.position = "right") +
  labs(y="log10(mortality rate)",
       title="Average (over 1921-2022) female mortality")


ltf %>%
  mutate(lmx1=log10(mx),
         Age=as_factor(Age)) %>%
  group_by(Age) %>%
  summarise(lmx_age=mean(lmx1)) %>%
  ggplot() +
  geom_line(aes(x=Age,y=lmx_age, group=1), size=1.2) +
  theme_bw() +
  theme(axis.text.x = element_text(angle=90, vjust=0.5),
        legend.position = "right") +
  labs(y="log10(mortality rate)",
       title="Average (over 1921-2022) female mortality")


ltf %>%
  mutate(lmx1=log10(mx),
         Age=as_factor(Age)) %>%
  group_by(Year) %>%
  summarise(lmx_time=mean(lmx1)) %>%
  ggplot() +
  geom_line(aes(x=Year,y=lmx_time, group=1), size=1.2) +
  theme_bw() +
  theme(axis.text.x = element_text(angle=90, vjust=0.5),
        legend.position = "right") +
  labs(y="log10(mortality rate)",
       title="Average (over all age groups) female mortality")













ltm %>%
  mutate(lmx1=log10(mx),
         Age=as_factor(Age)) %>%
  ggplot() +
  geom_line(aes(x=Age,y=lmx1, colour=Year,group=Year), size=1.05) +
  scale_color_viridis(option = "B", end=0.9) + #this is only for pretty colours
  theme_bw() +
  theme(axis.text.x = element_text(angle=90, vjust=0.5),
        legend.position = "right") +
  labs(y="log10(mortality rate)",
       title="Average (over 1921-2022) male mortality")


ltm %>%
  mutate(lmx1=log10(mx),
         Age=as_factor(Age)) %>%
  group_by(Age) %>%
  summarise(lmx_age=mean(lmx1)) %>%
  ggplot() +
  geom_line(aes(x=Age,y=lmx_age, group=1), size=1.2) +
  theme_bw() +
  theme(axis.text.x = element_text(angle=90, vjust=0.5),
        legend.position = "right") +
  labs(y="log10(mortality rate)",
       title="Average (over 1921-2022) male mortality")


ltm %>%
  mutate(lmx1=log10(mx),
         Age=as_factor(Age)) %>%
  group_by(Year) %>%
  summarise(lmx_time=mean(lmx1)) %>%
  ggplot() +
  geom_line(aes(x=Year,y=lmx_time, group=1), size=1.2) +
  theme_bw() +
  theme(axis.text.x = element_text(angle=90, vjust=0.5),
        legend.position = "right") +
  labs(y="log10(mortality rate)",
       title="Average (over all age groups) male mortality")

