# data current through 4/26/2020

library(car)

df <- read.csv('covid_county.csv')
head(df)

# convert to date format
df$stay_home_date <- as.Date(df$stay_home_date)
df$first_case <- as.Date(df$first_case)

# create exposure days variable, the number of days after the first case in which no lockdown was in place
# if there is no order, todays date minus first case. If there is, order date minus first case
df$exp_days <- ifelse(is.na(df$stay_home_date), as.Date('2020-04-26') - df$first_case, df$stay_home_date - df$first_case)
df$exp_days[df$exp_days < 0] <- 0

# create deaths per capita
df$dpc <- df$deaths/df$pop2018

# % elderly
df$eld_perc <- df$X65plus2018/df$pop2018

############
# fit models
############
# model deaths
d1 <- lm(deaths ~ X65plus2018 + dens2019 + poverty + obesity + exp_days + r_percent, data = df)
summary(d1)

d2 <- lm(dpc ~ eld_perc + dens2019 + obesity + exp_days + r_percent, data = df)
summary(d2)

vif(d1)
vif(d2)
# model exposure days. Need state data for this? 
exp1 <- lm(exp_days ~ eld_perc + r_percent, data = df)
summary(exp1)
vif(exp1)


# Diagnose colinearity
# create new variables
# get vif
# 

# correlations
cor(df[,c(5, 11, 14, 15, 16, 17, 18, 19)], use = 'complete.obs')

# regress r_perc on other variables
summary(lm(r_percent ~ dens2019 + poverty + obesity + eld_perc, data = df))
