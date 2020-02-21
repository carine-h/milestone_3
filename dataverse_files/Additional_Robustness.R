library(foreign)
library("survey")
library(car)
library("ggplot2")
library("descr")
library("weights")
setwd("C:/Users/tobia/Dropbox/Partisan Homogeneity Project/ANES/")
data2 <- read.dta("anes_timeseries_2012_Stata12.dta")
# pid
data2$pid <- NA
data2$pid[as.numeric(data2$pid_x) == 1] <- 1
data2$pid[as.numeric(data2$pid_x) == 2] <- 2
data2$pid[as.numeric(data2$pid_x) == 3] <- 3
data2$pid[as.numeric(data2$pid_x) == 4] <- 4
data2$pid[as.numeric(data2$pid_x) == 5] <- 5
data2$pid[as.numeric(data2$pid_x) == 6] <- 6
data2$pid[as.numeric(data2$pid_x) == 7] <- 7

data2$pid_folded <- NA
data2$pid_folded[data2$pid == 7 | data2$pid == 1] <- 4
data2$pid_folded[data2$pid == 6 | data2$pid == 2] <- 3
data2$pid_folded[data2$pid == 5 | data2$pid == 3] <- 2
data2$pid_folded[data2$pid == 4] <- 1



svymean(~campaign_traditional_contact, design = data3.hh.design_ftof, na.rm = T)
wtd.table(data2$pid, data2$weight_full, na.rm = T)


# 2012 vote and imputation share
load("../Deliverables_20150921/Robustness.RData")
vote_2012 <- read.dbf("../Deliverables_20150921/2012_vote_county.dbf", as.is = T)
vote_2012 <- data.table(vote_2012)
setkey(vote_2012, FIPS)
setkey(corrs, county_fips)
corrs <- vote_2012[corrs]
corrs$imputed <- corrs$imputed + 2
corrs$non_imputed <- corrs$non_imputed + 2

summary(lm(corrs$PCT_OBM ~ corrs$imputed))
summary(lm(corrs$PCT_OBM ~ corrs$non_imputed))
party_registration_states <- c("AK", "AZ", "CA", "CO", "CT", "DC", "DE", "FL", "IA", "ID", "KS", "KY", "LA", "MA", "MD", "ME", "NC", "NE", "NH", 
                               "NJ", "NM", "NV", "NY", "OK", "OR", "PA", "RI", "SD", "UT", "WV", "WY")
t.test(vote_2012$PCT_OBM[vote_2012$STATE %in% party_registration_states], vote_2012$PCT_OBM[!vote_2012$STATE %in% party_registration_states])