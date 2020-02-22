# Carine comment: these are a bunch of mathematical caluclations for different means, etc. to use in the paper as basic summary statistics
# it is NOT the main code for the paper



############# read in data#############################################
#setwd("C:\\Users\\tobia\\Dropbox\\Partisan Homogeneity Project\\Jennings Data\\Parent 1965-1982/")


# set up and libraries#######################
library(dplyr)
library(plyr)
library(data.table)
library(foreign)
library(plm)
library(lmtest)
library(sandwich)
library(memisc)
library(car)
library(mirt)
library(descr)
library(polycor)
library(xtable)
library(stargazer)
library(haven)
# read in data##############################




#couple_1973 <- read.dta("X2waveparentspouse.dta")
#offspring_1973 <- read.dta("X2waveyouthspouse.dta")
#offspring_1973_spouses <- read.dta("icpsr_yspouse73.dta")


couple_1973 <- X2waveparentspouse
offspring_1973 <- X2waveyouthspouse
offspring_1973_spouses <- icpsr_yspouse73



# merging variable in 1972-souses presumably v7805
length(which(offspring_1973_spouses$v7805 %in% offspring_1973$v262))  #check

offspring <- merge(offspring_1973_spouses, offspring_1973, by.x = "v7805", by.y = "v262")
#write.dta(offspring, "offspring_spouse_65_73.dta")
# 7848 party 5-point spouse, create 5-point party from v479 and 480
offspring$party_5 <- NA
offspring$party_5[offspring$v479 == 1 & offspring$v480 == 1] <- 5
offspring$party_5[offspring$v479 == 1 & offspring$v480 == 5] <- 4
offspring$party_5[offspring$v479 == 2] <- 3
offspring$party_5[offspring$v479 == 5 & offspring$v480 == 5] <- 2
offspring$party_5[offspring$v479 == 5 & offspring$v480 == 1] <- 1
CrossTable(offspring$party_5, offspring$v7848, prop.r = F, prop.c = F, prop.chisq = F)
inds = offspring$party_5[which(offspring$party_5 != 3 & offspring$v7848 != 3)]
inds_2 = offspring$v7848[which(offspring$party_5 != 3 & offspring$v7848 != 3)]

CrossTable(inds, inds_2, prop.r = F, prop.c = F, prop.chisq = F)

round((15 + 10 + 21 + 57 + 115 + 30 + 10 + 9 + 6)/503 * 100, 2)  #54.27% agreement
round((15 + 10 + 21 + 57 + 30 + 10 + 9 + 6)/182 * 100, 2)  #86.61% agreement
offspring$Party_p1 <- NA
offspring$Party_p1[offspring$party_5 < 3] <- "Dem"
offspring$Party_p1[offspring$party_5 == 3] <- "Ind"
offspring$Party_p1[offspring$party_5 > 3] <- "Rep"

offspring$Party_p2 <- NA
offspring$Party_p2[offspring$v7848 < 3] <- "Dem"
offspring$Party_p2[offspring$v7848 == 3] <- "Ind"
offspring$Party_p2[offspring$v7848 > 3] <- "Rep"
# mean
offspring$homogeneity <- NA
offspring$homogeneity[offspring$Party_p1 == "Dem" & offspring$Party_p2 == "Dem"] <- 1
offspring$homogeneity[offspring$Party_p1 == "Dem" & offspring$Party_p2 == "Rep"] <- 0
offspring$homogeneity[offspring$Party_p1 == "Rep" & offspring$Party_p2 == "Dem"] <- 0
offspring$homogeneity[offspring$Party_p1 == "Rep" & offspring$Party_p2 == "Rep"] <- 1
offspring$homogeneity[offspring$Party_p1 == "Ind" & offspring$Party_p2 == "Ind"] <- 1
offspring$homogeneity[offspring$Party_p1 == "Rep" & offspring$Party_p2 == "Ind"] <- 0
offspring$homogeneity[offspring$Party_p1 == "Ind" & offspring$Party_p2 == "Rep"] <- 0
offspring$homogeneity[offspring$Party_p1 == "Dem" & offspring$Party_p2 == "Ind"] <- 0
# 62 spouse_gen_complete$homogeneity[spouse_gen_complete$Party_p1 =='Ind'&spouse_gen_complete$Party_p2=='Dem']<-0
mean(offspring$homogeneity, na.rm = T)
length(which(!is.na(offspring$homogeneity)))
# se don't report
sqrt(62.04545 * (100 - 62.04545)/440)
2.313452