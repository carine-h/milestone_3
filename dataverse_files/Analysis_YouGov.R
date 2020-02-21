# setwd ('C:\\Users\\t-toko\\Dropbox\\VoterIssues\\PollingResults')
#setwd("C:\\Users\\tobia\\Dropbox\\Partisan Homogeneity Project\\Deliverables_20150921")
# check voter file load('C:/Users/tobia/Dropbox/Partisan Homogeneity
# Project/Deliverables_20150921/marriage_data.RData')
# data=data[(party_spouse_1=='Democrat'|party_spouse_1=='Republican')&(party_spouse_2=='Democrat'|party_spouse_2=='Republican')]
# crosstab(data$party_spouse_1, data$party_spouse_2,plot=F)
(3867143 + 3713198)/9145677 * 100
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
library(psychometric)
library(ggplot2)
library(apsrtable)
#source("C:/Users/tobia/Dropbox/R-Functions/Causal Inference/baltestcollect.R")
recode_nas <- function(x) {
    require(car)
    x <- recode(x, "2=NA")
    x
}
library(Matching)
library(scales)
library(Rcpp)
library(devtools)
theme_set(theme_bw())
recode<-car::recode

########################################## read in data##############################
set.seed(2141)
# length of marriage
#couples = as.data.set(spss.system.file("STAN0078_COUPLES_OUTPUT2.sav"))
couples$stan099_p2 <- as.numeric(couples$stan099_p2)
couples$stan099_p1 <- as.numeric(couples$stan099_p1)
couples$birthyr_p1 <- as.numeric(couples$birthyr_p1)
couples$birthyr_p2 <- as.numeric(couples$birthyr_p2)
# match couples, fathers and mothers
fathers = couples[couples$gender_p1 == "Male", c(1, 3, 5, 12, grep("p1", colnames(couples))[-(1:11)])]
colnames(fathers)[1] <- "caseid_p1"
fathers$complier_p1 <- 1
colnames(fathers) <- gsub("p1", "fathers", colnames(fathers))
fathers_2 = couples[couples$gender_p2 == "Male", c(1, 3, 5, 12, grep("p2", colnames(couples)))]
fathers_2$educ_p1 <- NA
fathers_2$complier_p2 <- 0
colnames(fathers_2)[1] <- "caseid_p2"
colnames(fathers_2)[2] <- "educ_p2"
colnames(fathers_2)[3] <- "faminc_p2"
colnames(fathers_2)[4] <- "inputzip_p2"
colnames(fathers_2) <- gsub("p2", "fathers", colnames(fathers_2))
fathers = rbind(data.frame(fathers), data.frame(fathers_2))
# match couples, mothers
mothers = couples[couples$gender_p1 == "Female", c(1, 3, 5, grep("p1", colnames(couples))[-(1:11)])]
colnames(mothers)[1] <- "caseid_p1"
mothers$complier_p1 <- 1
colnames(mothers) <- gsub("p1", "mothers", colnames(mothers))
mothers_2 = couples[couples$gender_p2 == "Female", c(1, 3, 5, grep("p2", colnames(couples)))]
mothers_2$educ_p1 <- NA
mothers_2$complier_p2 <- 0
colnames(mothers_2)[1] <- "caseid_p2"
colnames(mothers_2)[2] <- "educ_p2"
colnames(mothers_2)[3] <- "faminc_p2"
colnames(mothers_2) <- gsub("p2", "mothers", colnames(mothers_2))
mothers = rbind(data.frame(mothers), data.frame(mothers_2))
colnames(mothers) <- gsub("mothers", "p2", colnames(mothers))
colnames(fathers) <- gsub("fathers", "p1", colnames(fathers))
couples_dyad <- merge(fathers, mothers, by.x = "caseid_p1", by.y = "caseid_p2")
# trhow out homosexual pairs
couples_dyad = couples_dyad[(couples_dyad$gender_p1 == "Female" & couples_dyad$gender_p2 == "Male") | 
    (couples_dyad$gender_p2 == "Female" & couples_dyad$gender_p1 == "Male"), ]
######################### 
# checks
which(couples_dyad$caseid_p1 != couples_dyad$caseid_p2)
which(couples_dyad$gender_p1 == couples_dyad$gender_p2)
summary(couples_dyad$gender_p1)
summary(couples_dyad$gender_p2)
###### 
couples_dyad$marriage_length_p1 <- 2015 - couples_dyad$stan099_p1
couples_dyad$marriage_length_p2 <- 2015 - couples_dyad$stan099_p2
# education and discussion
couples_dyad$education_p1 <- recode(as.numeric(couples_dyad$educ_p1), "1=1;2=1;3=2;4=3;5=3;6=3;")
couples_dyad$education_p2 <- recode(as.numeric(couples_dyad$educ_p2), "1=1;2=1;3=2;4=3;5=3;6=3;")
couples_dyad$education_overall <- ifelse(is.na(couples_dyad$education_p1), couples_dyad$education_p2, 
    couples_dyad$education_p1)
couples_dyad$educ_overall <- factor(couples_dyad$education_overall, labels = c("HS or less", "Some college", 
    "College+"))

couples_dyad$spousal_discussion_p1<-with(couples_dyad, recode(as.numeric(stan101_p1),"1=3;2=2;3=1;4=0"))
couples_dyad$spousal_discussion_p2<-with(couples_dyad, recode(as.numeric(stan101_p2),"1=3;2=2;3=1;4=0"))
couples_dyad$spousal_discussion_overall=(couples_dyad$spousal_discussion_p1+couples_dyad$spousal_discussion_p2)/2

# couples_dyad=couples_dyad[couples_dyad$pid7_p2!='Not sure'&couples_dyad$pid7_p1!='Not
# sure'&!is.na(couples_dyad$pid7_p2)&!is.na(couples_dyad$pid7_p2),]
# couples_dyad$pid7_p2<-factor(couples_dyad$pid7_p2);couples_dyad$pid7_p1<-factor(couples_dyad$pid7_p1)
couples_dyad$Party_p1 <- recode(as.numeric(couples_dyad$pid7_p1), "8=NA")
couples_dyad$Party_p2 <- recode(as.numeric(couples_dyad$pid7_p2), "8=NA")
couples_dyad$Party_p1_3 <- recode(couples_dyad$pid3_p1, "'Other'=NA;'No preference'=NA")
couples_dyad$Party_p2_3 <- recode(couples_dyad$pid3_p2, "'Other'=NA;'No preference'=NA")
polychor(couples_dyad$Party_p1_3, couples_dyad$Party_p2_3)
t.test(as.numeric(couples_dyad$Party_p1_3), as.numeric(couples_dyad$Party_p2_3))
mean(abs(as.numeric(couples_dyad$Party_p1_3) - as.numeric(couples_dyad$Party_p2_3)), na.rm = T)/sd(c(as.numeric(couples_dyad$Party_p1_3)), 
    na.rm = T)
couples_dyad$Ideology_p1 <- recode(as.numeric(couples_dyad$stan019_p1), "8=NA")
couples_dyad$Ideology_p2 <- recode(as.numeric(couples_dyad$stan019_p2), "8=NA")
crosstab(couples_dyad$Party_p1, couples_dyad$Party_p2)
round((85 + 5 + 8 + 17 + 11 + 6 + 22 + 2 + 15 + 31 + 35 + 16 + 27 + 10 + 19 + 21 + 18 + 18 + 84)/552 * 
    100, 2)
crosstab(couples_dyad$Party_p1_3, couples_dyad$Party_p2_3, plot = F)
round((122 + 83 + 138)/495 * 100, 2)
# se
sqrt(81.52174 * (100 - 81.52174)/552)
crosstab(couples_dyad$Party_p1[couples_dyad$pid7_p1 != "Independent" & couples_dyad$pid7_p2 != "Independent"], 
    couples_dyad$Party_p2[couples_dyad$pid7_p1 != "Independent" & couples_dyad$pid7_p2 != "Independent"], 
    dnn = c("Spouse1", "Spouse2"))
polychor(couples_dyad$Party_p1, couples_dyad$Party_p2)
cor.test(couples_dyad$Party_p1, couples_dyad$Party_p2)
# get ci
polychor(couples_dyad$Party_p1, couples_dyad$Party_p2, std.err = T, ML = T)
# ci
polychor(couples_dyad$Party_p1, couples_dyad$Party_p2) + 1.96 * 0.01964
polychor(couples_dyad$Party_p1, couples_dyad$Party_p2) - 1.96 * 0.01964
# for #recently married couples
polychor(couples_dyad$Party_p1, couples_dyad$Party_p2)
t.test(as.numeric(couples_dyad$Party_p1), as.numeric(couples_dyad$Party_p2))
# with(couples_dyad, polychor(Party_p1[Party_p1<4&Party_p2<4],Party_p2[Party_p1<4&Party_p2<4]))
# with(couples_dyad, polychor(Party_p1[Party_p1>4&Party_p2>4],Party_p2[Party_p1>4&Party_p2>4]))
polychor(couples_dyad$Ideology_p1, couples_dyad$Ideology_p2)
crosstab(couples_dyad$Party_p1, couples_dyad$Party_p2, plot = F, dnn = c("Spouse1", "Spouse2"))
# 7 point scale #homogeneity (1,2,3) (4) and (5, 6, 7));
round((85 + 5 + 8 + 17 + 11 + 6 + 22 + 2 + 15 + 31 + 35 + 16 + 27 + 10 + 19 + 21 + 18 + 18 + 84)/552 * 
    100, 2)
# 7 point scale #homogeneity (1,2,3) and (5, 6, 7)) 4 exlucded;
round((85 + 5 + 8 + 17 + 11 + 6 + 22 + 2 + 15 + 35 + 16 + 27 + 10 + 19 + 21 + 18 + 18 + 84)/451 * 100, 
    3)
# disagreement
round((5 + 2 + 1 + 1 + 3 + 1 + 1 + 5 + 5 + 4 + 1 + 1 + 2)/552 * 100, 2)  #5.8
################### non-policy domains
#### Interest in non-political things
# homogeneity in interest for creative things
polychor(couples_dyad$stan061_p1, couples_dyad$stan061_p2)
# homogeneity in interest in volounteering
polychor(couples_dyad$stan065_p1, couples_dyad$stan065_p2)
# homogeneity in interest in learning new skills
polychor(couples_dyad$stan068_p1, couples_dyad$stan068_p2)
# homogeneity in interest in outdoor activities
polychor(couples_dyad$stan070_p1, couples_dyad$stan070_p2)
# latent
couples_dyad$latent_interest_p1 <- fscores(mirt(cbind(as.numeric(couples_dyad$stan061_p1), as.numeric(couples_dyad$stan062_p1), 
    as.numeric(couples_dyad$stan063_p1), as.numeric(couples_dyad$stan064_p1), as.numeric(couples_dyad$stan065_p1), 
    as.numeric(couples_dyad$stan066_p1), as.numeric(couples_dyad$stan067_p1), as.numeric(couples_dyad$stan068_p1), 
    as.numeric(couples_dyad$stan069_p1), as.numeric(couples_dyad$stan070_p1)), 1, itemtype = "graded"), 
    full.scores = T, scores.only = T)
couples_dyad$latent_interest_p2 <- fscores(mirt(cbind(as.numeric(couples_dyad$stan061_p2), as.numeric(couples_dyad$stan062_p2), 
    as.numeric(couples_dyad$stan063_p2), as.numeric(couples_dyad$stan064_p2), as.numeric(couples_dyad$stan065_p2), 
    as.numeric(couples_dyad$stan066_p2), as.numeric(couples_dyad$stan067_p2), as.numeric(couples_dyad$stan068_p2), 
    as.numeric(couples_dyad$stan069_p2), as.numeric(couples_dyad$stan070_p2)), 1, itemtype = "graded"), 
    full.scores = T, scores.only = T)
cor.test(couples_dyad$latent_interest_p1, couples_dyad$latent_interest_p2)
#### Personality of Children Respect for elders/independence
couples_dyad$independence_p1 <- as.numeric(couples_dyad$stan020_p1)
couples_dyad$independence_p2 <- as.numeric(couples_dyad$stan020_p2)
polychor(couples_dyad$independence_p2, couples_dyad$independence_p1)
# obedience/self-reliance
couples_dyad$obedience_p1 <- recode(as.numeric(couples_dyad$stan021_p1), "1=2;2=1")
couples_dyad$obedience_p2 <- recode(as.numeric(couples_dyad$stan021_p2), "1=2;2=1")
polychor(couples_dyad$obedience_p1, couples_dyad$obedience_p2)
# good manners/curiosity
couples_dyad$manners_p1 <- as.numeric(couples_dyad$stan022_p1)
couples_dyad$manners_p2 <- as.numeric(couples_dyad$stan022_p2)
polychor(couples_dyad$manners_p1, couples_dyad$manners_p2)
# considerate vs. well-behaved
couples_dyad$behaved_p1 <- recode(as.numeric(couples_dyad$stan023_p1), "1=2;2=1")
couples_dyad$behaved_p2 <- recode(as.numeric(couples_dyad$stan023_p2), "1=2;2=1")
polychor(couples_dyad$behaved_p1, couples_dyad$behaved_p2)
# latent
latent_personality_p1_holder <- with(couples_dyad, fscores(mirt(cbind(independence_p1, obedience_p1, 
    manners_p1, behaved_p1)[-115, ], 1, itemtype = "graded"), full.scores = T, scores.only = T))
couples_dyad$latent_personality_p1 <- append(latent_personality_p1_holder, NA, after = 114)
latent_personality_p2_holder <- with(couples_dyad, fscores(mirt(cbind(independence_p2, obedience_p2, 
    manners_p2, behaved_p2)[-524, ], 1, itemtype = "graded"), full.scores = T, scores.only = T))
couples_dyad$latent_personality_p2 <- append(latent_personality_p2_holder, NA, after = 523)
cor.test(couples_dyad$latent_personality_p1, couples_dyad$latent_personality_p2, use = "complete.obs")
######################## use ideology, policy positions, and thermometer scores to get a latent variable score for
######################## independents recode consistently to avoide negative discrimination parameters;
######################## liberal-conservative, no opinion as center position
# replicate Shanto's numbers for bivariate issue pairs ideology
couples_dyad$ideology_p1 <- recode(as.numeric(couples_dyad$stan019_p1), "8=NA")
couples_dyad$ideology_p2 <- recode(as.numeric(couples_dyad$stan019_p2), "8=NA")
polychor(couples_dyad$ideology_p1, couples_dyad$ideology_p2)
# immigrants
couples_dyad$immigrants_p1 <- recode(as.numeric(couples_dyad$stan024_p1), "2=3;3=2")
couples_dyad$immigrants_p2 <- recode(as.numeric(couples_dyad$stan024_p2), "2=3;3=2")
polychor(couples_dyad$immigrants_p1, couples_dyad$immigrants_p2)
# gvt. regulation of business
couples_dyad$regulation_p1 <- recode(as.numeric(couples_dyad$stan025_p1), "2=3;3=2")
couples_dyad$regulation_p2 <- recode(as.numeric(couples_dyad$stan025_p2), "2=3;3=2")
polychor(couples_dyad$regulation_p1, couples_dyad$regulation_p2)
# people convicted of murder, no opinion between death penalty and prison sentence
couples_dyad$crime_p1 <- recode(as.numeric(couples_dyad$stan026_p1), "1=4;2=2;3=1;4=3")
couples_dyad$crime_p2 <- recode(as.numeric(couples_dyad$stan026_p2), "1=4;2=2;3=1;4=3")
polychor(couples_dyad$crime_p1, couples_dyad$crime_p2)
# income inequality
couples_dyad$income_p1 <- recode(as.numeric(couples_dyad$stan028_p1), "2=3;3=2")
couples_dyad$income_p2 <- recode(as.numeric(couples_dyad$stan028_p2), "2=3;3=2")
polychor(couples_dyad$income_p1, couples_dyad$income_p2)
# services, don't knows as NAS
couples_dyad$services_p1 <- recode(as.numeric(couples_dyad$stan029_p1), "1=7;2=6;3=5;4=4;5=3;6=2;7=1;8=NA")
couples_dyad$services_p2 <- recode(as.numeric(couples_dyad$stan029_p2), "1=7;2=6;3=5;4=4;5=3;6=2;7=1;8=NA")
polychor(couples_dyad$services_p1, couples_dyad$services_p2)
# healthcare, no opinion as have no effect
couples_dyad$healthcare_p1 <- recode(as.numeric(couples_dyad$stan030_p1), "2=3;3=2;4=2")
couples_dyad$healthcare_p2 <- recode(as.numeric(couples_dyad$stan030_p2), "2=3;3=2;4=2")
polychor(couples_dyad$healthcare_p2, couples_dyad$healthcare_p1)
# abortion
couples_dyad$abortion_p1 <- recode(as.numeric(couples_dyad$stan031_p1), "1=3;2=1;3=2")
couples_dyad$abortion_p2 <- recode(as.numeric(couples_dyad$stan031_p2), "1=3;2=1;3=2")
polychor(couples_dyad$abortion_p2, couples_dyad$abortion_p1)
# troops for Isis
couples_dyad$groundtroops_p1 <- recode(as.numeric(couples_dyad$stan032_p1), "1=3;2=1;3=2")
couples_dyad$groundtroops_p2 <- recode(as.numeric(couples_dyad$stan032_p2), "1=3;2=1;3=2")
polychor(couples_dyad$groundtroops_p1, couples_dyad$groundtroops_p2)
# marijuana
couples_dyad$marijuana_p1 <- as.numeric(couples_dyad$stan033_p1)
couples_dyad$marijuana_p2 <- as.numeric(couples_dyad$stan033_p2)
polychor(couples_dyad$marijuana_p2, couples_dyad$marijuana_p1)
# gay marriage
couples_dyad$marriage_p1 <- recode(as.numeric(couples_dyad$stan034_p1), "2=3;3=2")
couples_dyad$marriage_p2 <- recode(as.numeric(couples_dyad$stan034_p2), "2=3;3=2")
# three-partite items immigrants, income regulation, income inequality, healthcare, abortion, isis,
# marijuana, gay marriage
mean(c(mean(c(couples_dyad$Party_p1 < 4 & couples_dyad$immigrants_p1 < 2, couples_dyad$Party_p2 < 4 & 
    couples_dyad$immigrants_p2 < 2) | c(couples_dyad$Party_p1 > 4 & couples_dyad$immigrants_p1 > 2, couples_dyad$Party_p2 > 
    4 & couples_dyad$immigrants_p2 > 2), na.rm = T), mean(c(couples_dyad$Party_p1 < 4 & couples_dyad$regulation_p1 < 
    2, couples_dyad$Party_p2 < 4 & couples_dyad$regulation_p2 < 2) | c(couples_dyad$Party_p1 > 4 & couples_dyad$regulation_p1 > 
    2, couples_dyad$Party_p2 > 4 & couples_dyad$regulation_p2 > 2), na.rm = T), mean(c(couples_dyad$Party_p1 < 
    4 & couples_dyad$income_p1 < 2, couples_dyad$Party_p2 < 4 & couples_dyad$income_p2 < 2) | c(couples_dyad$Party_p1 > 
    4 & couples_dyad$income_p1 > 2, couples_dyad$Party_p2 > 4 & couples_dyad$income_p2 > 2), na.rm = T), 
    mean(c(couples_dyad$Party_p1 < 4 & couples_dyad$healthcare_p1 < 2, couples_dyad$Party_p2 < 4 & couples_dyad$healthcare_p2 < 
        2) | c(couples_dyad$Party_p1 > 4 & couples_dyad$healthcare_p1 > 2, couples_dyad$Party_p2 > 4 & 
        couples_dyad$healthcare_p2 > 2), na.rm = T), mean(c(couples_dyad$Party_p1 < 4 & couples_dyad$abortion_p1 < 
        2, couples_dyad$Party_p2 < 4 & couples_dyad$abortion_p2 < 2) | c(couples_dyad$Party_p1 > 4 & 
        couples_dyad$abortion_p1 > 2, couples_dyad$Party_p2 > 4 & couples_dyad$abortion_p2 > 2), na.rm = T), 
    mean(c(couples_dyad$Party_p1 < 4 & couples_dyad$marriage_p1 < 2, couples_dyad$Party_p2 < 4 & couples_dyad$marriage_p2 < 
        2) | c(couples_dyad$Party_p1 > 4 & couples_dyad$marriage_p1 > 2, couples_dyad$Party_p2 > 4 & 
        couples_dyad$marriage_p2 > 2), na.rm = T)))
############### thermoeter scores
couples_dyad$homogeneity <- NA
couples_dyad$homogeneity[couples_dyad$Party_p1 < 4 & couples_dyad$Party_p2 < 4] <- 1
couples_dyad$homogeneity[couples_dyad$Party_p1 < 4 & couples_dyad$Party_p2 > 4] <- 0
couples_dyad$homogeneity[couples_dyad$Party_p1 > 4 & couples_dyad$Party_p2 < 4] <- 0
couples_dyad$homogeneity[couples_dyad$Party_p1 > 4 & couples_dyad$Party_p2 > 4] <- 1
couples_dyad$homogeneity[couples_dyad$Party_p1 == 4 & couples_dyad$Party_p2 == 4] <- 1
couples_dyad$homogeneity[couples_dyad$Party_p1 > 4 & couples_dyad$Party_p2 == 4] <- 0
couples_dyad$homogeneity[couples_dyad$Party_p1 == 4 & couples_dyad$Party_p2 > 4] <- 0
couples_dyad$homogeneity[couples_dyad$Party_p1 < 4 & couples_dyad$Party_p2 == 4] <- 0
couples_dyad$homogeneity[couples_dyad$Party_p1 == 4 & couples_dyad$Party_p2 < 4] <- 0
mean(couples_dyad$homogeneity, na.rm = T)
# homogeneity by education
crosstab(couples_dyad$homogeneity, couples_dyad$educ_overall, prop.c = T)
# Democrat
couples_dyad$dem_feeling_p1 <- 100 - (as.numeric(couples_dyad$stan004_p1) - 1)
couples_dyad$dem_feeling_p2 <- 100 - (as.numeric(couples_dyad$stan004_p2) - 1)
round(cor(couples_dyad$dem_feeling_p1, couples_dyad$dem_feeling_p2, use = "complete.obs"), 2)
couples_dyad$dem_feeling_ordinal_p1 <- NA
couples_dyad$dem_feeling_ordinal_p1[couples_dyad$dem_feeling_p1 > 80] <- 5
couples_dyad$dem_feeling_ordinal_p1[couples_dyad$dem_feeling_p1 <= 80 & couples_dyad$dem_feeling_p1 > 
    60] <- 4
couples_dyad$dem_feeling_ordinal_p1[couples_dyad$dem_feeling_p1 <= 60 & couples_dyad$dem_feeling_p1 > 
    40] <- 3
couples_dyad$dem_feeling_ordinal_p1[couples_dyad$dem_feeling_p1 <= 40 & couples_dyad$dem_feeling_p1 > 
    20] <- 2
couples_dyad$dem_feeling_ordinal_p1[couples_dyad$dem_feeling_p1 <= 20] <- 1
couples_dyad$dem_feeling_ordinal_p2 <- NA
couples_dyad$dem_feeling_ordinal_p2[couples_dyad$dem_feeling_p2 > 80] <- 5
couples_dyad$dem_feeling_ordinal_p2[couples_dyad$dem_feeling_p2 <= 80 & couples_dyad$dem_feeling_p2 > 
    60] <- 4
couples_dyad$dem_feeling_ordinal_p2[couples_dyad$dem_feeling_p2 <= 60 & couples_dyad$dem_feeling_p2 > 
    40] <- 3
couples_dyad$dem_feeling_ordinal_p2[couples_dyad$dem_feeling_p2 <= 40 & couples_dyad$dem_feeling_p2 > 
    20] <- 2
couples_dyad$dem_feeling_ordinal_p2[couples_dyad$dem_feeling_p2 <= 20] <- 1
# Republican
couples_dyad$rep_feeling_p1 <- (as.numeric(couples_dyad$stan005_p1) - 1)
couples_dyad$rep_feeling_p2 <- (as.numeric(couples_dyad$stan005_p2) - 1)
round(cor(couples_dyad$rep_feeling_p1, couples_dyad$rep_feeling_p2, use = "complete.obs"), 2)
couples_dyad$rep_feeling_ordinal_p1 <- NA
couples_dyad$rep_feeling_ordinal_p1[couples_dyad$rep_feeling_p1 > 80] <- 5
couples_dyad$rep_feeling_ordinal_p1[couples_dyad$rep_feeling_p1 <= 80 & couples_dyad$rep_feeling_p1 > 
    60] <- 4
couples_dyad$rep_feeling_ordinal_p1[couples_dyad$rep_feeling_p1 <= 60 & couples_dyad$rep_feeling_p1 > 
    40] <- 3
couples_dyad$rep_feeling_ordinal_p1[couples_dyad$rep_feeling_p1 <= 40 & couples_dyad$rep_feeling_p1 > 
    20] <- 2
couples_dyad$rep_feeling_ordinal_p1[couples_dyad$rep_feeling_p1 <= 20] <- 1
couples_dyad$rep_feeling_ordinal_p2 <- NA
couples_dyad$rep_feeling_ordinal_p2[couples_dyad$rep_feeling_p2 > 80] <- 5
couples_dyad$rep_feeling_ordinal_p2[couples_dyad$rep_feeling_p2 <= 80 & couples_dyad$rep_feeling_p2 > 
    60] <- 4
couples_dyad$rep_feeling_ordinal_p2[couples_dyad$rep_feeling_p2 <= 60 & couples_dyad$rep_feeling_p2 > 
    40] <- 3
couples_dyad$rep_feeling_ordinal_p2[couples_dyad$rep_feeling_p2 <= 40 & couples_dyad$rep_feeling_p2 > 
    20] <- 2
couples_dyad$rep_feeling_ordinal_p2[couples_dyad$rep_feeling_p2 <= 20] <- 1
# Tea party
couples_dyad$tea_feeling_p1 <- (as.numeric(couples_dyad$stan006_p1) - 1)
couples_dyad$tea_feeling_p2 <- (as.numeric(couples_dyad$stan006_p2) - 1)
round(cor(couples_dyad$tea_feeling_p1, couples_dyad$tea_feeling_p2, use = "complete.obs"), 2)
couples_dyad$tea_feeling_ordinal_p1 <- NA
couples_dyad$tea_feeling_ordinal_p1[couples_dyad$tea_feeling_p1 > 80] <- 5
couples_dyad$tea_feeling_ordinal_p1[couples_dyad$tea_feeling_p1 <= 80 & couples_dyad$tea_feeling_p1 > 
    60] <- 4
couples_dyad$tea_feeling_ordinal_p1[couples_dyad$tea_feeling_p1 <= 60 & couples_dyad$tea_feeling_p1 > 
    40] <- 3
couples_dyad$tea_feeling_ordinal_p1[couples_dyad$tea_feeling_p1 <= 40 & couples_dyad$tea_feeling_p1 > 
    20] <- 2
couples_dyad$tea_feeling_ordinal_p1[couples_dyad$tea_feeling_p1 <= 20] <- 1
couples_dyad$tea_feeling_ordinal_p2 <- NA
couples_dyad$tea_feeling_ordinal_p2[couples_dyad$tea_feeling_p2 > 80] <- 5
couples_dyad$tea_feeling_ordinal_p2[couples_dyad$tea_feeling_p2 <= 80 & couples_dyad$tea_feeling_p2 > 
    60] <- 4
couples_dyad$tea_feeling_ordinal_p2[couples_dyad$tea_feeling_p2 <= 60 & couples_dyad$tea_feeling_p2 > 
    40] <- 3
couples_dyad$tea_feeling_ordinal_p2[couples_dyad$tea_feeling_p2 <= 40 & couples_dyad$tea_feeling_p2 > 
    20] <- 2
couples_dyad$tea_feeling_ordinal_p2[couples_dyad$tea_feeling_p2 <= 20] <- 1
# Obama
couples_dyad$obama_feeling_p1 <- 100 - (as.numeric(couples_dyad$stan007_p1) - 1)
couples_dyad$obama_feeling_p2 <- 100 - (as.numeric(couples_dyad$stan007_p2) - 1)
round(cor(couples_dyad$obama_feeling_p1, couples_dyad$obama_feeling_p2, use = "complete.obs"), 2)
couples_dyad$obama_feeling_ordinal_p1 <- NA
couples_dyad$obama_feeling_ordinal_p1[couples_dyad$obama_feeling_p1 > 80] <- 5
couples_dyad$obama_feeling_ordinal_p1[couples_dyad$obama_feeling_p1 <= 80 & couples_dyad$obama_feeling_p1 > 
    60] <- 4
couples_dyad$obama_feeling_ordinal_p1[couples_dyad$obama_feeling_p1 <= 60 & couples_dyad$obama_feeling_p1 > 
    40] <- 3
couples_dyad$obama_feeling_ordinal_p1[couples_dyad$obama_feeling_p1 <= 40 & couples_dyad$obama_feeling_p1 > 
    20] <- 2
couples_dyad$obama_feeling_ordinal_p1[couples_dyad$obama_feeling_p1 <= 20] <- 1
couples_dyad$obama_feeling_ordinal_p2 <- NA
couples_dyad$obama_feeling_ordinal_p2[couples_dyad$obama_feeling_p2 > 80] <- 5
couples_dyad$obama_feeling_ordinal_p2[couples_dyad$obama_feeling_p2 <= 80 & couples_dyad$obama_feeling_p2 > 
    60] <- 4
couples_dyad$obama_feeling_ordinal_p2[couples_dyad$obama_feeling_p2 <= 60 & couples_dyad$obama_feeling_p2 > 
    40] <- 3
couples_dyad$obama_feeling_ordinal_p2[couples_dyad$obama_feeling_p2 <= 40 & couples_dyad$obama_feeling_p2 > 
    20] <- 2
couples_dyad$obama_feeling_ordinal_p2[couples_dyad$obama_feeling_p2 <= 20] <- 1
# Clinton
couples_dyad$clinton_feeling_p1 <- 100 - (as.numeric(couples_dyad$stan008_p1) - 1)
couples_dyad$clinton_feeling_p2 <- 100 - (as.numeric(couples_dyad$stan008_p2) - 1)
round(cor(couples_dyad$clinton_feeling_p2, couples_dyad$clinton_feeling_p1, use = "complete.obs"), 2)
couples_dyad$clinton_feeling_ordinal_p1 <- NA
couples_dyad$clinton_feeling_ordinal_p1[couples_dyad$clinton_feeling_p1 > 80] <- 5
couples_dyad$clinton_feeling_ordinal_p1[couples_dyad$clinton_feeling_p1 <= 80 & couples_dyad$clinton_feeling_p1 > 
    60] <- 4
couples_dyad$clinton_feeling_ordinal_p1[couples_dyad$clinton_feeling_p1 <= 60 & couples_dyad$clinton_feeling_p1 > 
    40] <- 3
couples_dyad$clinton_feeling_ordinal_p1[couples_dyad$clinton_feeling_p1 <= 40 & couples_dyad$clinton_feeling_p1 > 
    20] <- 2
couples_dyad$clinton_feeling_ordinal_p1[couples_dyad$clinton_feeling_p1 <= 20] <- 1
couples_dyad$clinton_feeling_ordinal_p2 <- NA
couples_dyad$clinton_feeling_ordinal_p2[couples_dyad$clinton_feeling_p2 > 80] <- 5
couples_dyad$clinton_feeling_ordinal_p2[couples_dyad$clinton_feeling_p2 <= 80 & couples_dyad$clinton_feeling_p2 > 
    60] <- 4
couples_dyad$clinton_feeling_ordinal_p2[couples_dyad$clinton_feeling_p2 <= 60 & couples_dyad$clinton_feeling_p2 > 
    40] <- 3
couples_dyad$clinton_feeling_ordinal_p2[couples_dyad$clinton_feeling_p2 <= 40 & couples_dyad$clinton_feeling_p2 > 
    20] <- 2
couples_dyad$clinton_feeling_ordinal_p2[couples_dyad$clinton_feeling_p2 <= 20] <- 1
# Paul
couples_dyad$paul_feeling_p1 <- (as.numeric(couples_dyad$stan009_p1) - 1)
couples_dyad$paul_feeling_p2 <- (as.numeric(couples_dyad$stan009_p2) - 1)
round(cor(couples_dyad$paul_feeling_p1, couples_dyad$paul_feeling_p2, use = "complete.obs"), 2)
couples_dyad$paul_feeling_ordinal_p1 <- NA
couples_dyad$paul_feeling_ordinal_p1[couples_dyad$paul_feeling_p1 > 80] <- 5
couples_dyad$paul_feeling_ordinal_p1[couples_dyad$paul_feeling_p1 <= 80 & couples_dyad$paul_feeling_p1 > 
    60] <- 4
couples_dyad$paul_feeling_ordinal_p1[couples_dyad$paul_feeling_p1 <= 60 & couples_dyad$paul_feeling_p1 > 
    40] <- 3
couples_dyad$paul_feeling_ordinal_p1[couples_dyad$paul_feeling_p1 <= 40 & couples_dyad$paul_feeling_p1 > 
    20] <- 2
couples_dyad$paul_feeling_ordinal_p1[couples_dyad$paul_feeling_p1 <= 20] <- 1
couples_dyad$paul_feeling_ordinal_p2 <- NA
couples_dyad$paul_feeling_ordinal_p2[couples_dyad$paul_feeling_p2 > 80] <- 5
couples_dyad$paul_feeling_ordinal_p2[couples_dyad$paul_feeling_p2 <= 80 & couples_dyad$paul_feeling_p2 > 
    60] <- 4
couples_dyad$paul_feeling_ordinal_p2[couples_dyad$paul_feeling_p2 <= 60 & couples_dyad$paul_feeling_p2 > 
    40] <- 3
couples_dyad$paul_feeling_ordinal_p2[couples_dyad$paul_feeling_p2 <= 40 & couples_dyad$paul_feeling_p2 > 
    20] <- 2
couples_dyad$paul_feeling_ordinal_p2[couples_dyad$paul_feeling_p2 <= 20] <- 1
# Bush
couples_dyad$bush_feeling_p1 <- (as.numeric(couples_dyad$stan010_p1) - 1)
couples_dyad$bush_feeling_p2 <- (as.numeric(couples_dyad$stan010_p2) - 1)
round(cor(couples_dyad$bush_feeling_p1, couples_dyad$bush_feeling_p2, use = "complete.obs"), 2)
couples_dyad$bush_feeling_ordinal_p1 <- NA
couples_dyad$bush_feeling_ordinal_p1[couples_dyad$bush_feeling_p1 > 80] <- 5
couples_dyad$bush_feeling_ordinal_p1[couples_dyad$bush_feeling_p1 <= 80 & couples_dyad$bush_feeling_p1 > 
    60] <- 4
couples_dyad$bush_feeling_ordinal_p1[couples_dyad$bush_feeling_p1 <= 60 & couples_dyad$bush_feeling_p1 > 
    40] <- 3
couples_dyad$bush_feeling_ordinal_p1[couples_dyad$bush_feeling_p1 <= 40 & couples_dyad$bush_feeling_p1 > 
    20] <- 2
couples_dyad$bush_feeling_ordinal_p1[couples_dyad$bush_feeling_p1 <= 20] <- 1
couples_dyad$bush_feeling_ordinal_p2 <- NA
couples_dyad$bush_feeling_ordinal_p2[couples_dyad$bush_feeling_p2 > 80] <- 5
couples_dyad$bush_feeling_ordinal_p2[couples_dyad$bush_feeling_p2 <= 80 & couples_dyad$bush_feeling_p2 > 
    60] <- 4
couples_dyad$bush_feeling_ordinal_p2[couples_dyad$bush_feeling_p2 <= 60 & couples_dyad$bush_feeling_p2 > 
    40] <- 3
couples_dyad$bush_feeling_ordinal_p2[couples_dyad$bush_feeling_p2 <= 40 & couples_dyad$bush_feeling_p2 > 
    20] <- 2
couples_dyad$bush_feeling_ordinal_p2[couples_dyad$bush_feeling_p2 <= 20] <- 1
# Cruz
couples_dyad$cruz_feeling_p1 <- (as.numeric(couples_dyad$stan011_p1) - 1)
couples_dyad$cruz_feeling_p2 <- (as.numeric(couples_dyad$stan011_p2) - 1)
round(cor(couples_dyad$cruz_feeling_p1, couples_dyad$cruz_feeling_p2, use = "complete.obs"), 2)
couples_dyad$cruz_feeling_ordinal_p1 <- NA
couples_dyad$cruz_feeling_ordinal_p1[couples_dyad$cruz_feeling_p1 > 80] <- 5
couples_dyad$cruz_feeling_ordinal_p1[couples_dyad$cruz_feeling_p1 <= 80 & couples_dyad$cruz_feeling_p1 > 
    60] <- 4
couples_dyad$cruz_feeling_ordinal_p1[couples_dyad$cruz_feeling_p1 <= 60 & couples_dyad$cruz_feeling_p1 > 
    40] <- 3
couples_dyad$cruz_feeling_ordinal_p1[couples_dyad$cruz_feeling_p1 <= 40 & couples_dyad$cruz_feeling_p1 > 
    20] <- 2
couples_dyad$cruz_feeling_ordinal_p1[couples_dyad$cruz_feeling_p1 <= 20] <- 1
couples_dyad$cruz_feeling_ordinal_p2 <- NA
couples_dyad$cruz_feeling_ordinal_p2[couples_dyad$cruz_feeling_p2 > 80] <- 5
couples_dyad$cruz_feeling_ordinal_p2[couples_dyad$cruz_feeling_p2 <= 80 & couples_dyad$cruz_feeling_p2 > 
    60] <- 4
couples_dyad$cruz_feeling_ordinal_p2[couples_dyad$cruz_feeling_p2 <= 60 & couples_dyad$cruz_feeling_p2 > 
    40] <- 3
couples_dyad$cruz_feeling_ordinal_p2[couples_dyad$cruz_feeling_p2 <= 40 & couples_dyad$cruz_feeling_p2 > 
    20] <- 2
couples_dyad$cruz_feeling_ordinal_p2[couples_dyad$cruz_feeling_p2 <= 20] <- 1
# Trump
couples_dyad$trump_feeling_p1 <- (as.numeric(couples_dyad$stan011b_p1) - 1)
couples_dyad$trump_feeling_p2 <- (as.numeric(couples_dyad$stan011b_p2) - 1)
round(cor(couples_dyad$trump_feeling_p1, couples_dyad$trump_feeling_p2, use = "complete.obs"), 2)
couples_dyad$trump_feeling_ordinal_p1 <- NA
couples_dyad$trump_feeling_ordinal_p1[couples_dyad$trump_feeling_p1 > 80] <- 5
couples_dyad$trump_feeling_ordinal_p1[couples_dyad$trump_feeling_p1 <= 80 & couples_dyad$trump_feeling_p1 > 
    60] <- 4
couples_dyad$trump_feeling_ordinal_p1[couples_dyad$trump_feeling_p1 <= 60 & couples_dyad$trump_feeling_p1 > 
    40] <- 3
couples_dyad$trump_feeling_ordinal_p1[couples_dyad$trump_feeling_p1 <= 40 & couples_dyad$trump_feeling_p1 > 
    20] <- 2
couples_dyad$trump_feeling_ordinal_p1[couples_dyad$trump_feeling_p1 <= 20] <- 1
couples_dyad$trump_feeling_ordinal_p2 <- NA
couples_dyad$trump_feeling_ordinal_p2[couples_dyad$trump_feeling_p2 > 80] <- 5
couples_dyad$trump_feeling_ordinal_p2[couples_dyad$trump_feeling_p2 <= 80 & couples_dyad$trump_feeling_p2 > 
    60] <- 4
couples_dyad$trump_feeling_ordinal_p2[couples_dyad$trump_feeling_p2 <= 60 & couples_dyad$trump_feeling_p2 > 
    40] <- 3
couples_dyad$trump_feeling_ordinal_p2[couples_dyad$trump_feeling_p2 <= 40 & couples_dyad$trump_feeling_p2 > 
    20] <- 2
couples_dyad$trump_feeling_ordinal_p2[couples_dyad$trump_feeling_p2 <= 20] <- 1
# NRA
couples_dyad$nra_feeling_p1 <- (as.numeric(couples_dyad$stan012_p1) - 1)
couples_dyad$nra_feeling_p2 <- (as.numeric(couples_dyad$stan012_p2) - 1)
round(cor(couples_dyad$nra_feeling_p1, couples_dyad$nra_feeling_p2, use = "complete.obs"), 2)
couples_dyad$nra_feeling_ordinal_p1 <- NA
couples_dyad$nra_feeling_ordinal_p1[couples_dyad$nra_feeling_p1 > 80] <- 5
couples_dyad$nra_feeling_ordinal_p1[couples_dyad$nra_feeling_p1 <= 80 & couples_dyad$nra_feeling_p1 > 
    60] <- 4
couples_dyad$nra_feeling_ordinal_p1[couples_dyad$nra_feeling_p1 <= 60 & couples_dyad$nra_feeling_p1 > 
    40] <- 3
couples_dyad$nra_feeling_ordinal_p1[couples_dyad$nra_feeling_p1 <= 40 & couples_dyad$nra_feeling_p1 > 
    20] <- 2
couples_dyad$nra_feeling_ordinal_p1[couples_dyad$nra_feeling_p1 <= 20] <- 1
couples_dyad$nra_feeling_ordinal_p2 <- NA
couples_dyad$nra_feeling_ordinal_p2[couples_dyad$nra_feeling_p2 > 80] <- 5
couples_dyad$nra_feeling_ordinal_p2[couples_dyad$nra_feeling_p2 <= 80 & couples_dyad$nra_feeling_p2 > 
    60] <- 4
couples_dyad$nra_feeling_ordinal_p2[couples_dyad$nra_feeling_p2 <= 60 & couples_dyad$nra_feeling_p2 > 
    40] <- 3
couples_dyad$nra_feeling_ordinal_p2[couples_dyad$nra_feeling_p2 <= 40 & couples_dyad$nra_feeling_p2 > 
    20] <- 2
couples_dyad$nra_feeling_ordinal_p2[couples_dyad$nra_feeling_p2 <= 20] <- 1
# gay
couples_dyad$gay_feeling_p1 <- 100 - (as.numeric(couples_dyad$stan013_p1) - 1)
couples_dyad$gay_feeling_p2 <- 100 - (as.numeric(couples_dyad$stan013_p2) - 1)
round(cor(couples_dyad$gay_feeling_p1, couples_dyad$gay_feeling_p2, use = "complete.obs"), 2)
couples_dyad$gay_feeling_ordinal_p1 <- NA
couples_dyad$gay_feeling_ordinal_p1[couples_dyad$gay_feeling_p1 > 80] <- 5
couples_dyad$gay_feeling_ordinal_p1[couples_dyad$gay_feeling_p1 <= 80 & couples_dyad$gay_feeling_p1 > 
    60] <- 4
couples_dyad$gay_feeling_ordinal_p1[couples_dyad$gay_feeling_p1 <= 60 & couples_dyad$gay_feeling_p1 > 
    40] <- 3
couples_dyad$gay_feeling_ordinal_p1[couples_dyad$gay_feeling_p1 <= 40 & couples_dyad$gay_feeling_p1 > 
    20] <- 2
couples_dyad$gay_feeling_ordinal_p1[couples_dyad$gay_feeling_p1 <= 20] <- 1
couples_dyad$gay_feeling_ordinal_p2 <- NA
couples_dyad$gay_feeling_ordinal_p2[couples_dyad$gay_feeling_p2 > 80] <- 5
couples_dyad$gay_feeling_ordinal_p2[couples_dyad$gay_feeling_p2 <= 80 & couples_dyad$gay_feeling_p2 > 
    60] <- 4
couples_dyad$gay_feeling_ordinal_p2[couples_dyad$gay_feeling_p2 <= 60 & couples_dyad$gay_feeling_p2 > 
    40] <- 3
couples_dyad$gay_feeling_ordinal_p2[couples_dyad$gay_feeling_p2 <= 40 & couples_dyad$gay_feeling_p2 > 
    20] <- 2
couples_dyad$gay_feeling_ordinal_p2[couples_dyad$gay_feeling_p2 <= 20] <- 1
# Christians
couples_dyad$christians_feeling_p1 <- (as.numeric(couples_dyad$stan014_p1) - 1)
couples_dyad$christians_feeling_p2 <- (as.numeric(couples_dyad$stan014_p2) - 1)
round(cor(couples_dyad$christians_feeling_p1, couples_dyad$christians_feeling_p2, use = "complete.obs"), 
    2)
couples_dyad$christians_feeling_ordinal_p1 <- NA
couples_dyad$christians_feeling_ordinal_p1[couples_dyad$christians_feeling_p1 > 80] <- 5
couples_dyad$christians_feeling_ordinal_p1[couples_dyad$christians_feeling_p1 <= 80 & couples_dyad$christians_feeling_p1 > 
    60] <- 4
couples_dyad$christians_feeling_ordinal_p1[couples_dyad$christians_feeling_p1 <= 60 & couples_dyad$christians_feeling_p1 > 
    40] <- 3
couples_dyad$christians_feeling_ordinal_p1[couples_dyad$christians_feeling_p1 <= 40 & couples_dyad$christians_feeling_p1 > 
    20] <- 2
couples_dyad$christians_feeling_ordinal_p1[couples_dyad$christians_feeling_p1 <= 20] <- 1
couples_dyad$christians_feeling_ordinal_p2 <- NA
couples_dyad$christians_feeling_ordinal_p2[couples_dyad$christians_feeling_p2 > 80] <- 5
couples_dyad$christians_feeling_ordinal_p2[couples_dyad$christians_feeling_p2 <= 80 & couples_dyad$christians_feeling_p2 > 
    60] <- 4
couples_dyad$christians_feeling_ordinal_p2[couples_dyad$christians_feeling_p2 <= 60 & couples_dyad$christians_feeling_p2 > 
    40] <- 3
couples_dyad$christians_feeling_ordinal_p2[couples_dyad$christians_feeling_p2 <= 40 & couples_dyad$christians_feeling_p2 > 
    20] <- 2
couples_dyad$christians_feeling_ordinal_p2[couples_dyad$christians_feeling_p2 <= 20] <- 1
# Atheists
couples_dyad$atheists_feeling_p1 <- 100 - (as.numeric(couples_dyad$stan015_p1) - 1)
couples_dyad$atheists_feeling_p2 <- 100 - (as.numeric(couples_dyad$stan015_p2) - 1)
round(cor(couples_dyad$atheists_feeling_p1, couples_dyad$atheists_feeling_p2, use = "complete.obs"), 
    2)
couples_dyad$atheists_feeling_ordinal_p1 <- NA
couples_dyad$atheists_feeling_ordinal_p1[couples_dyad$atheists_feeling_p1 > 80] <- 5
couples_dyad$atheists_feeling_ordinal_p1[couples_dyad$atheists_feeling_p1 <= 80 & couples_dyad$atheists_feeling_p1 > 
    60] <- 4
couples_dyad$atheists_feeling_ordinal_p1[couples_dyad$atheists_feeling_p1 <= 60 & couples_dyad$atheists_feeling_p1 > 
    40] <- 3
couples_dyad$atheists_feeling_ordinal_p1[couples_dyad$atheists_feeling_p1 <= 40 & couples_dyad$atheists_feeling_p1 > 
    20] <- 2
couples_dyad$atheists_feeling_ordinal_p1[couples_dyad$atheists_feeling_p1 <= 20] <- 1
couples_dyad$atheists_feeling_ordinal_p2 <- NA
couples_dyad$atheists_feeling_ordinal_p2[couples_dyad$atheists_feeling_p2 > 80] <- 5
couples_dyad$atheists_feeling_ordinal_p2[couples_dyad$atheists_feeling_p2 <= 80 & couples_dyad$atheists_feeling_p2 > 
    60] <- 4
couples_dyad$atheists_feeling_ordinal_p2[couples_dyad$atheists_feeling_p2 <= 60 & couples_dyad$atheists_feeling_p2 > 
    40] <- 3
couples_dyad$atheists_feeling_ordinal_p2[couples_dyad$atheists_feeling_p2 <= 40 & couples_dyad$atheists_feeling_p2 > 
    20] <- 2
couples_dyad$atheists_feeling_ordinal_p2[couples_dyad$atheists_feeling_p2 <= 20] <- 1
# groups, porbably better to do in terms of quintiles
couples_dyad$latent_group_p1 <- with(couples_dyad, fscores(mirt(cbind(tea_feeling_ordinal_p1, nra_feeling_ordinal_p1, 
    gay_feeling_ordinal_p1, christians_feeling_ordinal_p1, atheists_feeling_ordinal_p1), 1, itemtype = "graded"), 
    full.scores = TRUE, scores.only = TRUE))
couples_dyad$latent_group_p2 <- with(couples_dyad, fscores(mirt(cbind(tea_feeling_ordinal_p2, nra_feeling_ordinal_p2, 
    gay_feeling_ordinal_p2, christians_feeling_ordinal_p2, atheists_feeling_ordinal_p2), 1, itemtype = "graded"), 
    full.scores = TRUE, scores.only = TRUE))
cor(couples_dyad$latent_group_p1, couples_dyad$latent_group_p2)
# by party,dems first, republicans second
with(couples_dyad, cor(latent_group_p1[Party_p1 < 4 & Party_p2 < 4], latent_group_p2[Party_p1 < 4 & Party_p2 < 
    4], use = "complete.obs"))
with(couples_dyad, cor(latent_group_p1[Party_p1 > 4 & Party_p2 > 4], latent_group_p2[Party_p1 > 4 & Party_p2 > 
    4], use = "complete.obs"))
# persons/politicians
couples_dyad$latent_political_group_p1 <- with(couples_dyad, fscores(mirt(cbind(dem_feeling_ordinal_p1, 
    rep_feeling_ordinal_p1, obama_feeling_ordinal_p1, clinton_feeling_ordinal_p1, paul_feeling_ordinal_p1, 
    bush_feeling_ordinal_p1, cruz_feeling_ordinal_p1, trump_feeling_ordinal_p1), 1, itemtype = "graded"), 
    full.scores = TRUE, scores.only = TRUE))
couples_dyad$latent_political_group_p2 <- with(couples_dyad, fscores(mirt(cbind(dem_feeling_ordinal_p2, 
    rep_feeling_ordinal_p2, obama_feeling_ordinal_p2, clinton_feeling_ordinal_p2, paul_feeling_ordinal_p2, 
    bush_feeling_ordinal_p2, cruz_feeling_ordinal_p2, trump_feeling_ordinal_p2), 1, itemtype = "graded"), 
    full.scores = TRUE, scores.only = TRUE))
cor(couples_dyad$latent_political_group_p1, couples_dyad$latent_political_group_p2)
# by party, dems first, republicans second
with(couples_dyad, cor(latent_political_group_p1[Party_p1 < 4 & Party_p2 < 4], latent_political_group_p2[Party_p1 < 
    4 & Party_p2 < 4], use = "complete.obs"))
with(couples_dyad, cor(latent_political_group_p1[Party_p1 > 4 & Party_p2 > 4], latent_political_group_p2[Party_p1 > 
    4 & Party_p2 > 4], use = "complete.obs"))
# overal partisan attitudes
couples_dyad$latent_partisan_attitudes_p1 <- with(couples_dyad, fscores(mirt(cbind(Ideology_p1, Party_p1, 
    tea_feeling_ordinal_p1, nra_feeling_ordinal_p1, gay_feeling_ordinal_p1, christians_feeling_ordinal_p1, 
    atheists_feeling_ordinal_p1, dem_feeling_ordinal_p1, rep_feeling_ordinal_p1, obama_feeling_ordinal_p1, 
    clinton_feeling_ordinal_p1, paul_feeling_ordinal_p1, bush_feeling_ordinal_p1, cruz_feeling_ordinal_p1, 
    trump_feeling_ordinal_p1), 1, itemtype = "graded"), full.scores = TRUE, scores.only = TRUE))
couples_dyad$latent_partisan_attitudes_p2 <- with(couples_dyad, fscores(mirt(cbind(Ideology_p2, Party_p2, 
    tea_feeling_ordinal_p2, nra_feeling_ordinal_p2, gay_feeling_ordinal_p2, christians_feeling_ordinal_p2, 
    atheists_feeling_ordinal_p2, dem_feeling_ordinal_p2, rep_feeling_ordinal_p2, obama_feeling_ordinal_p2, 
    clinton_feeling_ordinal_p2, paul_feeling_ordinal_p2, bush_feeling_ordinal_p2, cruz_feeling_ordinal_p2, 
    trump_feeling_ordinal_p2), 1, itemtype = "graded"), full.scores = TRUE, scores.only = TRUE))
cor.test(couples_dyad$latent_partisan_attitudes_p2, couples_dyad$latent_partisan_attitudes_p1)
# marriages/stereotyping
couples_dyad$marriage_dem_p1 <- with(couples_dyad, as.numeric(stan037_p1))
couples_dyad$marriage_dem_p2 <- with(couples_dyad, as.numeric(stan037_p2))
polychor(couples_dyad$marriage_dem_p1, couples_dyad$marriage_dem_p2)  #0.71 correlation betw
couples_dyad$marriage_rep_p1 <- with(couples_dyad, recode(as.numeric(stan038_p1), "1=3;2=2;3=1"))
couples_dyad$marriage_rep_p2 <- with(couples_dyad, recode(as.numeric(stan038_p2), "1=3;2=2;3=1"))
polychor(couples_dyad$marriage_rep_p1, couples_dyad$marriage_rep_p2)  #0.69 correlation betw
couples_dyad$inter_marriage_p1 <- ifelse(couples_dyad$Party_p1 < 4, couples_dyad$marriage_rep_p1, ifelse(couples_dyad$Party_p1 > 
    4, couples_dyad$marriage_dem_p1, NA))
couples_dyad$inter_marriage_p2 <- ifelse(couples_dyad$Party_p2 < 4, couples_dyad$marriage_rep_p2, ifelse(couples_dyad$Party_p2 > 
    4, couples_dyad$marriage_dem_p2, NA))
couples_dyad$stereotype_patriotic_democrat_p1 <- with(couples_dyad, recode(as.numeric(stan046_p1), "1=1;2=2;3=4;4=5;5=3"))
couples_dyad$stereotype_patriotic_democrat_p2 <- with(couples_dyad, recode(as.numeric(stan046_p2), "1=1;2=2;3=4;4=5;5=3"))
polychor(couples_dyad$stereotype_patriotic_democrat_p1, couples_dyad$stereotype_patriotic_democrat_p2)  #0.72 correlation betw
couples_dyad$stereotype_selfish_democrat_p1 <- with(couples_dyad, recode(as.numeric(stan047_p1), "1=5;2=4;3=2;4=1;5=3"))
couples_dyad$stereotype_selfish_democrat_p2 <- with(couples_dyad, recode(as.numeric(stan047_p2), "1=5;2=4;3=2;4=1;5=3"))
polychor(couples_dyad$stereotype_selfish_democrat_p1, couples_dyad$stereotype_selfish_democrat_p2)  #0.64 correlation betw
couples_dyad$stereotype_compromise_democrat_p1 <- with(couples_dyad, recode(as.numeric(stan048_p1), "1=1;2=2;3=4;4=5;5=3"))
couples_dyad$stereotype_compromise_democrat_p2 <- with(couples_dyad, recode(as.numeric(stan048_p2), "1=1;2=2;3=4;4=5;5=3"))
polychor(couples_dyad$stereotype_compromise_democrat_p1, couples_dyad$stereotype_compromise_democrat_p2)  #0.76 correlation betw
couples_dyad$stereotype_compassionate_democrat_p1 <- with(couples_dyad, recode(as.numeric(stan049_p1), 
    "1=1;2=2;3=4;4=5;5=3"))
couples_dyad$stereotype_compassionate_democrat_p2 <- with(couples_dyad, recode(as.numeric(stan049_p2), 
    "1=1;2=2;3=4;4=5;5=3"))
polychor(couples_dyad$stereotype_compassionate_democrat_p1, couples_dyad$stereotype_compassionate_democrat_p2)  #0.72 correlation betw
couples_dyad$stereotype_patriotic_republican_p1 <- with(couples_dyad, recode(as.numeric(stan052_p1), 
    "1=5;2=4;3=2;4=1;5=3"))
couples_dyad$stereotype_patriotic_republican_p2 <- with(couples_dyad, recode(as.numeric(stan052_p2), 
    "1=5;2=4;3=2;4=1;5=3"))
polychor(couples_dyad$stereotype_patriotic_republican_p1, couples_dyad$stereotype_patriotic_republican_p2)  #0.52 correlation betw
couples_dyad$stereotype_selfish_republican_p1 <- with(couples_dyad, recode(as.numeric(stan053_p1), "1=1;2=2;3=4;4=5;5=3"))
couples_dyad$stereotype_selfish_republican_p2 <- with(couples_dyad, recode(as.numeric(stan053_p2), "1=1;2=2;3=4;4=5;5=3"))
polychor(couples_dyad$stereotype_selfish_republican_p1, couples_dyad$stereotype_selfish_republican_p2)  #0.65 correlation betw
couples_dyad$stereotype_compromise_republican_p1 <- with(couples_dyad, recode(as.numeric(stan054_p1), 
    "1=5;2=4;3=2;4=1;5=3"))
couples_dyad$stereotype_compromise_republican_p2 <- with(couples_dyad, recode(as.numeric(stan054_p2), 
    "1=5;2=4;3=2;4=1;5=3"))
polychor(couples_dyad$stereotype_compromise_republican_p1, couples_dyad$stereotype_compromise_republican_p2)  #0.64 correlation betw
couples_dyad$stereotype_compassionate_republican_p1 <- with(couples_dyad, recode(as.numeric(stan055_p1), 
    "1=5;2=4;3=2;4=1;5=3"))
couples_dyad$stereotype_compassionate_republican_p2 <- with(couples_dyad, recode(as.numeric(stan055_p2), 
    "1=5;2=4;3=2;4=1;5=3"))
polychor(couples_dyad$stereotype_compassionate_republican_p1, couples_dyad$stereotype_compassionate_republican_p2)  #0.64 correlation betw
couples_dyad$inter_selfish_p1 <- ifelse(couples_dyad$Party_p1 < 4, couples_dyad$stereotype_selfish_republican_p1, 
    ifelse(couples_dyad$Party_p1 > 4, couples_dyad$stereotype_selfish_democrat_p1, NA))
couples_dyad$inter_selfish_p2 <- ifelse(couples_dyad$Party_p2 < 4, couples_dyad$stereotype_selfish_republican_p2, 
    ifelse(couples_dyad$Party_p2 > 4, couples_dyad$stereotype_selfish_democrat_p2, NA))
polychor(couples_dyad$marriage_p1, couples_dyad$marriage_p2)
# latent
couples_dyad$latent_stereotyping_p1 <- fscores(mirt(couples_dyad[, c("marriage_dem_p1", "marriage_rep_p1", 
    "stereotype_patriotic_democrat_p1", "stereotype_selfish_democrat_p1", "stereotype_compromise_democrat_p1", 
    "stereotype_compassionate_democrat_p1", "stereotype_patriotic_republican_p1", "stereotype_selfish_republican_p1", 
    "stereotype_compromise_republican_p1", "stereotype_compassionate_republican_p1")], 1), full.scores = T, 
    scores.only = T)
couples_dyad$latent_stereotyping_p2 <- fscores(mirt(couples_dyad[, c("marriage_dem_p2", "marriage_rep_p2", 
    "stereotype_patriotic_democrat_p2", "stereotype_selfish_democrat_p2", "stereotype_compromise_democrat_p2", 
    "stereotype_compassionate_democrat_p2", "stereotype_patriotic_republican_p2", "stereotype_selfish_republican_p2", 
    "stereotype_compromise_republican_p2", "stereotype_compassionate_republican_p2")], 1), full.scores = T, 
    scores.only = T)
with(couples_dyad, cor.test(latent_stereotyping_p1, latent_stereotyping_p2))
################################# racial resentment#############################
couples_dyad$resentment_p1 <- fscores(mirt(cbind(recode(as.numeric(couples_dyad$stan084_p1), "1=5;2=4;3=3;4=2;5=1"), 
    recode(as.numeric(couples_dyad$stan085_p1), "1=5;2=4;3=3;4=2;5=1"), recode(as.numeric(couples_dyad$stan086_p1), 
        "1=5;2=4;3=3;4=2;5=1"), as.numeric(couples_dyad$stan087_p1)), 1, itemtype = "graded"), full.scores = TRUE, 
    scores.only = TRUE)
couples_dyad$resentment_p2 <- fscores(mirt(cbind(recode(as.numeric(couples_dyad$stan084_p2), "1=5;2=4;3=3;4=2;5=1"), 
    recode(as.numeric(couples_dyad$stan085_p2), "1=5;2=4;3=3;4=2;5=1"), recode(as.numeric(couples_dyad$stan086_p2), 
        "1=5;2=4;3=3;4=2;5=1"), as.numeric(couples_dyad$stan087_p2)), 1, itemtype = "graded"), full.scores = TRUE, 
    scores.only = TRUE)
cor(couples_dyad$resentment_p1, couples_dyad$resentment_p2)
##################################### religious affiliation##################### religious service attendance
polychor(recode(as.numeric(couples_dyad$stan096_p1), "1=5;2=4;3=3;4=2;5=1"), recode(as.numeric(couples_dyad$stan096_p2), 
    "1=5;2=4;3=3;4=2;5=1"))
polychor(recode(as.numeric(couples_dyad$stan097_p1), "1=5;2=4;3=3;4=2;5=1"), recode(as.numeric(couples_dyad$stan097_p2), 
    "1=5;2=4;3=3;4=2;5=1"))
couples_dyad$religious_attendance_p1 <- fscores(mirt(cbind(recode(as.numeric(couples_dyad$stan096_p1), 
    "1=5;2=4;3=3;4=2;5=1"), recode(as.numeric(couples_dyad$stan097_p1), "1=5;2=4;3=3;4=2;5=1"), couples_dyad$christians_feeling_ordinal_p1, 
    couples_dyad$atheists_feeling_ordinal_p1), 1, itemtype = "graded"), full.scores = TRUE, scores.only = TRUE)
couples_dyad$religious_attendance_p2 <- fscores(mirt(cbind(recode(as.numeric(couples_dyad$stan096_p2), 
    "1=5;2=4;3=3;4=2;5=1"), recode(as.numeric(couples_dyad$stan097_p2), "1=5;2=4;3=3;4=2;5=1"), couples_dyad$christians_feeling_ordinal_p2, 
    couples_dyad$atheists_feeling_ordinal_p2), 1, itemtype = "graded"), full.scores = TRUE, scores.only = TRUE)
cor.test(couples_dyad$religious_attendance_p1, couples_dyad$religious_attendance_p2)
################################################ get latent ideology and party scores###########
couples_dyad$latent_ideology_p1 <- fscores(mirt(couples_dyad[, c("immigrants_p1", "regulation_p1", "crime_p1", 
    "income_p1", "services_p1", "healthcare_p1", "abortion_p1", "groundtroops_p1", "marijuana_p1", "marriage_p1")], 
    1), full.scores = T, scores.only = T)
couples_dyad$latent_ideology_p2 <- fscores(mirt(couples_dyad[, c("immigrants_p2", "regulation_p2", "crime_p2", 
    "income_p2", "services_p2", "healthcare_p2", "abortion_p2", "groundtroops_p2", "marijuana_p2", "marriage_p2")], 
    1), full.scores = T)
with(couples_dyad, cor.test(latent_ideology_p1, latent_ideology_p2))
latent_party_model_p1 = mirt(couples_dyad[, c("Ideology_p1", "Party_p1", "dem_feeling_ordinal_p1", "rep_feeling_ordinal_p1", 
    "tea_feeling_ordinal_p1", "obama_feeling_ordinal_p1", "clinton_feeling_ordinal_p1", "paul_feeling_ordinal_p1", 
    "bush_feeling_ordinal_p1", "cruz_feeling_ordinal_p1", "trump_feeling_ordinal_p1", "nra_feeling_ordinal_p1", 
    "gay_feeling_ordinal_p1")], 1)  #,'christians_feeling_ordinal_p1','atheists_feeling_ordinal_p1')],1)
latent_party_model_p2 = mirt(couples_dyad[, c("Ideology_p2", "Party_p1", "dem_feeling_ordinal_p2", "rep_feeling_ordinal_p2", 
    "tea_feeling_ordinal_p2", "obama_feeling_ordinal_p2", "clinton_feeling_ordinal_p2", "paul_feeling_ordinal_p2", 
    "bush_feeling_ordinal_p2", "cruz_feeling_ordinal_p2", "trump_feeling_ordinal_p2", "nra_feeling_ordinal_p2", 
    "gay_feeling_ordinal_p2")], 1)  #,'christians_feeling_ordinal_p2', 'atheists_feeling_ordinal_p2')],1)
couples_dyad$latent_party_p1 <- fscores(latent_party_model_p1, full.scores = T)
couples_dyad$latent_party_p2 <- fscores(latent_party_model_p2, full.scores = T)
######################### Modeling#######################################################
#load("homogeneity_by_zip_registration_partisan_denominator.RData")
couples_dyad <- data.table(couples_dyad)
couples_dyad$inputzip_p1 <- as.character(couples_dyad$inputzip_p1)
zip_code_just_partisan$zip <- as.character(zip_code_just_partisan$zip)
zip_code_just_partisan$zip = ifelse(nchar(zip_code_just_partisan$zip) == 3, paste0("00", zip_code_just_partisan$zip), 
    ifelse(nchar(zip_code_just_partisan$zip) == 4, paste0("0", zip_code_just_partisan$zip), zip_code_just_partisan$zip))
setkey(couples_dyad, inputzip_p1)
setkey(zip_code_just_partisan, zip)
couples_dyad = zip_code_just_partisan[couples_dyad]
# recode variables, set up data frame allowing for clutsering

histogram(couples_dyad$Homogeneity, freq = F, breaks = 20, type = "percent")
# convergence and non-linear age
couples_dyad$delta_male_female = couples_dyad$latent_party_p1 - couples_dyad$latent_party_p2
# joint
couples_dyad$delta_male_female_race = couples_dyad$resentment_p1 - couples_dyad$resentment_p2
couples_dyad$delta_male_female_religion = couples_dyad$religious_attendance_p1 - couples_dyad$religious_attendance_p2
couples_dyad$delta_male_female_interest = couples_dyad$latent_interest_p1 - couples_dyad$latent_interest_p2
couples_dyad$delta_male_female_authoritarianism = couples_dyad$latent_personality_p1 - couples_dyad$latent_personality_p2
couples_dyad$length_1 <- with(couples_dyad, ifelse(marriage_length_p1 < 15, 1, 0))
couples_dyad$length_2 <- with(couples_dyad, ifelse(marriage_length_p1 < 30 & marriage_length_p1 >= 15, 
    1, 0))
couples_dyad$length_3 <- with(couples_dyad, ifelse(marriage_length_p1 < 45 & marriage_length_p1 >= 30, 
    1, 0))
couples_dyad$length_4 <- with(couples_dyad, ifelse(marriage_length_p1 >= 45, 1, 0))
# agreement:
model_party_table <- lm(abs(delta_male_female) ~ marriage_length_p1 + education_overall + spousal_discussion_overall, 
    data = couples_dyad)
model_party_table$se <- coeftest(model_party_table, vcov = vcovHC(model_party_table, type = "HC1", cluster = "group"))[, 
    2]
model_religion_table <- lm(abs(delta_male_female_religion) ~ marriage_length_p1 + education_overall + 
    spousal_discussion_overall, data = couples_dyad)
model_religion_table$se <- coeftest(model_religion_table, vcov = vcovHC(model_religion_table, type = "HC1", 
    cluster = "group"))[, 2]
model_interest_table <- lm(abs(delta_male_female_interest) ~ marriage_length_p1 + education_overall + 
    spousal_discussion_overall, data = couples_dyad)
model_interest_table$se <- coeftest(model_interest_table, vcov = vcovHC(model_interest_table, type = "HC1", 
    cluster = "group"))[, 2]
model_race_table <- lm(abs(delta_male_female_race) ~ marriage_length_p1 + education_overall + spousal_discussion_overall, 
    data = couples_dyad)
model_race_table$se <- coeftest(model_race_table, vcov = vcovHC(model_race_table, type = "HC1", cluster = "group"))[, 
    2]
model_authoritarianism_table <- lm(abs(delta_male_female_authoritarianism) ~ marriage_length_p1 + education_overall + 
    spousal_discussion_overall, data = couples_dyad)
model_authoritarianism_table$se <- coeftest(model_authoritarianism_table, vcov = vcovHC(model_authoritarianism_table, 
    type = "HC1", cluster = "group"))[, 2]
apsrtable(model_party_table, model_religion_table,model_interest_table, digits = 3, 
    se = "robust")

#take out race and authoritarianism, put in interest
# interactions to get at induced selection:
model_authoritarianism_interaction <- lm(abs(delta_male_female) ~ education_overall + spousal_discussion_overall + 
    marriage_length_p1 * delta_male_female_authoritarianism, data = couples_dyad)
model_authoritarianism_interaction$se <- coeftest(model_authoritarianism_interaction, vcov = vcovHC(model_authoritarianism_interaction, 
    type = "HC1", cluster = "group"))[, 2]
model_race_interaction <- lm(abs(delta_male_female) ~ education_overall + spousal_discussion_overall + 
    marriage_length_p1 * delta_male_female_race, data = couples_dyad)
model_race_interaction$se <- coeftest(model_race_interaction, vcov = vcovHC(model_race_interaction, type = "HC1", 
    cluster = "group"))[, 2]
model_religion_interaction <- lm(abs(delta_male_female) ~ education_overall + spousal_discussion_overall + 
    marriage_length_p1 * delta_male_female_religion, data = couples_dyad)
model_religion_interaction$se <- coeftest(model_religion_interaction, vcov = vcovHC(model_religion_interaction, 
    type = "HC1", cluster = "group"))[, 2]
apsrtable(model_religion_interaction,  model_authoritarianism_interaction, digits = 3, 
    se = "robust", model.names = c("Religiosity","Authoritarianism"))

#take out racial attitudes

# intercept differences
(0.354 - 0.746)/sqrt(0.063^2 + 0.136^2)
# use robust standard errors
# save(couples_dyad, file='Couples_dyad.RData')
#################### Compute compliance rates################
non_compliers <- as.data.set(spss.system.file("STAN0078_COUPLES_wave1.sav"))
non_compliers$stan099_p1 <- as.numeric(non_compliers$stan099_p1)
non_compliers$birthyr_p1 <- as.numeric(non_compliers$birthyr_p1)
non_compliers$marriage_length_p1 <- 2015 - non_compliers$stan099_p1
non_compliers <- data.frame(non_compliers)
summary(non_compliers$gender)
# Party ideology
non_compliers$Party_p1 <- recode(as.numeric(non_compliers$pid7_p1), "8=NA")
non_compliers$Ideology_p1 <- recode(as.numeric(non_compliers$stan019_p1), "8=NA;997=NA")
############### thermoeter scores Democrat
non_compliers$dem_feeling_p1 <- 100 - (as.numeric(non_compliers$stan004_p1) - 1)
non_compliers$dem_feeling_ordinal_p1 <- NA
non_compliers$dem_feeling_ordinal_p1[non_compliers$dem_feeling_p1 > 80] <- 5
non_compliers$dem_feeling_ordinal_p1[non_compliers$dem_feeling_p1 <= 80 & non_compliers$dem_feeling_p1 > 
    60] <- 4
non_compliers$dem_feeling_ordinal_p1[non_compliers$dem_feeling_p1 <= 60 & non_compliers$dem_feeling_p1 > 
    40] <- 3
non_compliers$dem_feeling_ordinal_p1[non_compliers$dem_feeling_p1 <= 40 & non_compliers$dem_feeling_p1 > 
    20] <- 2
non_compliers$dem_feeling_ordinal_p1[non_compliers$dem_feeling_p1 <= 20] <- 1
# Republican
non_compliers$rep_feeling_p1 <- (as.numeric(non_compliers$stan005_p1) - 1)
non_compliers$rep_feeling_ordinal_p1 <- NA
non_compliers$rep_feeling_ordinal_p1[non_compliers$rep_feeling_p1 > 80] <- 5
non_compliers$rep_feeling_ordinal_p1[non_compliers$rep_feeling_p1 <= 80 & non_compliers$rep_feeling_p1 > 
    60] <- 4
non_compliers$rep_feeling_ordinal_p1[non_compliers$rep_feeling_p1 <= 60 & non_compliers$rep_feeling_p1 > 
    40] <- 3
non_compliers$rep_feeling_ordinal_p1[non_compliers$rep_feeling_p1 <= 40 & non_compliers$rep_feeling_p1 > 
    20] <- 2
non_compliers$rep_feeling_ordinal_p1[non_compliers$rep_feeling_p1 <= 20] <- 1
# Tea party
non_compliers$tea_feeling_p1 <- (as.numeric(non_compliers$stan006_p1) - 1)
non_compliers$tea_feeling_ordinal_p1 <- NA
non_compliers$tea_feeling_ordinal_p1[non_compliers$tea_feeling_p1 > 80] <- 5
non_compliers$tea_feeling_ordinal_p1[non_compliers$tea_feeling_p1 <= 80 & non_compliers$tea_feeling_p1 > 
    60] <- 4
non_compliers$tea_feeling_ordinal_p1[non_compliers$tea_feeling_p1 <= 60 & non_compliers$tea_feeling_p1 > 
    40] <- 3
non_compliers$tea_feeling_ordinal_p1[non_compliers$tea_feeling_p1 <= 40 & non_compliers$tea_feeling_p1 > 
    20] <- 2
non_compliers$tea_feeling_ordinal_p1[non_compliers$tea_feeling_p1 <= 20] <- 1
# Obama
non_compliers$obama_feeling_p1 <- 100 - (as.numeric(non_compliers$stan007_p1) - 1)
non_compliers$obama_feeling_ordinal_p1 <- NA
non_compliers$obama_feeling_ordinal_p1[non_compliers$obama_feeling_p1 > 80] <- 5
non_compliers$obama_feeling_ordinal_p1[non_compliers$obama_feeling_p1 <= 80 & non_compliers$obama_feeling_p1 > 
    60] <- 4
non_compliers$obama_feeling_ordinal_p1[non_compliers$obama_feeling_p1 <= 60 & non_compliers$obama_feeling_p1 > 
    40] <- 3
non_compliers$obama_feeling_ordinal_p1[non_compliers$obama_feeling_p1 <= 40 & non_compliers$obama_feeling_p1 > 
    20] <- 2
non_compliers$obama_feeling_ordinal_p1[non_compliers$obama_feeling_p1 <= 20] <- 1
# Clinton
non_compliers$clinton_feeling_p1 <- 100 - (as.numeric(non_compliers$stan008_p1) - 1)
non_compliers$clinton_feeling_ordinal_p1 <- NA
non_compliers$clinton_feeling_ordinal_p1[non_compliers$clinton_feeling_p1 > 80] <- 5
non_compliers$clinton_feeling_ordinal_p1[non_compliers$clinton_feeling_p1 <= 80 & non_compliers$clinton_feeling_p1 > 
    60] <- 4
non_compliers$clinton_feeling_ordinal_p1[non_compliers$clinton_feeling_p1 <= 60 & non_compliers$clinton_feeling_p1 > 
    40] <- 3
non_compliers$clinton_feeling_ordinal_p1[non_compliers$clinton_feeling_p1 <= 40 & non_compliers$clinton_feeling_p1 > 
    20] <- 2
non_compliers$clinton_feeling_ordinal_p1[non_compliers$clinton_feeling_p1 <= 20] <- 1
# Paul
non_compliers$paul_feeling_p1 <- (as.numeric(non_compliers$stan009_p1) - 1)
non_compliers$paul_feeling_ordinal_p1 <- NA
non_compliers$paul_feeling_ordinal_p1[non_compliers$paul_feeling_p1 > 80] <- 5
non_compliers$paul_feeling_ordinal_p1[non_compliers$paul_feeling_p1 <= 80 & non_compliers$paul_feeling_p1 > 
    60] <- 4
non_compliers$paul_feeling_ordinal_p1[non_compliers$paul_feeling_p1 <= 60 & non_compliers$paul_feeling_p1 > 
    40] <- 3
non_compliers$paul_feeling_ordinal_p1[non_compliers$paul_feeling_p1 <= 40 & non_compliers$paul_feeling_p1 > 
    20] <- 2
non_compliers$paul_feeling_ordinal_p1[non_compliers$paul_feeling_p1 <= 20] <- 1
# Bush
non_compliers$bush_feeling_p1 <- (as.numeric(non_compliers$stan010_p1) - 1)
non_compliers$bush_feeling_ordinal_p1 <- NA
non_compliers$bush_feeling_ordinal_p1[non_compliers$bush_feeling_p1 > 80] <- 5
non_compliers$bush_feeling_ordinal_p1[non_compliers$bush_feeling_p1 <= 80 & non_compliers$bush_feeling_p1 > 
    60] <- 4
non_compliers$bush_feeling_ordinal_p1[non_compliers$bush_feeling_p1 <= 60 & non_compliers$bush_feeling_p1 > 
    40] <- 3
non_compliers$bush_feeling_ordinal_p1[non_compliers$bush_feeling_p1 <= 40 & non_compliers$bush_feeling_p1 > 
    20] <- 2
non_compliers$bush_feeling_ordinal_p1[non_compliers$bush_feeling_p1 <= 20] <- 1
# Cruz
non_compliers$cruz_feeling_p1 <- (as.numeric(non_compliers$stan011_p1) - 1)
non_compliers$cruz_feeling_ordinal_p1 <- NA
non_compliers$cruz_feeling_ordinal_p1[non_compliers$cruz_feeling_p1 > 80] <- 5
non_compliers$cruz_feeling_ordinal_p1[non_compliers$cruz_feeling_p1 <= 80 & non_compliers$cruz_feeling_p1 > 
    60] <- 4
non_compliers$cruz_feeling_ordinal_p1[non_compliers$cruz_feeling_p1 <= 60 & non_compliers$cruz_feeling_p1 > 
    40] <- 3
non_compliers$cruz_feeling_ordinal_p1[non_compliers$cruz_feeling_p1 <= 40 & non_compliers$cruz_feeling_p1 > 
    20] <- 2
non_compliers$cruz_feeling_ordinal_p1[non_compliers$cruz_feeling_p1 <= 20] <- 1
# Trump
non_compliers$trump_feeling_p1 <- (as.numeric(non_compliers$stan011b_p1) - 1)
non_compliers$trump_feeling_ordinal_p1 <- NA
non_compliers$trump_feeling_ordinal_p1[non_compliers$trump_feeling_p1 > 80] <- 5
non_compliers$trump_feeling_ordinal_p1[non_compliers$trump_feeling_p1 <= 80 & non_compliers$trump_feeling_p1 > 
    60] <- 4
non_compliers$trump_feeling_ordinal_p1[non_compliers$trump_feeling_p1 <= 60 & non_compliers$trump_feeling_p1 > 
    40] <- 3
non_compliers$trump_feeling_ordinal_p1[non_compliers$trump_feeling_p1 <= 40 & non_compliers$trump_feeling_p1 > 
    20] <- 2
non_compliers$trump_feeling_ordinal_p1[non_compliers$trump_feeling_p1 <= 20] <- 1
# NRA
non_compliers$nra_feeling_p1 <- (as.numeric(non_compliers$stan012_p1) - 1)
non_compliers$nra_feeling_ordinal_p1 <- NA
non_compliers$nra_feeling_ordinal_p1[non_compliers$nra_feeling_p1 > 80] <- 5
non_compliers$nra_feeling_ordinal_p1[non_compliers$nra_feeling_p1 <= 80 & non_compliers$nra_feeling_p1 > 
    60] <- 4
non_compliers$nra_feeling_ordinal_p1[non_compliers$nra_feeling_p1 <= 60 & non_compliers$nra_feeling_p1 > 
    40] <- 3
non_compliers$nra_feeling_ordinal_p1[non_compliers$nra_feeling_p1 <= 40 & non_compliers$nra_feeling_p1 > 
    20] <- 2
non_compliers$nra_feeling_ordinal_p1[non_compliers$nra_feeling_p1 <= 20] <- 1
# gay
non_compliers$gay_feeling_p1 <- 100 - (as.numeric(non_compliers$stan013_p1) - 1)
non_compliers$gay_feeling_ordinal_p1 <- NA
non_compliers$gay_feeling_ordinal_p1[non_compliers$gay_feeling_p1 > 80] <- 5
non_compliers$gay_feeling_ordinal_p1[non_compliers$gay_feeling_p1 <= 80 & non_compliers$gay_feeling_p1 > 
    60] <- 4
non_compliers$gay_feeling_ordinal_p1[non_compliers$gay_feeling_p1 <= 60 & non_compliers$gay_feeling_p1 > 
    40] <- 3
non_compliers$gay_feeling_ordinal_p1[non_compliers$gay_feeling_p1 <= 40 & non_compliers$gay_feeling_p1 > 
    20] <- 2
non_compliers$gay_feeling_ordinal_p1[non_compliers$gay_feeling_p1 <= 20] <- 1
# Christians
non_compliers$christians_feeling_p1 <- (as.numeric(non_compliers$stan014_p1) - 1)
non_compliers$christians_feeling_ordinal_p1 <- NA
non_compliers$christians_feeling_ordinal_p1[non_compliers$christians_feeling_p1 > 80] <- 5
non_compliers$christians_feeling_ordinal_p1[non_compliers$christians_feeling_p1 <= 80 & non_compliers$christians_feeling_p1 > 
    60] <- 4
non_compliers$christians_feeling_ordinal_p1[non_compliers$christians_feeling_p1 <= 60 & non_compliers$christians_feeling_p1 > 
    40] <- 3
non_compliers$christians_feeling_ordinal_p1[non_compliers$christians_feeling_p1 <= 40 & non_compliers$christians_feeling_p1 > 
    20] <- 2
non_compliers$christians_feeling_ordinal_p1[non_compliers$christians_feeling_p1 <= 20] <- 1
# Atheists
non_compliers$atheists_feeling_p1 <- 100 - (as.numeric(non_compliers$stan015_p1) - 1)
non_compliers$atheists_feeling_ordinal_p1 <- NA
non_compliers$atheists_feeling_ordinal_p1[non_compliers$atheists_feeling_p1 > 80] <- 5
non_compliers$atheists_feeling_ordinal_p1[non_compliers$atheists_feeling_p1 <= 80 & non_compliers$atheists_feeling_p1 > 
    60] <- 4
non_compliers$atheists_feeling_ordinal_p1[non_compliers$atheists_feeling_p1 <= 60 & non_compliers$atheists_feeling_p1 > 
    40] <- 3
non_compliers$atheists_feeling_ordinal_p1[non_compliers$atheists_feeling_p1 <= 40 & non_compliers$atheists_feeling_p1 > 
    20] <- 2
non_compliers$atheists_feeling_ordinal_p1[non_compliers$atheists_feeling_p1 <= 20] <- 1
# merge with homogeneity data
non_compliers <- data.table(non_compliers)
non_compliers$inputzip_p1 <- as.character(non_compliers$inputzip_p1)
zip_code_just_partisan$zip <- as.character(zip_code_just_partisan$zip)
setkey(non_compliers, inputzip_p1)
setkey(zip_code_just_partisan, zip)
non_compliers = zip_code_just_partisan[non_compliers]
non_compliers$latent_party_p1 = fscores(mirt(data.frame(non_compliers)[, c("Ideology_p1", "Party_p1", 
    "dem_feeling_ordinal_p1", "rep_feeling_ordinal_p1", "tea_feeling_ordinal_p1", "obama_feeling_ordinal_p1", 
    "clinton_feeling_ordinal_p1", "paul_feeling_ordinal_p1", "bush_feeling_ordinal_p1", "cruz_feeling_ordinal_p1", 
    "trump_feeling_ordinal_p1", "nra_feeling_ordinal_p1", "gay_feeling_ordinal_p1", "christians_feeling_ordinal_p1", 
    "atheists_feeling_ordinal_p1")], 1), full.scores = T)
# new_data=data.frame(non_compliers)[,c('gender_p1','marriage_length_p1', 'Homogeneity',
# 'latent_party_p1' )] new_data=na.omit(new_data)
# latent_party_p2<-ifelse(new_data$gender_p1=='Male',predict(model1,
# newdata=new_data,type='terms',na.action=na.exclude),NA) colnames(new_data)[4]<-'latent_party_p2'
# latent_party_p2<-ifelse(new_data$gender_p1=='Female', predict(model2,
# newdata=new_data,type='terms',na.action=na.exclude),latent_party_p2)
# which(rowSums(is.na(data.frame(non_compliers)[,c('gender_p1','marriage_length_p1',
# 'Partisan_Homogeneity', 'latent_party_p1' )]))>=1)
# latent_party_p2<-append(latent_party_p2,NA,after=0);latent_party_p2<-append(latent_party_p2,NA,after=54)
# latent_party_p2<-append(latent_party_p2,NA,after=129);latent_party_p2<-append(latent_party_p2,NA,after=166)
# latent_party_p2<-append(latent_party_p2,NA,after=290);latent_party_p2<-append(latent_party_p2,NA,after=301)
# latent_party_p2<-append(latent_party_p2,NA,after=418);latent_party_p2<-append(latent_party_p2,NA,after=419)
# latent_party_p2<-append(latent_party_p2,NA,after=420);latent_party_p2<-append(latent_party_p2,NA,after=421)
# which(is.na(latent_party_p2))
# non_compliers$latent_party_p2<-latent_party_p2
# non_compliers$gender_p2<-ifelse(non_compliers$gender_p1=='Male', 'Female', 'Male')
# ballance
selection <- data.frame(Party = c(non_compliers$Party_p1, ifelse(couples_dyad$complier_p1 == 1, couples_dyad$Party_p1, 
    couples_dyad$Party_p2)), Marriage_Length = c(non_compliers$marriage_length_p1, ifelse(couples_dyad$complier_p1 == 
    1, couples_dyad$marriage_length_p1, couples_dyad$marriage_length_p2)), Age = c(2015 - non_compliers$birthyr_p1, 
    ifelse(couples_dyad$complier_p1 == 1, 2015 - couples_dyad$birthyr_p1, 2015 - couples_dyad$birthyr_p2)), 
    Income = c(non_compliers$faminc_p1, couples_dyad$faminc_p1), Gender = c(as.numeric(non_compliers$gender_p1) - 
        1, ifelse(couples_dyad$complier_p1 == 1, as.numeric(couples_dyad$gender_p1) - 1, as.numeric(couples_dyad$gender_p2) - 
        1)), Latent_party = c(non_compliers$latent_party_p1, ifelse(couples_dyad$complier_p1 == 1, couples_dyad$latent_party_p1, 
        couples_dyad$latent_party_p2)), selection = c(rep(0, nrow(non_compliers)), rep(1, nrow(couples_dyad))))
mb <- MatchBalance(selection ~ Party + Marriage_Length + Age + Income + Gender + Latent_party, digits = 3, 
    data = selection, print.level = 0)
ballance_pre <- baltest.collect(matchbal.out = mb, var.names = c("Party", "Length of Marriage", "Age", 
    "Income", "Female", "Latent Party"), after = FALSE)
dotplot(rownames(ballance_pre) ~ ballance_pre[, "sdiff"]/100, xlab = list("Standardized Bias", font = 2), 
    pch = 3, col = "black", scales = list(cex = 3), cex = 3, par.settings = list(axis.text = list(cex = 3), 
        par.xlab.text = list(cex = 3), par.ylab.text = list(cex = 3)))
######################################################################## 
######## now intergenerational########################################
setwd("C:\\Users\\tobia\\Dropbox\\Partisan Homogeneity Project\\Deliverables_20150921")
p_c_dyad <- as.data.set(spss.system.file("STAN0078_OUTPUT2.sav"))
p_c_dyad <- data.frame(p_c_dyad)
# summary statistics#######################
dim(p_c_dyad)
### issue agreement bewteen parents-kids (in cases of disagreement, throw out, in case of one don't
### know response, compare to parent with opinion) sorting in 1972, 1983? get dsiiagreement as well
# ideology and pid
p_c_dyad$Party_k = recode(as.numeric(p_c_dyad$pid7_kids), "8=NA")
p_c_dyad$Party_f = recode(as.numeric(p_c_dyad$pid7_parents), "8=NA")
p_c_dyad$party_perception <- with(p_c_dyad, ifelse(gender_parents == "Female", as.numeric(stan003_kids), 
    as.numeric(stan002_kids)))
p_c_dyad$party_perception <- recode(as.numeric(p_c_dyad$party_perception), "4=NA;5=NA")
p_c_dyad$Party_f_simple <- recode(as.numeric(p_c_dyad$pid3_parents), "2=3;3=2;4=NA;5=NA")
# perception
polychor(p_c_dyad$Party_f_simple, p_c_dyad$party_perception)
crosstab(p_c_dyad$Party_f_simple, p_c_dyad$party_perception, prop.r = F, prop.c = F, prop.chisq = F, 
    plot = F)
round((160 + 61 + 139)/438 * 100, 2)
# missclassification
round((5 + 6)/299 * 100, 2)
polychor(p_c_dyad$Party_f_simple[p_c_dyad$gender_parents == "Female"], p_c_dyad$party_perception[p_c_dyad$gender_parents == 
    "Female"])
polychor(p_c_dyad$Party_f_simple[p_c_dyad$gender_parents == "Male"], p_c_dyad$party_perception[p_c_dyad$gender_parents == 
    "Male"])
polychor(p_c_dyad$Party_k, p_c_dyad$Party_f)
CrossTable(p_c_dyad$Party_k, p_c_dyad$Party_f, prop.r = F, prop.t = F, prop.chisq = F)
round((78 + 14 + 10 + 18 + 24 + 9 + 18 + 9 + 12 + 35 + 11 + 9 + 15 + 4 + 11 + 20 + 11 + 7 + 39)/477 * 
    100, 2)
# se
sqrt(74.21384 * (100 - 74.21384)/477)
# disagreement
round((1 + 6 + 5 + 2 + 8 + 4 + 6 + 1 + 10 + 3 + 1 + 3 + 1 + 1 + 1 + 1)/477 * 100, 2)
# get ci polychor(p_c_dyad$Party_k,p_c_dyad$Party_f)#,std.err = T,ML=T)
# ci
polychor(p_c_dyad$Party_k, p_c_dyad$Party_f) + 1.96 * 0.02681
polychor(p_c_dyad$Party_k, p_c_dyad$Party_f) - 1.96 * 0.02681
p_c_dyad$Party_k_3 <- recode(as.numeric(p_c_dyad$pid7_kids), "1=1;2=1;3=1;4=2;5=3;6=3;7=3;8=NA")
p_c_dyad$Party_f_3 <- recode(as.numeric(p_c_dyad$pid7_parents), "1=1;2=1;3=1;4=2;5=3;6=3;7=3;8=NA")
polychor(p_c_dyad$Party_k_3, p_c_dyad$Party_f_3)
p_c_dyad$homogeneity = as.numeric(p_c_dyad$Party_f_3 == p_c_dyad$Party_k_3)
# 0.75 ideology
p_c_dyad$Ideology_f = recode(as.numeric(p_c_dyad$stan019_parents), "8=NA")
p_c_dyad$Ideology_k = recode(as.numeric(p_c_dyad$stan019_kids), "8=NA")
polychor(p_c_dyad$Ideology_k, p_c_dyad$Ideology_f)
# immigrants
p_c_dyad$immigrants_k <- recode(as.numeric(p_c_dyad$stan024_kids), "2=3;3=2")
p_c_dyad$immigrants_f <- recode(as.numeric(p_c_dyad$stan024_parents), "2=3;3=2;4=NA")
polychor(p_c_dyad$immigrants_k, p_c_dyad$immigrants_f)
t.test(p_c_dyad$immigrants_f, p_c_dyad$immigrants_k, var.equal = F)
# gvt. regulation of business
p_c_dyad$regulation_k <- recode(as.numeric(p_c_dyad$stan025_kids), "2=3;3=2;4=NA")
p_c_dyad$regulation_f <- recode(as.numeric(p_c_dyad$stan025_parents), "2=3;3=2;4=NA")
polychor(p_c_dyad$regulation_k, p_c_dyad$regulation_f)
t.test(p_c_dyad$regulation_f, p_c_dyad$regulation_k, var.equal = F)
# income inequality
p_c_dyad$income_k <- recode(as.numeric(p_c_dyad$stan028_kids), "2=3;3=2")
p_c_dyad$income_f <- recode(as.numeric(p_c_dyad$stan028_parents), "2=3;3=2")
polychor(p_c_dyad$income_k, p_c_dyad$income_f)
t.test(p_c_dyad$income_k, p_c_dyad$income_f, var.equal = F)
# healthcare, no opinion as have no effect
p_c_dyad$healthcare_k <- recode(as.numeric(p_c_dyad$stan030_kids), "2=3;3=2;4=2")
p_c_dyad$healthcare_f <- recode(as.numeric(p_c_dyad$stan030_parents), "2=3;3=2;4=2")
polychor(p_c_dyad$healthcare_k, p_c_dyad$healthcare_f)
t.test(p_c_dyad$healthcare_k, p_c_dyad$healthcare_f, var.equal = F)
# abortion
p_c_dyad$abortion_k <- as.numeric(p_c_dyad$stan031_kids)
p_c_dyad$abortion_f <- recode(as.numeric(p_c_dyad$stan031_parents), "1=3;2=1;3=2")
polychor(p_c_dyad$abortion_k, p_c_dyad$abortion_f)
t.test(p_c_dyad$abortion_k, p_c_dyad$abortion_f, var.equal = F)
mean(abs(p_c_dyad$abortion_k - p_c_dyad$abortion_f), na.rm = T)# troops for Isis
p_c_dyad$groundtroops_k <- recode(as.numeric(p_c_dyad$stan032_kids), "1=3;2=1;3=2")
p_c_dyad$groundtroops_f <- recode(as.numeric(p_c_dyad$stan032_parents), "1=3;2=1;3=2")
polychor(p_c_dyad$groundtroops_k, p_c_dyad$groundtroops_f)
t.test(p_c_dyad$groundtroops_k, p_c_dyad$groundtroops_f, var.equal = F)
# marijuana
p_c_dyad$marijuana_k <- as.numeric(p_c_dyad$stan033_kids)
p_c_dyad$marijuana_f <- as.numeric(p_c_dyad$stan033_parents)
polychor(p_c_dyad$marijuana_k, p_c_dyad$marijuana_f)
t.test(p_c_dyad$marijuana_k, p_c_dyad$marijuana_f, var.equal = F)
# gay marriage
p_c_dyad$marriage_k <- as.numeric(p_c_dyad$stan034_kids)
p_c_dyad$marriage_f <- recode(as.numeric(p_c_dyad$stan034_parents), "2=3;3=2")
polychor(p_c_dyad$marriage_f, p_c_dyad$marriage_k)
t.test(p_c_dyad$marriage_k, p_c_dyad$marriage_f, var.equal = F)
# latent
p_c_dyad$latent_ideology_k <- fscores(mirt(p_c_dyad[, c("immigrants_k", "regulation_k", "healthcare_k", 
    "income_k", "abortion_k", "groundtroops_k", "marijuana_k", "marriage_k")], 1, itemtype = "graded"), 
    full.scores = T, scores.only = T)
p_c_dyad$latent_ideology_f <- fscores(mirt(p_c_dyad[, c("immigrants_f", "regulation_f", "healthcare_f", 
    "income_f", "abortion_f", "groundtroops_f", "marijuana_f", "marriage_f")], 1, itemtype = "graded"), 
    full.scores = T, scores.only = T)
cor.test(p_c_dyad$latent_ideology_k, p_c_dyad$Party_k, use = "complete.obs")
############### thermoeter scores Democrat
p_c_dyad$dem_feeling_k <- 100 - (as.numeric(p_c_dyad$stan004_kids) - 1)
p_c_dyad$dem_feeling_f <- 100 - (as.numeric(p_c_dyad$stan004_parents) - 1)
round(cor(p_c_dyad$dem_feeling_k, p_c_dyad$dem_feeling_f, use = "complete.obs"), 2)
p_c_dyad$dem_feeling_ordinal_k <- NA
p_c_dyad$dem_feeling_ordinal_k[p_c_dyad$dem_feeling_k > 80] <- 5
p_c_dyad$dem_feeling_ordinal_k[p_c_dyad$dem_feeling_k <= 80 & p_c_dyad$dem_feeling_k > 60] <- 4
p_c_dyad$dem_feeling_ordinal_k[p_c_dyad$dem_feeling_k <= 60 & p_c_dyad$dem_feeling_k > 40] <- 3
p_c_dyad$dem_feeling_ordinal_k[p_c_dyad$dem_feeling_k <= 40 & p_c_dyad$dem_feeling_k > 20] <- 2
p_c_dyad$dem_feeling_ordinal_k[p_c_dyad$dem_feeling_k <= 20] <- 1
p_c_dyad$dem_feeling_ordinal_f <- NA
p_c_dyad$dem_feeling_ordinal_f[p_c_dyad$dem_feeling_f > 80] <- 5
p_c_dyad$dem_feeling_ordinal_f[p_c_dyad$dem_feeling_f <= 80 & p_c_dyad$dem_feeling_f > 60] <- 4
p_c_dyad$dem_feeling_ordinal_f[p_c_dyad$dem_feeling_f <= 60 & p_c_dyad$dem_feeling_f > 40] <- 3
p_c_dyad$dem_feeling_ordinal_f[p_c_dyad$dem_feeling_f <= 40 & p_c_dyad$dem_feeling_f > 20] <- 2
p_c_dyad$dem_feeling_ordinal_f[p_c_dyad$dem_feeling_f <= 20] <- 1
# Republican
p_c_dyad$rep_feeling_k <- (as.numeric(p_c_dyad$stan005_kids) - 1)
p_c_dyad$rep_feeling_f <- (as.numeric(p_c_dyad$stan005_parents) - 1)
round(cor(p_c_dyad$rep_feeling_k, p_c_dyad$rep_feeling_f, use = "complete.obs"), 2)
p_c_dyad$rep_feeling_ordinal_k <- NA
p_c_dyad$rep_feeling_ordinal_k[p_c_dyad$rep_feeling_k > 80] <- 5
p_c_dyad$rep_feeling_ordinal_k[p_c_dyad$rep_feeling_k <= 80 & p_c_dyad$rep_feeling_k > 60] <- 4
p_c_dyad$rep_feeling_ordinal_k[p_c_dyad$rep_feeling_k <= 60 & p_c_dyad$rep_feeling_k > 40] <- 3
p_c_dyad$rep_feeling_ordinal_k[p_c_dyad$rep_feeling_k <= 40 & p_c_dyad$rep_feeling_k > 20] <- 2
p_c_dyad$rep_feeling_ordinal_k[p_c_dyad$rep_feeling_k <= 20] <- 1
p_c_dyad$rep_feeling_ordinal_f <- NA
p_c_dyad$rep_feeling_ordinal_f[p_c_dyad$rep_feeling_f > 80] <- 5
p_c_dyad$rep_feeling_ordinal_f[p_c_dyad$rep_feeling_f <= 80 & p_c_dyad$rep_feeling_f > 60] <- 4
p_c_dyad$rep_feeling_ordinal_f[p_c_dyad$rep_feeling_f <= 60 & p_c_dyad$rep_feeling_f > 40] <- 3
p_c_dyad$rep_feeling_ordinal_f[p_c_dyad$rep_feeling_f <= 40 & p_c_dyad$rep_feeling_f > 20] <- 2
p_c_dyad$rep_feeling_ordinal_f[p_c_dyad$rep_feeling_f <= 20] <- 1
# Tea party
p_c_dyad$tea_feeling_k <- (as.numeric(p_c_dyad$stan006_kids) - 1)
p_c_dyad$tea_feeling_f <- (as.numeric(p_c_dyad$stan006_parents) - 1)
round(cor(p_c_dyad$tea_feeling_k, p_c_dyad$tea_feeling_f, use = "complete.obs"), 2)
p_c_dyad$tea_feeling_ordinal_k <- NA
p_c_dyad$tea_feeling_ordinal_k[p_c_dyad$tea_feeling_k > 80] <- 5
p_c_dyad$tea_feeling_ordinal_k[p_c_dyad$tea_feeling_k <= 80 & p_c_dyad$tea_feeling_k > 60] <- 4
p_c_dyad$tea_feeling_ordinal_k[p_c_dyad$tea_feeling_k <= 60 & p_c_dyad$tea_feeling_k > 40] <- 3
p_c_dyad$tea_feeling_ordinal_k[p_c_dyad$tea_feeling_k <= 40 & p_c_dyad$tea_feeling_k > 20] <- 2
p_c_dyad$tea_feeling_ordinal_k[p_c_dyad$tea_feeling_k <= 20] <- 1
p_c_dyad$tea_feeling_ordinal_f <- NA
p_c_dyad$tea_feeling_ordinal_f[p_c_dyad$tea_feeling_f > 80] <- 5
p_c_dyad$tea_feeling_ordinal_f[p_c_dyad$tea_feeling_f <= 80 & p_c_dyad$tea_feeling_f > 60] <- 4
p_c_dyad$tea_feeling_ordinal_f[p_c_dyad$tea_feeling_f <= 60 & p_c_dyad$tea_feeling_f > 40] <- 3
p_c_dyad$tea_feeling_ordinal_f[p_c_dyad$tea_feeling_f <= 40 & p_c_dyad$tea_feeling_f > 20] <- 2
p_c_dyad$tea_feeling_ordinal_f[p_c_dyad$tea_feeling_f <= 20] <- 1
# Obama
p_c_dyad$obama_feeling_k <- 100 - (as.numeric(p_c_dyad$stan007_kids) - 1)
p_c_dyad$obama_feeling_f <- 100 - (as.numeric(p_c_dyad$stan007_parents) - 1)
round(cor(p_c_dyad$obama_feeling_k, p_c_dyad$obama_feeling_f, use = "complete.obs"), 2)
p_c_dyad$obama_feeling_ordinal_k <- NA
p_c_dyad$obama_feeling_ordinal_k[p_c_dyad$obama_feeling_k > 80] <- 5
p_c_dyad$obama_feeling_ordinal_k[p_c_dyad$obama_feeling_k <= 80 & p_c_dyad$obama_feeling_k > 60] <- 4
p_c_dyad$obama_feeling_ordinal_k[p_c_dyad$obama_feeling_k <= 60 & p_c_dyad$obama_feeling_k > 40] <- 3
p_c_dyad$obama_feeling_ordinal_k[p_c_dyad$obama_feeling_k <= 40 & p_c_dyad$obama_feeling_k > 20] <- 2
p_c_dyad$obama_feeling_ordinal_k[p_c_dyad$obama_feeling_k <= 20] <- 1
p_c_dyad$obama_feeling_ordinal_f <- NA
p_c_dyad$obama_feeling_ordinal_f[p_c_dyad$obama_feeling_f > 80] <- 5
p_c_dyad$obama_feeling_ordinal_f[p_c_dyad$obama_feeling_f <= 80 & p_c_dyad$obama_feeling_f > 60] <- 4
p_c_dyad$obama_feeling_ordinal_f[p_c_dyad$obama_feeling_f <= 60 & p_c_dyad$obama_feeling_f > 40] <- 3
p_c_dyad$obama_feeling_ordinal_f[p_c_dyad$obama_feeling_f <= 40 & p_c_dyad$obama_feeling_f > 20] <- 2
p_c_dyad$obama_feeling_ordinal_f[p_c_dyad$obama_feeling_f <= 20] <- 1
# Clinton
p_c_dyad$clinton_feeling_k <- 100 - (as.numeric(p_c_dyad$stan008_kids) - 1)
p_c_dyad$clinton_feeling_f <- 100 - (as.numeric(p_c_dyad$stan008_parents) - 1)
round(cor(p_c_dyad$clinton_feeling_f, p_c_dyad$clinton_feeling_k, use = "complete.obs"), 2)
p_c_dyad$clinton_feeling_ordinal_k <- NA
p_c_dyad$clinton_feeling_ordinal_k[p_c_dyad$clinton_feeling_k > 80] <- 5
p_c_dyad$clinton_feeling_ordinal_k[p_c_dyad$clinton_feeling_k <= 80 & p_c_dyad$clinton_feeling_k > 60] <- 4
p_c_dyad$clinton_feeling_ordinal_k[p_c_dyad$clinton_feeling_k <= 60 & p_c_dyad$clinton_feeling_k > 40] <- 3
p_c_dyad$clinton_feeling_ordinal_k[p_c_dyad$clinton_feeling_k <= 40 & p_c_dyad$clinton_feeling_k > 20] <- 2
p_c_dyad$clinton_feeling_ordinal_k[p_c_dyad$clinton_feeling_k <= 20] <- 1
p_c_dyad$clinton_feeling_ordinal_f <- NA
p_c_dyad$clinton_feeling_ordinal_f[p_c_dyad$clinton_feeling_f > 80] <- 5
p_c_dyad$clinton_feeling_ordinal_f[p_c_dyad$clinton_feeling_f <= 80 & p_c_dyad$clinton_feeling_f > 60] <- 4
p_c_dyad$clinton_feeling_ordinal_f[p_c_dyad$clinton_feeling_f <= 60 & p_c_dyad$clinton_feeling_f > 40] <- 3
p_c_dyad$clinton_feeling_ordinal_f[p_c_dyad$clinton_feeling_f <= 40 & p_c_dyad$clinton_feeling_f > 20] <- 2
p_c_dyad$clinton_feeling_ordinal_f[p_c_dyad$clinton_feeling_f <= 20] <- 1
# Paul
p_c_dyad$paul_feeling_k <- (as.numeric(p_c_dyad$stan009_kids) - 1)
p_c_dyad$paul_feeling_f <- (as.numeric(p_c_dyad$stan009_parents) - 1)
round(cor(p_c_dyad$paul_feeling_k, p_c_dyad$paul_feeling_f, use = "complete.obs"), 2)
p_c_dyad$paul_feeling_ordinal_k <- NA
p_c_dyad$paul_feeling_ordinal_k[p_c_dyad$paul_feeling_k > 80] <- 5
p_c_dyad$paul_feeling_ordinal_k[p_c_dyad$paul_feeling_k <= 80 & p_c_dyad$paul_feeling_k > 60] <- 4
p_c_dyad$paul_feeling_ordinal_k[p_c_dyad$paul_feeling_k <= 60 & p_c_dyad$paul_feeling_k > 40] <- 3
p_c_dyad$paul_feeling_ordinal_k[p_c_dyad$paul_feeling_k <= 40 & p_c_dyad$paul_feeling_k > 20] <- 2
p_c_dyad$paul_feeling_ordinal_k[p_c_dyad$paul_feeling_k <= 20] <- 1
p_c_dyad$paul_feeling_ordinal_f <- NA
p_c_dyad$paul_feeling_ordinal_f[p_c_dyad$paul_feeling_f > 80] <- 5
p_c_dyad$paul_feeling_ordinal_f[p_c_dyad$paul_feeling_f <= 80 & p_c_dyad$paul_feeling_f > 60] <- 4
p_c_dyad$paul_feeling_ordinal_f[p_c_dyad$paul_feeling_f <= 60 & p_c_dyad$paul_feeling_f > 40] <- 3
p_c_dyad$paul_feeling_ordinal_f[p_c_dyad$paul_feeling_f <= 40 & p_c_dyad$paul_feeling_f > 20] <- 2
p_c_dyad$paul_feeling_ordinal_f[p_c_dyad$paul_feeling_f <= 20] <- 1
# Bush
p_c_dyad$bush_feeling_k <- (as.numeric(p_c_dyad$stan010_kids) - 1)
p_c_dyad$bush_feeling_f <- (as.numeric(p_c_dyad$stan010_parents) - 1)
round(cor(p_c_dyad$bush_feeling_k, p_c_dyad$bush_feeling_f, use = "complete.obs"), 2)
p_c_dyad$bush_feeling_ordinal_k <- NA
p_c_dyad$bush_feeling_ordinal_k[p_c_dyad$bush_feeling_k > 80] <- 5
p_c_dyad$bush_feeling_ordinal_k[p_c_dyad$bush_feeling_k <= 80 & p_c_dyad$bush_feeling_k > 60] <- 4
p_c_dyad$bush_feeling_ordinal_k[p_c_dyad$bush_feeling_k <= 60 & p_c_dyad$bush_feeling_k > 40] <- 3
p_c_dyad$bush_feeling_ordinal_k[p_c_dyad$bush_feeling_k <= 40 & p_c_dyad$bush_feeling_k > 20] <- 2
p_c_dyad$bush_feeling_ordinal_k[p_c_dyad$bush_feeling_k <= 20] <- 1
p_c_dyad$bush_feeling_ordinal_f <- NA
p_c_dyad$bush_feeling_ordinal_f[p_c_dyad$bush_feeling_f > 80] <- 5
p_c_dyad$bush_feeling_ordinal_f[p_c_dyad$bush_feeling_f <= 80 & p_c_dyad$bush_feeling_f > 60] <- 4
p_c_dyad$bush_feeling_ordinal_f[p_c_dyad$bush_feeling_f <= 60 & p_c_dyad$bush_feeling_f > 40] <- 3
p_c_dyad$bush_feeling_ordinal_f[p_c_dyad$bush_feeling_f <= 40 & p_c_dyad$bush_feeling_f > 20] <- 2
p_c_dyad$bush_feeling_ordinal_f[p_c_dyad$bush_feeling_f <= 20] <- 1
# Cruz
p_c_dyad$cruz_feeling_k <- (as.numeric(p_c_dyad$stan011_kids) - 1)
p_c_dyad$cruz_feeling_f <- (as.numeric(p_c_dyad$stan011_parents) - 1)
round(cor(p_c_dyad$cruz_feeling_k, p_c_dyad$cruz_feeling_f, use = "complete.obs"), 2)
p_c_dyad$cruz_feeling_ordinal_k <- NA
p_c_dyad$cruz_feeling_ordinal_k[p_c_dyad$cruz_feeling_k > 80] <- 5
p_c_dyad$cruz_feeling_ordinal_k[p_c_dyad$cruz_feeling_k <= 80 & p_c_dyad$cruz_feeling_k > 60] <- 4
p_c_dyad$cruz_feeling_ordinal_k[p_c_dyad$cruz_feeling_k <= 60 & p_c_dyad$cruz_feeling_k > 40] <- 3
p_c_dyad$cruz_feeling_ordinal_k[p_c_dyad$cruz_feeling_k <= 40 & p_c_dyad$cruz_feeling_k > 20] <- 2
p_c_dyad$cruz_feeling_ordinal_k[p_c_dyad$cruz_feeling_k <= 20] <- 1
p_c_dyad$cruz_feeling_ordinal_f <- NA
p_c_dyad$cruz_feeling_ordinal_f[p_c_dyad$cruz_feeling_f > 80] <- 5
p_c_dyad$cruz_feeling_ordinal_f[p_c_dyad$cruz_feeling_f <= 80 & p_c_dyad$cruz_feeling_f > 60] <- 4
p_c_dyad$cruz_feeling_ordinal_f[p_c_dyad$cruz_feeling_f <= 60 & p_c_dyad$cruz_feeling_f > 40] <- 3
p_c_dyad$cruz_feeling_ordinal_f[p_c_dyad$cruz_feeling_f <= 40 & p_c_dyad$cruz_feeling_f > 20] <- 2
p_c_dyad$cruz_feeling_ordinal_f[p_c_dyad$cruz_feeling_f <= 20] <- 1
# Trump
p_c_dyad$trump_feeling_k <- (as.numeric(p_c_dyad$stan011b_kids) - 1)
p_c_dyad$trump_feeling_f <- (as.numeric(p_c_dyad$stan011b_parents) - 1)
round(cor(p_c_dyad$trump_feeling_k, p_c_dyad$trump_feeling_f, use = "complete.obs"), 2)
p_c_dyad$trump_feeling_ordinal_k <- NA
p_c_dyad$trump_feeling_ordinal_k[p_c_dyad$trump_feeling_k > 80] <- 5
p_c_dyad$trump_feeling_ordinal_k[p_c_dyad$trump_feeling_k <= 80 & p_c_dyad$trump_feeling_k > 60] <- 4
p_c_dyad$trump_feeling_ordinal_k[p_c_dyad$trump_feeling_k <= 60 & p_c_dyad$trump_feeling_k > 40] <- 3
p_c_dyad$trump_feeling_ordinal_k[p_c_dyad$trump_feeling_k <= 40 & p_c_dyad$trump_feeling_k > 20] <- 2
p_c_dyad$trump_feeling_ordinal_k[p_c_dyad$trump_feeling_k <= 20] <- 1
p_c_dyad$trump_feeling_ordinal_f <- NA
p_c_dyad$trump_feeling_ordinal_f[p_c_dyad$trump_feeling_f > 80] <- 5
p_c_dyad$trump_feeling_ordinal_f[p_c_dyad$trump_feeling_f <= 80 & p_c_dyad$trump_feeling_f > 60] <- 4
p_c_dyad$trump_feeling_ordinal_f[p_c_dyad$trump_feeling_f <= 60 & p_c_dyad$trump_feeling_f > 40] <- 3
p_c_dyad$trump_feeling_ordinal_f[p_c_dyad$trump_feeling_f <= 40 & p_c_dyad$trump_feeling_f > 20] <- 2
p_c_dyad$trump_feeling_ordinal_f[p_c_dyad$trump_feeling_f <= 20] <- 1
# NRA
p_c_dyad$nra_feeling_k <- (as.numeric(p_c_dyad$stan012_kids) - 1)
p_c_dyad$nra_feeling_f <- (as.numeric(p_c_dyad$stan012_parents) - 1)
round(cor(p_c_dyad$nra_feeling_k, p_c_dyad$nra_feeling_f, use = "complete.obs"), 2)
p_c_dyad$nra_feeling_ordinal_k <- NA
p_c_dyad$nra_feeling_ordinal_k[p_c_dyad$nra_feeling_k > 80] <- 5
p_c_dyad$nra_feeling_ordinal_k[p_c_dyad$nra_feeling_k <= 80 & p_c_dyad$nra_feeling_k > 60] <- 4
p_c_dyad$nra_feeling_ordinal_k[p_c_dyad$nra_feeling_k <= 60 & p_c_dyad$nra_feeling_k > 40] <- 3
p_c_dyad$nra_feeling_ordinal_k[p_c_dyad$nra_feeling_k <= 40 & p_c_dyad$nra_feeling_k > 20] <- 2
p_c_dyad$nra_feeling_ordinal_k[p_c_dyad$nra_feeling_k <= 20] <- 1
p_c_dyad$nra_feeling_ordinal_f <- NA
p_c_dyad$nra_feeling_ordinal_f[p_c_dyad$nra_feeling_f > 80] <- 5
p_c_dyad$nra_feeling_ordinal_f[p_c_dyad$nra_feeling_f <= 80 & p_c_dyad$nra_feeling_f > 60] <- 4
p_c_dyad$nra_feeling_ordinal_f[p_c_dyad$nra_feeling_f <= 60 & p_c_dyad$nra_feeling_f > 40] <- 3
p_c_dyad$nra_feeling_ordinal_f[p_c_dyad$nra_feeling_f <= 40 & p_c_dyad$nra_feeling_f > 20] <- 2
p_c_dyad$nra_feeling_ordinal_f[p_c_dyad$nra_feeling_f <= 20] <- 1
# gay
p_c_dyad$gay_feeling_k <- 100 - (as.numeric(p_c_dyad$stan013_kids) - 1)
p_c_dyad$gay_feeling_f <- 100 - (as.numeric(p_c_dyad$stan013_parents) - 1)
round(cor(p_c_dyad$gay_feeling_k, p_c_dyad$gay_feeling_f, use = "complete.obs"), 2)
p_c_dyad$gay_feeling_ordinal_k <- NA
p_c_dyad$gay_feeling_ordinal_k[p_c_dyad$gay_feeling_k > 80] <- 5
p_c_dyad$gay_feeling_ordinal_k[p_c_dyad$gay_feeling_k <= 80 & p_c_dyad$gay_feeling_k > 60] <- 4
p_c_dyad$gay_feeling_ordinal_k[p_c_dyad$gay_feeling_k <= 60 & p_c_dyad$gay_feeling_k > 40] <- 3
p_c_dyad$gay_feeling_ordinal_k[p_c_dyad$gay_feeling_k <= 40 & p_c_dyad$gay_feeling_k > 20] <- 2
p_c_dyad$gay_feeling_ordinal_k[p_c_dyad$gay_feeling_k <= 20] <- 1
p_c_dyad$gay_feeling_ordinal_f <- NA
p_c_dyad$gay_feeling_ordinal_f[p_c_dyad$gay_feeling_f > 80] <- 5
p_c_dyad$gay_feeling_ordinal_f[p_c_dyad$gay_feeling_f <= 80 & p_c_dyad$gay_feeling_f > 60] <- 4
p_c_dyad$gay_feeling_ordinal_f[p_c_dyad$gay_feeling_f <= 60 & p_c_dyad$gay_feeling_f > 40] <- 3
p_c_dyad$gay_feeling_ordinal_f[p_c_dyad$gay_feeling_f <= 40 & p_c_dyad$gay_feeling_f > 20] <- 2
p_c_dyad$gay_feeling_ordinal_f[p_c_dyad$gay_feeling_f <= 20] <- 1
# Christians
p_c_dyad$christians_feeling_k <- (as.numeric(p_c_dyad$stan014_kids) - 1)
p_c_dyad$christians_feeling_f <- (as.numeric(p_c_dyad$stan014_parents) - 1)
round(cor(p_c_dyad$christians_feeling_k, p_c_dyad$christians_feeling_f, use = "complete.obs"), 2)
p_c_dyad$christians_feeling_ordinal_k <- NA
p_c_dyad$christians_feeling_ordinal_k[p_c_dyad$christians_feeling_k > 80] <- 5
p_c_dyad$christians_feeling_ordinal_k[p_c_dyad$christians_feeling_k <= 80 & p_c_dyad$christians_feeling_k > 
    60] <- 4
p_c_dyad$christians_feeling_ordinal_k[p_c_dyad$christians_feeling_k <= 60 & p_c_dyad$christians_feeling_k > 
    40] <- 3
p_c_dyad$christians_feeling_ordinal_k[p_c_dyad$christians_feeling_k <= 40 & p_c_dyad$christians_feeling_k > 
    20] <- 2
p_c_dyad$christians_feeling_ordinal_k[p_c_dyad$christians_feeling_k <= 20] <- 1
p_c_dyad$christians_feeling_ordinal_f <- NA
p_c_dyad$christians_feeling_ordinal_f[p_c_dyad$christians_feeling_f > 80] <- 5
p_c_dyad$christians_feeling_ordinal_f[p_c_dyad$christians_feeling_f <= 80 & p_c_dyad$christians_feeling_f > 
    60] <- 4
p_c_dyad$christians_feeling_ordinal_f[p_c_dyad$christians_feeling_f <= 60 & p_c_dyad$christians_feeling_f > 
    40] <- 3
p_c_dyad$christians_feeling_ordinal_f[p_c_dyad$christians_feeling_f <= 40 & p_c_dyad$christians_feeling_f > 
    20] <- 2
p_c_dyad$christians_feeling_ordinal_f[p_c_dyad$christians_feeling_f <= 20] <- 1
# Atheists
p_c_dyad$atheists_feeling_k <- 100 - (as.numeric(p_c_dyad$stan015_kids) - 1)
p_c_dyad$atheists_feeling_f <- 100 - (as.numeric(p_c_dyad$stan015_parents) - 1)
round(cor(p_c_dyad$atheists_feeling_k, p_c_dyad$atheists_feeling_f, use = "complete.obs"), 2)
p_c_dyad$atheists_feeling_ordinal_k <- NA
p_c_dyad$atheists_feeling_ordinal_k[p_c_dyad$atheists_feeling_k > 80] <- 5
p_c_dyad$atheists_feeling_ordinal_k[p_c_dyad$atheists_feeling_k <= 80 & p_c_dyad$atheists_feeling_k > 
    60] <- 4
p_c_dyad$atheists_feeling_ordinal_k[p_c_dyad$atheists_feeling_k <= 60 & p_c_dyad$atheists_feeling_k > 
    40] <- 3
p_c_dyad$atheists_feeling_ordinal_k[p_c_dyad$atheists_feeling_k <= 40 & p_c_dyad$atheists_feeling_k > 
    20] <- 2
p_c_dyad$atheists_feeling_ordinal_k[p_c_dyad$atheists_feeling_k <= 20] <- 1
p_c_dyad$atheists_feeling_ordinal_f <- NA
p_c_dyad$atheists_feeling_ordinal_f[p_c_dyad$atheists_feeling_f > 80] <- 5
p_c_dyad$atheists_feeling_ordinal_f[p_c_dyad$atheists_feeling_f <= 80 & p_c_dyad$atheists_feeling_f > 
    60] <- 4
p_c_dyad$atheists_feeling_ordinal_f[p_c_dyad$atheists_feeling_f <= 60 & p_c_dyad$atheists_feeling_f > 
    40] <- 3
p_c_dyad$atheists_feeling_ordinal_f[p_c_dyad$atheists_feeling_f <= 40 & p_c_dyad$atheists_feeling_f > 
    20] <- 2
p_c_dyad$atheists_feeling_ordinal_f[p_c_dyad$atheists_feeling_f <= 20] <- 1
p_c_dyad$latent_partisan_attitudes_k <- with(p_c_dyad, fscores(mirt(cbind(Ideology_k, Party_k, tea_feeling_ordinal_k, 
    nra_feeling_ordinal_k, gay_feeling_ordinal_k, christians_feeling_ordinal_k, atheists_feeling_ordinal_k, 
    dem_feeling_ordinal_k, rep_feeling_ordinal_k, obama_feeling_ordinal_k, clinton_feeling_ordinal_k, 
    paul_feeling_ordinal_k, bush_feeling_ordinal_k, cruz_feeling_ordinal_k, trump_feeling_ordinal_k), 
    1, itemtype = "graded"), full.scores = TRUE, scores.only = TRUE))
p_c_dyad$latent_partisan_attitudes_f <- with(p_c_dyad, fscores(mirt(cbind(Ideology_f, Party_f, tea_feeling_ordinal_f, 
    nra_feeling_ordinal_f, gay_feeling_ordinal_f, christians_feeling_ordinal_f, atheists_feeling_ordinal_f, 
    dem_feeling_ordinal_f, rep_feeling_ordinal_f, obama_feeling_ordinal_f, clinton_feeling_ordinal_f, 
    paul_feeling_ordinal_f, bush_feeling_ordinal_f, cruz_feeling_ordinal_f, trump_feeling_ordinal_f), 
    1, itemtype = "graded"), full.scores = TRUE, scores.only = TRUE))
cor.test(p_c_dyad$latent_partisan_attitudes_f, p_c_dyad$latent_partisan_attitudes_k)
# include party, ideology non-policy domains
#### Personality of Children Respect for elders/independence
p_c_dyad$independence_kids <- as.numeric(p_c_dyad$stan020_kids)
p_c_dyad$independence_parents <- as.numeric(p_c_dyad$stan020_parents)
polychor(p_c_dyad$independence_parents, p_c_dyad$independence_kids)
# obedience/self-reliance
p_c_dyad$obedience_kids <- recode(as.numeric(p_c_dyad$stan021_kids), "1=2;2=1")
p_c_dyad$obedience_parents <- recode(as.numeric(p_c_dyad$stan021_parents), "1=2;2=1")
crosstab(p_c_dyad$obedience_kids, p_c_dyad$obedience_parents)
polychor(p_c_dyad$obedience_kids, p_c_dyad$obedience_parents)
# good manners/curiosity
p_c_dyad$manners_kids <- as.numeric(p_c_dyad$stan022_kids)
p_c_dyad$manners_parents <- as.numeric(p_c_dyad$stan022_parents)
polychor(p_c_dyad$manners_kids, p_c_dyad$manners_parents)
# considerate vs. well-behaved
p_c_dyad$behaved_kids <- recode(as.numeric(p_c_dyad$stan023_kids), "1=2;2=1")
p_c_dyad$behaved_parents <- recode(as.numeric(p_c_dyad$stan023_parents), "1=2;2=1")
crosstab(p_c_dyad$behaved_kids, p_c_dyad$behaved_parents)
polychor(p_c_dyad$behaved_kids, p_c_dyad$behaved_parents)
# latent
p_c_dyad$latent_personality_kids <- with(p_c_dyad, fscores(mirt(cbind(independence_kids, obedience_kids, 
    manners_kids, behaved_kids), 1, itemtype = "graded"), full.scores = T, scores.only = T))
p_c_dyad$latent_personality_parents <- with(p_c_dyad, fscores(mirt(cbind(independence_parents, obedience_parents, 
    manners_parents, behaved_parents), 1, itemtype = "graded"), full.scores = T, scores.only = T))
cor.test(p_c_dyad$latent_personality_kids, p_c_dyad$latent_personality_parents)
####################################### Affect
p_c_dyad$marriage_dem_f <- with(p_c_dyad, as.numeric(stan037_parents))
p_c_dyad$marriage_dem_k <- with(p_c_dyad, as.numeric(stan037_kids))
polychor(p_c_dyad$marriage_dem_f, p_c_dyad$marriage_dem_k)  #0.61 correlation betw
p_c_dyad$marriage_rep_f <- with(p_c_dyad, recode(as.numeric(stan038_parents), "1=3;2=2;3=1"))
p_c_dyad$marriage_rep_k <- with(p_c_dyad, recode(as.numeric(stan038_kids), "1=3;2=2;3=1"))
polychor(p_c_dyad$marriage_rep_f, p_c_dyad$marriage_rep_k)  #0.48 correlation betw
p_c_dyad$inter_marriage_k <- ifelse(p_c_dyad$Party_k < 4, p_c_dyad$marriage_rep_k, ifelse(p_c_dyad$Party_k > 
    4, p_c_dyad$marriage_dem_k, NA))
p_c_dyad$inter_marriage_f <- ifelse(p_c_dyad$Party_f < 4, p_c_dyad$marriage_rep_f, ifelse(p_c_dyad$Party_f > 
    4, p_c_dyad$marriage_dem_f, NA))
p_c_dyad$stereotype_patriotic_democrat_f <- with(p_c_dyad, recode(as.numeric(stan046_parents), "1=1;2=2;3=4;4=5;5=3"))
p_c_dyad$stereotype_patriotic_democrat_k <- with(p_c_dyad, recode(as.numeric(stan046_kids), "1=1;2=2;3=4;4=5;5=3"))
polychor(p_c_dyad$stereotype_patriotic_democrat_f, p_c_dyad$stereotype_patriotic_democrat_k)  #0.72 correlation betw
p_c_dyad$stereotype_selfish_democrat_f <- with(p_c_dyad, recode(as.numeric(stan047_parents), "1=5;2=4;3=2;4=1;5=3"))
p_c_dyad$stereotype_selfish_democrat_k <- with(p_c_dyad, recode(as.numeric(stan047_kids), "1=5;2=4;3=2;4=1;5=3"))
polychor(p_c_dyad$stereotype_selfish_democrat_f, p_c_dyad$stereotype_selfish_democrat_k)  #0.64 correlation betw
p_c_dyad$stereotype_compromise_democrat_f <- with(p_c_dyad, recode(as.numeric(stan048_parents), "1=1;2=2;3=4;4=5;5=3"))
p_c_dyad$stereotype_compromise_democrat_k <- with(p_c_dyad, recode(as.numeric(stan048_kids), "1=1;2=2;3=4;4=5;5=3"))
polychor(p_c_dyad$stereotype_compromise_democrat_f, p_c_dyad$stereotype_compromise_democrat_k)  #0.76 correlation betw
p_c_dyad$stereotype_compassionate_democrat_f <- with(p_c_dyad, recode(as.numeric(stan049_parents), "1=1;2=2;3=4;4=5;5=3"))
p_c_dyad$stereotype_compassionate_democrat_k <- with(p_c_dyad, recode(as.numeric(stan049_kids), "1=1;2=2;3=4;4=5;5=3"))
polychor(p_c_dyad$stereotype_compassionate_democrat_f, p_c_dyad$stereotype_compassionate_democrat_k)  #0.72 correlation betw
p_c_dyad$stereotype_patriotic_republican_f <- with(p_c_dyad, recode(as.numeric(stan052_parents), "1=5;2=4;3=2;4=1;5=3"))
p_c_dyad$stereotype_patriotic_republican_k <- with(p_c_dyad, recode(as.numeric(stan052_kids), "1=5;2=4;3=2;4=1;5=3"))
polychor(p_c_dyad$stereotype_patriotic_republican_f, p_c_dyad$stereotype_patriotic_republican_k)  #0.52 correlation betw
p_c_dyad$stereotype_selfish_republican_f <- with(p_c_dyad, recode(as.numeric(stan053_parents), "1=1;2=2;3=4;4=5;5=3"))
p_c_dyad$stereotype_selfish_republican_k <- with(p_c_dyad, recode(as.numeric(stan053_kids), "1=1;2=2;3=4;4=5;5=3"))
polychor(p_c_dyad$stereotype_selfish_republican_f, p_c_dyad$stereotype_selfish_republican_k)  #0.65 correlation betw
p_c_dyad$inter_stereotype_k <- ifelse(p_c_dyad$Party_k < 4, p_c_dyad$stereotype_selfish_republican_k, 
    ifelse(p_c_dyad$Party_k > 4, p_c_dyad$stereotype_selfish_democrat_k, NA))
p_c_dyad$inter_stereotype_f <- ifelse(p_c_dyad$Party_f < 4, p_c_dyad$stereotype_selfish_republican_f, 
    ifelse(p_c_dyad$Party_f > 4, p_c_dyad$stereotype_selfish_democrat_f, NA))
p_c_dyad$stereotype_compromise_republican_f <- with(p_c_dyad, recode(as.numeric(stan054_parents), "1=5;2=4;3=2;4=1;5=3"))
p_c_dyad$stereotype_compromise_republican_k <- with(p_c_dyad, recode(as.numeric(stan054_kids), "1=5;2=4;3=2;4=1;5=3"))
polychor(p_c_dyad$stereotype_compromise_republican_f, p_c_dyad$stereotype_compromise_republican_k)  #0.64 correlation betw
p_c_dyad$stereotype_compassionate_republican_f <- with(p_c_dyad, recode(as.numeric(stan055_parents), 
    "1=5;2=4;3=2;4=1;5=3"))
p_c_dyad$stereotype_compassionate_republican_k <- with(p_c_dyad, recode(as.numeric(stan055_kids), "1=5;2=4;3=2;4=1;5=3"))
polychor(p_c_dyad$stereotype_compassionate_republican_f, p_c_dyad$stereotype_compassionate_republican_k)  #0.64 correlation betw
p_c_dyad$latent_stereotyping_f <- fscores(mirt(p_c_dyad[, c("marriage_dem_f", "marriage_rep_f", "stereotype_patriotic_democrat_f", 
    "stereotype_selfish_democrat_f", "stereotype_compassionate_democrat_f", "stereotype_patriotic_republican_f", 
    "stereotype_selfish_republican_f", "stereotype_compromise_republican_f", "stereotype_compassionate_republican_f")], 
    1), full.scores = T, scores.only = T)
p_c_dyad$latent_stereotyping_k <- fscores(mirt(p_c_dyad[, c("marriage_dem_k", "marriage_rep_k", "stereotype_patriotic_democrat_k", 
    "stereotype_selfish_democrat_k", "stereotype_compassionate_democrat_k", "stereotype_patriotic_republican_k", 
    "stereotype_selfish_republican_k", "stereotype_compromise_republican_k", "stereotype_compassionate_republican_k")], 
    1), full.scores = T, scores.only = T)
cor.test(p_c_dyad$latent_stereotyping_f, p_c_dyad$latent_stereotyping_k)
# religious service attendance
polychor(recode(as.numeric(p_c_dyad$stan096_kids), "1=5;2=4;3=3;4=2;5=1"), recode(as.numeric(p_c_dyad$stan096_parents), 
    "1=5;2=4;3=3;4=2;5=1"))
polychor(recode(as.numeric(p_c_dyad$stan097_kids), "1=5;2=4;3=3;4=2;5=1"), recode(as.numeric(p_c_dyad$stan097_parents), 
    "1=5;2=4;3=3;4=2;5=1"))
p_c_dyad$religious_attendance_kids <- fscores(mirt(cbind(recode(as.numeric(p_c_dyad$stan096_kids), "1=5;2=4;3=3;4=2;5=1"), 
    recode(as.numeric(p_c_dyad$stan097_kids), "1=5;2=4;3=3;4=2;5=1"), p_c_dyad$christians_feeling_ordinal_kids, 
    p_c_dyad$atheists_feeling_ordinal_kids), 1, itemtype = "graded"), full.scores = TRUE, scores.only = TRUE)
p_c_dyad$religious_attendance_parents <- fscores(mirt(cbind(recode(as.numeric(p_c_dyad$stan096_parents), 
    "1=5;2=4;3=3;4=2;5=1"), recode(as.numeric(p_c_dyad$stan097_parents), "1=5;2=4;3=3;4=2;5=1"), p_c_dyad$christians_feeling_ordinal_parents, 
    p_c_dyad$atheists_feeling_ordinal_parents), 1, itemtype = "graded"), full.scores = TRUE, scores.only = TRUE)
cor.test(p_c_dyad$religious_attendance_parents, p_c_dyad$religious_attendance_kids)
# latent 0.52
