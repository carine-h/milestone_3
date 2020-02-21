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
recode <- car::recode
library(apsrtable)
########################################## read in data##############################
set.seed(2141)
# length of marriage
#couples = as.data.set(spss.system.file("STAN0078_COUPLES_OUTPUT2.sav"))
couples$stan099_p2 <- as.numeric(couples$stan099_p2)
couples$stan099_p1 <- as.numeric(couples$stan099_p1)
couples$birthyr_p1 <- as.numeric(couples$birthyr_p1)
couples$birthyr_p2 <- as.numeric(couples$birthyr_p2)
################## match in interchangeable dayd organization
draws = sample(rep(c("p1", "p2"), nrow(couples)), nrow(couples), replace = T)
for (i in 1:nrow(couples)) {
  if (draws[i] == "p1") 
    exchangeable_dyad_row_1 = couples[i, c(1, grep("p1", colnames(couples)))] else (exchangeable_dyad_row_1 = couples[i, c(1:12, grep("p2", colnames(couples)))])
  if (draws[i] == "p1") 
    exchangeable_dyad_row_2 = couples[i, c(1:12, grep("p2", colnames(couples)))] else (exchangeable_dyad_row_2 = couples[i, c(1, grep("p1", colnames(couples)))])
  colnames(exchangeable_dyad_row_1) <- gsub("p2", "p1", colnames(exchangeable_dyad_row_1))
  colnames(exchangeable_dyad_row_2) <- gsub("p1", "p2", colnames(exchangeable_dyad_row_2))
  colnames(exchangeable_dyad_row_1)[1] <- "caseid_p1"
  colnames(exchangeable_dyad_row_2)[1] <- "caseid_p2"
  if (i == 1) 
    exchangeable_dyad_row_p1 = exchangeable_dyad_row_1 else (exchangeable_dyad_row_p1 = rbind(exchangeable_dyad_row_p1, exchangeable_dyad_row_1))
  if (i == 1) 
    exchangeable_dyad_row_p2 = exchangeable_dyad_row_2 else (exchangeable_dyad_row_p2 = rbind(exchangeable_dyad_row_p2, exchangeable_dyad_row_2))
print(i)
  }
exchangeable_dyad <- merge(exchangeable_dyad_row_p1, exchangeable_dyad_row_p2, by.x = "caseid_p1", by.y = "caseid_p2")
# trhow out homosexual pairs
exchangeable_dyad = exchangeable_dyad[(exchangeable_dyad$gender_p1 == "Female" & exchangeable_dyad$gender_p2 == 
                                         "Male") | (exchangeable_dyad$gender_p2 == "Female" & exchangeable_dyad$gender_p1 == "Male"), ]
exchangeable_dyad = data.frame(exchangeable_dyad)
# checks
which(exchangeable_dyad$caseid_p1 != exchangeable_dyad$caseid_p2)
which(exchangeable_dyad$gender_p1 == exchangeable_dyad$gender_p2)
summary(exchangeable_dyad$gender_p1)
summary(exchangeable_dyad$gender_p2)
###### 
exchangeable_dyad$marriage_length_p1 <- 2015 - exchangeable_dyad$stan099_p1
exchangeable_dyad$marriage_length_p2 <- 2015 - exchangeable_dyad$stan099_p2
# education
exchangeable_dyad$education_p1 <- as.numeric(exchangeable_dyad$educ_p1)
exchangeable_dyad$education_p2 <- as.numeric(exchangeable_dyad$educ_p2)
exchangeable_dyad$education_overall <- exchangeable_dyad$education_p2
# exchangeable_dyad=exchangeable_dyad[exchangeable_dyad$pid7_p2!='Not
# sure'&exchangeable_dyad$pid7_p1!='Not
# sure'&!is.na(exchangeable_dyad$pid7_p2)&!is.na(exchangeable_dyad$pid7_p2),]
# exchangeable_dyad$pid7_p2<-factor(exchangeable_dyad$pid7_p2);exchangeable_dyad$pid7_p1<-factor(exchangeable_dyad$pid7_p1)
exchangeable_dyad$Party_p1 <- recode(as.numeric(exchangeable_dyad$pid7_p1), "8=NA")
exchangeable_dyad$Party_p2 <- recode(as.numeric(exchangeable_dyad$pid7_p2), "8=NA")
exchangeable_dyad$Party_p1_3 <- recode(exchangeable_dyad$pid3_p1, "'Other'=NA;'No preference'=NA")
exchangeable_dyad$Party_p2_3 <- recode(exchangeable_dyad$pid3_p2, "'Other'=NA;'No preference'=NA")
# get ci
polychor(exchangeable_dyad$Party_p1, exchangeable_dyad$Party_p2)
polychor(exchangeable_dyad$Party_p1_3, exchangeable_dyad$Party_p2_3)

exchangeable_dyad$Ideology_p1 <- recode(as.numeric(exchangeable_dyad$stan019_p1), "8=NA")
exchangeable_dyad$Ideology_p2 <- recode(as.numeric(exchangeable_dyad$stan019_p2), "8=NA")
polychor(exchangeable_dyad$Ideology_p1, exchangeable_dyad$Ideology_p2)

#### Interest in non-political things
# homogeneity in interest for creative things
mean_creative_1 = polychor(exchangeable_dyad$stan061_p1, exchangeable_dyad$stan061_p2)
# homogeneity in interest in volounteering
mean_creative_2 = polychor(exchangeable_dyad$stan065_p1, exchangeable_dyad$stan065_p2)
# homogeneity in interest in learning new skills
mean_creative_3 = polychor(exchangeable_dyad$stan068_p1, exchangeable_dyad$stan068_p2)
# homogeneity in interest in outdoor activities
mean_creative_4 = polychor(exchangeable_dyad$stan070_p1, exchangeable_dyad$stan070_p2)
mean_creative = mean(c(mean_creative_1, mean_creative_2, mean_creative_3, mean_creative_4))
#### Personality of Children Respect for elders/independence
exchangeable_dyad$independence_p1 <- as.numeric(exchangeable_dyad$stan020_p1)
exchangeable_dyad$independence_p2 <- as.numeric(exchangeable_dyad$stan020_p2)
mean_authoritarianism_1 = polychor(exchangeable_dyad$independence_p2, exchangeable_dyad$independence_p1)
# obedience/self-reliance
exchangeable_dyad$obedience_p1 <- recode(as.numeric(exchangeable_dyad$stan021_p1), "1=2;2=1")
exchangeable_dyad$obedience_p2 <- recode(as.numeric(exchangeable_dyad$stan021_p2), "1=2;2=1")
mean_authoritarianism_2 = polychor(exchangeable_dyad$obedience_p1, exchangeable_dyad$obedience_p2)
# good manners/curiosity
exchangeable_dyad$manners_p1 <- as.numeric(exchangeable_dyad$stan022_p1)
exchangeable_dyad$manners_p2 <- as.numeric(exchangeable_dyad$stan022_p2)
mean_authoritarianism_3 = polychor(exchangeable_dyad$manners_p1, exchangeable_dyad$manners_p2)
# considerate vs. well-behaved
exchangeable_dyad$behaved_p1 <- recode(as.numeric(exchangeable_dyad$stan023_p1), "1=2;2=1")
exchangeable_dyad$behaved_p2 <- recode(as.numeric(exchangeable_dyad$stan023_p2), "1=2;2=1")
mean_authoritarianism_4 = polychor(exchangeable_dyad$behaved_p1, exchangeable_dyad$behaved_p2)
mean_authoritarianism = mean(c(mean_authoritarianism_1, mean_authoritarianism_2, mean_authoritarianism_3, 
                               mean_authoritarianism_4))
# latent
latent_personality_p1_holder <- with(exchangeable_dyad, fscores(mirt(cbind(independence_p1, obedience_p1, 
                                                                           manners_p1, behaved_p1)[-506, ], 1, itemtype = "graded"), full.scores = T, scores.only = T))
latent_personality_p2_holder <- with(exchangeable_dyad, fscores(mirt(cbind(independence_p2, obedience_p2, 
                                                                           manners_p2, behaved_p2)[-115, ], 1, itemtype = "graded"), full.scores = T, scores.only = T))
exchangeable_dyad$latent_personality_p1 <- append(latent_personality_p1_holder, NA, after = 505)
exchangeable_dyad$latent_personality_p2 <- append(latent_personality_p2_holder, NA, after = 114)
cor.test(exchangeable_dyad$latent_personality_p1, exchangeable_dyad$latent_personality_p2, use = "complete.obs")
######################## use ideology, policy positions, and thermometer scores to get a latent variable score for
######################## independents recode consistently to avoide negative discrimination parameters;
######################## liberal-conservative, no opinion as center position
# immigrants
exchangeable_dyad$immigrants_p1 <- recode(as.numeric(exchangeable_dyad$stan024_p1), "2=3;3=2")
exchangeable_dyad$immigrants_p2 <- recode(as.numeric(exchangeable_dyad$stan024_p2), "2=3;3=2")
mean_issues_1 = polychor(exchangeable_dyad$immigrants_p1, exchangeable_dyad$immigrants_p2)
# gvt. regulation of business
exchangeable_dyad$regulation_p1 <- recode(as.numeric(exchangeable_dyad$stan025_p1), "2=3;3=2")
exchangeable_dyad$regulation_p2 <- recode(as.numeric(exchangeable_dyad$stan025_p2), "2=3;3=2")
mean_issues_2 = polychor(exchangeable_dyad$regulation_p1, exchangeable_dyad$regulation_p2)
# people convicted of murder, no opinion between death penalty and prison sentence
exchangeable_dyad$crime_p1 <- recode(as.numeric(exchangeable_dyad$stan026_p1), "1=4;2=2;3=1;4=3")
exchangeable_dyad$crime_p2 <- recode(as.numeric(exchangeable_dyad$stan026_p2), "1=4;2=2;3=1;4=3")
mean_issues_3 = polychor(exchangeable_dyad$crime_p1, exchangeable_dyad$crime_p2)
# income inequality
exchangeable_dyad$income_p1 <- recode(as.numeric(exchangeable_dyad$stan028_p1), "2=3;3=2")
exchangeable_dyad$income_p2 <- recode(as.numeric(exchangeable_dyad$stan028_p2), "2=3;3=2")
mean_issues_4 = polychor(exchangeable_dyad$income_p1, exchangeable_dyad$income_p2)
# services, don't knows as NAS
exchangeable_dyad$services_p1 <- recode(as.numeric(exchangeable_dyad$stan029_p1), "1=7;2=6;3=5;4=4;5=3;6=2;7=1;8=NA")
exchangeable_dyad$services_p2 <- recode(as.numeric(exchangeable_dyad$stan029_p2), "1=7;2=6;3=5;4=4;5=3;6=2;7=1;8=NA")
mean_issues_5 = polychor(exchangeable_dyad$services_p1, exchangeable_dyad$services_p2)
# healthcare, no opinion as have no effect
exchangeable_dyad$healthcare_p1 <- recode(as.numeric(exchangeable_dyad$stan030_p1), "2=3;3=2;4=2")
exchangeable_dyad$healthcare_p2 <- recode(as.numeric(exchangeable_dyad$stan030_p2), "2=3;3=2;4=2")
mean_issues_6 = polychor(exchangeable_dyad$healthcare_p2, exchangeable_dyad$healthcare_p1)
# abortion
exchangeable_dyad$abortion_p1 <- recode(as.numeric(exchangeable_dyad$stan031_p1), "1=3;2=1;3=2")
exchangeable_dyad$abortion_p2 <- recode(as.numeric(exchangeable_dyad$stan031_p2), "1=3;2=1;3=2")
mean_issues_7 = polychor(exchangeable_dyad$abortion_p2, exchangeable_dyad$abortion_p1)
# troops for Isis
exchangeable_dyad$groundtroops_p1 <- recode(as.numeric(exchangeable_dyad$stan032_p1), "1=3;2=1;3=2")
exchangeable_dyad$groundtroops_p2 <- recode(as.numeric(exchangeable_dyad$stan032_p2), "1=3;2=1;3=2")
mean_issues_8 = polychor(exchangeable_dyad$groundtroops_p1, exchangeable_dyad$groundtroops_p2)
# marijuana
exchangeable_dyad$marijuana_p1 <- as.numeric(exchangeable_dyad$stan033_p1)
exchangeable_dyad$marijuana_p2 <- as.numeric(exchangeable_dyad$stan033_p2)
mean_issues_9 = polychor(exchangeable_dyad$marijuana_p2, exchangeable_dyad$marijuana_p1)
# gay marriage
exchangeable_dyad$marriage_p1 <- recode(as.numeric(exchangeable_dyad$stan034_p1), "2=3;3=2")
exchangeable_dyad$marriage_p2 <- recode(as.numeric(exchangeable_dyad$stan034_p2), "2=3;3=2")
mean_issues_10 = polychor(exchangeable_dyad$marriage_p1, exchangeable_dyad$marriage_p2)
mean_issues = mean(c(mean_issues_1, mean_issues_2, mean_issues_3, mean_issues_4, mean_issues_5, mean_issues_6, 
                     mean_issues_7, mean_issues_8, mean_issues_9, mean_issues_10))
# three-partite items immigrants, income regulation, income inequality, healthcare, abortion, isis,
# marijuana, gay marriage
####### dem feeling thermometers
exchangeable_dyad$dem_feeling_p1 <- 100 - (as.numeric(exchangeable_dyad$stan004_p1) - 1)
exchangeable_dyad$dem_feeling_p2 <- 100 - (as.numeric(exchangeable_dyad$stan004_p2) - 1)
feeling_mean_1 = round(cor(exchangeable_dyad$dem_feeling_p1, exchangeable_dyad$dem_feeling_p2, use = "complete.obs"), 
                       2)
exchangeable_dyad$dem_feeling_ordinal_p1 <- NA
exchangeable_dyad$dem_feeling_ordinal_p1[exchangeable_dyad$dem_feeling_p1 > 80] <- 5
exchangeable_dyad$dem_feeling_ordinal_p1[exchangeable_dyad$dem_feeling_p1 <= 80 & exchangeable_dyad$dem_feeling_p1 > 
                                           60] <- 4
exchangeable_dyad$dem_feeling_ordinal_p1[exchangeable_dyad$dem_feeling_p1 <= 60 & exchangeable_dyad$dem_feeling_p1 > 
                                           40] <- 3
exchangeable_dyad$dem_feeling_ordinal_p1[exchangeable_dyad$dem_feeling_p1 <= 40 & exchangeable_dyad$dem_feeling_p1 > 
                                           20] <- 2
exchangeable_dyad$dem_feeling_ordinal_p1[exchangeable_dyad$dem_feeling_p1 <= 20] <- 1
exchangeable_dyad$dem_feeling_ordinal_p2 <- NA
exchangeable_dyad$dem_feeling_ordinal_p2[exchangeable_dyad$dem_feeling_p2 > 80] <- 5
exchangeable_dyad$dem_feeling_ordinal_p2[exchangeable_dyad$dem_feeling_p2 <= 80 & exchangeable_dyad$dem_feeling_p2 > 
                                           60] <- 4
exchangeable_dyad$dem_feeling_ordinal_p2[exchangeable_dyad$dem_feeling_p2 <= 60 & exchangeable_dyad$dem_feeling_p2 > 
                                           40] <- 3
exchangeable_dyad$dem_feeling_ordinal_p2[exchangeable_dyad$dem_feeling_p2 <= 40 & exchangeable_dyad$dem_feeling_p2 > 
                                           20] <- 2
exchangeable_dyad$dem_feeling_ordinal_p2[exchangeable_dyad$dem_feeling_p2 <= 20] <- 1
# Republican
exchangeable_dyad$rep_feeling_p1 <- (as.numeric(exchangeable_dyad$stan005_p1) - 1)
exchangeable_dyad$rep_feeling_p2 <- (as.numeric(exchangeable_dyad$stan005_p2) - 1)
feeling_mean_2 = round(cor(exchangeable_dyad$rep_feeling_p1, exchangeable_dyad$rep_feeling_p2, use = "complete.obs"), 
                       2)
exchangeable_dyad$rep_feeling_ordinal_p1 <- NA
exchangeable_dyad$rep_feeling_ordinal_p1[exchangeable_dyad$rep_feeling_p1 > 80] <- 5
exchangeable_dyad$rep_feeling_ordinal_p1[exchangeable_dyad$rep_feeling_p1 <= 80 & exchangeable_dyad$rep_feeling_p1 > 
                                           60] <- 4
exchangeable_dyad$rep_feeling_ordinal_p1[exchangeable_dyad$rep_feeling_p1 <= 60 & exchangeable_dyad$rep_feeling_p1 > 
                                           40] <- 3
exchangeable_dyad$rep_feeling_ordinal_p1[exchangeable_dyad$rep_feeling_p1 <= 40 & exchangeable_dyad$rep_feeling_p1 > 
                                           20] <- 2
exchangeable_dyad$rep_feeling_ordinal_p1[exchangeable_dyad$rep_feeling_p1 <= 20] <- 1
exchangeable_dyad$rep_feeling_ordinal_p2 <- NA
exchangeable_dyad$rep_feeling_ordinal_p2[exchangeable_dyad$rep_feeling_p2 > 80] <- 5
exchangeable_dyad$rep_feeling_ordinal_p2[exchangeable_dyad$rep_feeling_p2 <= 80 & exchangeable_dyad$rep_feeling_p2 > 
                                           60] <- 4
exchangeable_dyad$rep_feeling_ordinal_p2[exchangeable_dyad$rep_feeling_p2 <= 60 & exchangeable_dyad$rep_feeling_p2 > 
                                           40] <- 3
exchangeable_dyad$rep_feeling_ordinal_p2[exchangeable_dyad$rep_feeling_p2 <= 40 & exchangeable_dyad$rep_feeling_p2 > 
                                           20] <- 2
exchangeable_dyad$rep_feeling_ordinal_p2[exchangeable_dyad$rep_feeling_p2 <= 20] <- 1
# Tea party
exchangeable_dyad$tea_feeling_p1 <- (as.numeric(exchangeable_dyad$stan006_p1) - 1)
exchangeable_dyad$tea_feeling_p2 <- (as.numeric(exchangeable_dyad$stan006_p2) - 1)
feeling_mean_3 = round(cor(exchangeable_dyad$tea_feeling_p1, exchangeable_dyad$tea_feeling_p2, use = "complete.obs"), 
                       2)
exchangeable_dyad$tea_feeling_ordinal_p1 <- NA
exchangeable_dyad$tea_feeling_ordinal_p1[exchangeable_dyad$tea_feeling_p1 > 80] <- 5
exchangeable_dyad$tea_feeling_ordinal_p1[exchangeable_dyad$tea_feeling_p1 <= 80 & exchangeable_dyad$tea_feeling_p1 > 
                                           60] <- 4
exchangeable_dyad$tea_feeling_ordinal_p1[exchangeable_dyad$tea_feeling_p1 <= 60 & exchangeable_dyad$tea_feeling_p1 > 
                                           40] <- 3
exchangeable_dyad$tea_feeling_ordinal_p1[exchangeable_dyad$tea_feeling_p1 <= 40 & exchangeable_dyad$tea_feeling_p1 > 
                                           20] <- 2
exchangeable_dyad$tea_feeling_ordinal_p1[exchangeable_dyad$tea_feeling_p1 <= 20] <- 1
exchangeable_dyad$tea_feeling_ordinal_p2 <- NA
exchangeable_dyad$tea_feeling_ordinal_p2[exchangeable_dyad$tea_feeling_p2 > 80] <- 5
exchangeable_dyad$tea_feeling_ordinal_p2[exchangeable_dyad$tea_feeling_p2 <= 80 & exchangeable_dyad$tea_feeling_p2 > 
                                           60] <- 4
exchangeable_dyad$tea_feeling_ordinal_p2[exchangeable_dyad$tea_feeling_p2 <= 60 & exchangeable_dyad$tea_feeling_p2 > 
                                           40] <- 3
exchangeable_dyad$tea_feeling_ordinal_p2[exchangeable_dyad$tea_feeling_p2 <= 40 & exchangeable_dyad$tea_feeling_p2 > 
                                           20] <- 2
exchangeable_dyad$tea_feeling_ordinal_p2[exchangeable_dyad$tea_feeling_p2 <= 20] <- 1
# Obama
exchangeable_dyad$obama_feeling_p1 <- 100 - (as.numeric(exchangeable_dyad$stan007_p1) - 1)
exchangeable_dyad$obama_feeling_p2 <- 100 - (as.numeric(exchangeable_dyad$stan007_p2) - 1)
feeling_mean_4 = round(cor(exchangeable_dyad$obama_feeling_p1, exchangeable_dyad$obama_feeling_p2, use = "complete.obs"), 
                       2)
exchangeable_dyad$obama_feeling_ordinal_p1 <- NA
exchangeable_dyad$obama_feeling_ordinal_p1[exchangeable_dyad$obama_feeling_p1 > 80] <- 5
exchangeable_dyad$obama_feeling_ordinal_p1[exchangeable_dyad$obama_feeling_p1 <= 80 & exchangeable_dyad$obama_feeling_p1 > 
                                             60] <- 4
exchangeable_dyad$obama_feeling_ordinal_p1[exchangeable_dyad$obama_feeling_p1 <= 60 & exchangeable_dyad$obama_feeling_p1 > 
                                             40] <- 3
exchangeable_dyad$obama_feeling_ordinal_p1[exchangeable_dyad$obama_feeling_p1 <= 40 & exchangeable_dyad$obama_feeling_p1 > 
                                             20] <- 2
exchangeable_dyad$obama_feeling_ordinal_p1[exchangeable_dyad$obama_feeling_p1 <= 20] <- 1
exchangeable_dyad$obama_feeling_ordinal_p2 <- NA
exchangeable_dyad$obama_feeling_ordinal_p2[exchangeable_dyad$obama_feeling_p2 > 80] <- 5
exchangeable_dyad$obama_feeling_ordinal_p2[exchangeable_dyad$obama_feeling_p2 <= 80 & exchangeable_dyad$obama_feeling_p2 > 
                                             60] <- 4
exchangeable_dyad$obama_feeling_ordinal_p2[exchangeable_dyad$obama_feeling_p2 <= 60 & exchangeable_dyad$obama_feeling_p2 > 
                                             40] <- 3
exchangeable_dyad$obama_feeling_ordinal_p2[exchangeable_dyad$obama_feeling_p2 <= 40 & exchangeable_dyad$obama_feeling_p2 > 
                                             20] <- 2
exchangeable_dyad$obama_feeling_ordinal_p2[exchangeable_dyad$obama_feeling_p2 <= 20] <- 1
# Clinton
exchangeable_dyad$clinton_feeling_p1 <- 100 - (as.numeric(exchangeable_dyad$stan008_p1) - 1)
exchangeable_dyad$clinton_feeling_p2 <- 100 - (as.numeric(exchangeable_dyad$stan008_p2) - 1)
feeling_mean_5 = round(cor(exchangeable_dyad$clinton_feeling_p2, exchangeable_dyad$clinton_feeling_p1, 
                           use = "complete.obs"), 2)
exchangeable_dyad$clinton_feeling_ordinal_p1 <- NA
exchangeable_dyad$clinton_feeling_ordinal_p1[exchangeable_dyad$clinton_feeling_p1 > 80] <- 5
exchangeable_dyad$clinton_feeling_ordinal_p1[exchangeable_dyad$clinton_feeling_p1 <= 80 & exchangeable_dyad$clinton_feeling_p1 > 
                                               60] <- 4
exchangeable_dyad$clinton_feeling_ordinal_p1[exchangeable_dyad$clinton_feeling_p1 <= 60 & exchangeable_dyad$clinton_feeling_p1 > 
                                               40] <- 3
exchangeable_dyad$clinton_feeling_ordinal_p1[exchangeable_dyad$clinton_feeling_p1 <= 40 & exchangeable_dyad$clinton_feeling_p1 > 
                                               20] <- 2
exchangeable_dyad$clinton_feeling_ordinal_p1[exchangeable_dyad$clinton_feeling_p1 <= 20] <- 1
exchangeable_dyad$clinton_feeling_ordinal_p2 <- NA
exchangeable_dyad$clinton_feeling_ordinal_p2[exchangeable_dyad$clinton_feeling_p2 > 80] <- 5
exchangeable_dyad$clinton_feeling_ordinal_p2[exchangeable_dyad$clinton_feeling_p2 <= 80 & exchangeable_dyad$clinton_feeling_p2 > 
                                               60] <- 4
exchangeable_dyad$clinton_feeling_ordinal_p2[exchangeable_dyad$clinton_feeling_p2 <= 60 & exchangeable_dyad$clinton_feeling_p2 > 
                                               40] <- 3
exchangeable_dyad$clinton_feeling_ordinal_p2[exchangeable_dyad$clinton_feeling_p2 <= 40 & exchangeable_dyad$clinton_feeling_p2 > 
                                               20] <- 2
exchangeable_dyad$clinton_feeling_ordinal_p2[exchangeable_dyad$clinton_feeling_p2 <= 20] <- 1
# Paul
exchangeable_dyad$paul_feeling_p1 <- (as.numeric(exchangeable_dyad$stan009_p1) - 1)
exchangeable_dyad$paul_feeling_p2 <- (as.numeric(exchangeable_dyad$stan009_p2) - 1)
feeling_mean_6 = round(cor(exchangeable_dyad$paul_feeling_p1, exchangeable_dyad$paul_feeling_p2, use = "complete.obs"), 
                       2)
exchangeable_dyad$paul_feeling_ordinal_p1 <- NA
exchangeable_dyad$paul_feeling_ordinal_p1[exchangeable_dyad$paul_feeling_p1 > 80] <- 5
exchangeable_dyad$paul_feeling_ordinal_p1[exchangeable_dyad$paul_feeling_p1 <= 80 & exchangeable_dyad$paul_feeling_p1 > 
                                            60] <- 4
exchangeable_dyad$paul_feeling_ordinal_p1[exchangeable_dyad$paul_feeling_p1 <= 60 & exchangeable_dyad$paul_feeling_p1 > 
                                            40] <- 3
exchangeable_dyad$paul_feeling_ordinal_p1[exchangeable_dyad$paul_feeling_p1 <= 40 & exchangeable_dyad$paul_feeling_p1 > 
                                            20] <- 2
exchangeable_dyad$paul_feeling_ordinal_p1[exchangeable_dyad$paul_feeling_p1 <= 20] <- 1
exchangeable_dyad$paul_feeling_ordinal_p2 <- NA
exchangeable_dyad$paul_feeling_ordinal_p2[exchangeable_dyad$paul_feeling_p2 > 80] <- 5
exchangeable_dyad$paul_feeling_ordinal_p2[exchangeable_dyad$paul_feeling_p2 <= 80 & exchangeable_dyad$paul_feeling_p2 > 
                                            60] <- 4
exchangeable_dyad$paul_feeling_ordinal_p2[exchangeable_dyad$paul_feeling_p2 <= 60 & exchangeable_dyad$paul_feeling_p2 > 
                                            40] <- 3
exchangeable_dyad$paul_feeling_ordinal_p2[exchangeable_dyad$paul_feeling_p2 <= 40 & exchangeable_dyad$paul_feeling_p2 > 
                                            20] <- 2
exchangeable_dyad$paul_feeling_ordinal_p2[exchangeable_dyad$paul_feeling_p2 <= 20] <- 1
# Bush
exchangeable_dyad$bush_feeling_p1 <- (as.numeric(exchangeable_dyad$stan010_p1) - 1)
exchangeable_dyad$bush_feeling_p2 <- (as.numeric(exchangeable_dyad$stan010_p2) - 1)
feeling_mean_6 = round(cor(exchangeable_dyad$bush_feeling_p1, exchangeable_dyad$bush_feeling_p2, use = "complete.obs"), 
                       2)
exchangeable_dyad$bush_feeling_ordinal_p1 <- NA
exchangeable_dyad$bush_feeling_ordinal_p1[exchangeable_dyad$bush_feeling_p1 > 80] <- 5
exchangeable_dyad$bush_feeling_ordinal_p1[exchangeable_dyad$bush_feeling_p1 <= 80 & exchangeable_dyad$bush_feeling_p1 > 
                                            60] <- 4
exchangeable_dyad$bush_feeling_ordinal_p1[exchangeable_dyad$bush_feeling_p1 <= 60 & exchangeable_dyad$bush_feeling_p1 > 
                                            40] <- 3
exchangeable_dyad$bush_feeling_ordinal_p1[exchangeable_dyad$bush_feeling_p1 <= 40 & exchangeable_dyad$bush_feeling_p1 > 
                                            20] <- 2
exchangeable_dyad$bush_feeling_ordinal_p1[exchangeable_dyad$bush_feeling_p1 <= 20] <- 1
exchangeable_dyad$bush_feeling_ordinal_p2 <- NA
exchangeable_dyad$bush_feeling_ordinal_p2[exchangeable_dyad$bush_feeling_p2 > 80] <- 5
exchangeable_dyad$bush_feeling_ordinal_p2[exchangeable_dyad$bush_feeling_p2 <= 80 & exchangeable_dyad$bush_feeling_p2 > 
                                            60] <- 4
exchangeable_dyad$bush_feeling_ordinal_p2[exchangeable_dyad$bush_feeling_p2 <= 60 & exchangeable_dyad$bush_feeling_p2 > 
                                            40] <- 3
exchangeable_dyad$bush_feeling_ordinal_p2[exchangeable_dyad$bush_feeling_p2 <= 40 & exchangeable_dyad$bush_feeling_p2 > 
                                            20] <- 2
exchangeable_dyad$bush_feeling_ordinal_p2[exchangeable_dyad$bush_feeling_p2 <= 20] <- 1
# Cruz
exchangeable_dyad$cruz_feeling_p1 <- (as.numeric(exchangeable_dyad$stan011_p1) - 1)
exchangeable_dyad$cruz_feeling_p2 <- (as.numeric(exchangeable_dyad$stan011_p2) - 1)
feeling_mean_7 = round(cor(exchangeable_dyad$cruz_feeling_p1, exchangeable_dyad$cruz_feeling_p2, use = "complete.obs"), 
                       2)
exchangeable_dyad$cruz_feeling_ordinal_p1 <- NA
exchangeable_dyad$cruz_feeling_ordinal_p1[exchangeable_dyad$cruz_feeling_p1 > 80] <- 5
exchangeable_dyad$cruz_feeling_ordinal_p1[exchangeable_dyad$cruz_feeling_p1 <= 80 & exchangeable_dyad$cruz_feeling_p1 > 
                                            60] <- 4
exchangeable_dyad$cruz_feeling_ordinal_p1[exchangeable_dyad$cruz_feeling_p1 <= 60 & exchangeable_dyad$cruz_feeling_p1 > 
                                            40] <- 3
exchangeable_dyad$cruz_feeling_ordinal_p1[exchangeable_dyad$cruz_feeling_p1 <= 40 & exchangeable_dyad$cruz_feeling_p1 > 
                                            20] <- 2
exchangeable_dyad$cruz_feeling_ordinal_p1[exchangeable_dyad$cruz_feeling_p1 <= 20] <- 1
exchangeable_dyad$cruz_feeling_ordinal_p2 <- NA
exchangeable_dyad$cruz_feeling_ordinal_p2[exchangeable_dyad$cruz_feeling_p2 > 80] <- 5
exchangeable_dyad$cruz_feeling_ordinal_p2[exchangeable_dyad$cruz_feeling_p2 <= 80 & exchangeable_dyad$cruz_feeling_p2 > 
                                            60] <- 4
exchangeable_dyad$cruz_feeling_ordinal_p2[exchangeable_dyad$cruz_feeling_p2 <= 60 & exchangeable_dyad$cruz_feeling_p2 > 
                                            40] <- 3
exchangeable_dyad$cruz_feeling_ordinal_p2[exchangeable_dyad$cruz_feeling_p2 <= 40 & exchangeable_dyad$cruz_feeling_p2 > 
                                            20] <- 2
exchangeable_dyad$cruz_feeling_ordinal_p2[exchangeable_dyad$cruz_feeling_p2 <= 20] <- 1
# Trump
exchangeable_dyad$trump_feeling_p1 <- (as.numeric(exchangeable_dyad$stan011b_p1) - 1)
exchangeable_dyad$trump_feeling_p2 <- (as.numeric(exchangeable_dyad$stan011b_p2) - 1)
feeling_mean_9 = round(cor(exchangeable_dyad$trump_feeling_p1, exchangeable_dyad$trump_feeling_p2, use = "complete.obs"), 
                       2)
exchangeable_dyad$trump_feeling_ordinal_p1 <- NA
exchangeable_dyad$trump_feeling_ordinal_p1[exchangeable_dyad$trump_feeling_p1 > 80] <- 5
exchangeable_dyad$trump_feeling_ordinal_p1[exchangeable_dyad$trump_feeling_p1 <= 80 & exchangeable_dyad$trump_feeling_p1 > 
                                             60] <- 4
exchangeable_dyad$trump_feeling_ordinal_p1[exchangeable_dyad$trump_feeling_p1 <= 60 & exchangeable_dyad$trump_feeling_p1 > 
                                             40] <- 3
exchangeable_dyad$trump_feeling_ordinal_p1[exchangeable_dyad$trump_feeling_p1 <= 40 & exchangeable_dyad$trump_feeling_p1 > 
                                             20] <- 2
exchangeable_dyad$trump_feeling_ordinal_p1[exchangeable_dyad$trump_feeling_p1 <= 20] <- 1
exchangeable_dyad$trump_feeling_ordinal_p2 <- NA
exchangeable_dyad$trump_feeling_ordinal_p2[exchangeable_dyad$trump_feeling_p2 > 80] <- 5
exchangeable_dyad$trump_feeling_ordinal_p2[exchangeable_dyad$trump_feeling_p2 <= 80 & exchangeable_dyad$trump_feeling_p2 > 
                                             60] <- 4
exchangeable_dyad$trump_feeling_ordinal_p2[exchangeable_dyad$trump_feeling_p2 <= 60 & exchangeable_dyad$trump_feeling_p2 > 
                                             40] <- 3
exchangeable_dyad$trump_feeling_ordinal_p2[exchangeable_dyad$trump_feeling_p2 <= 40 & exchangeable_dyad$trump_feeling_p2 > 
                                             20] <- 2
exchangeable_dyad$trump_feeling_ordinal_p2[exchangeable_dyad$trump_feeling_p2 <= 20] <- 1
# NRA
exchangeable_dyad$nra_feeling_p1 <- (as.numeric(exchangeable_dyad$stan012_p1) - 1)
exchangeable_dyad$nra_feeling_p2 <- (as.numeric(exchangeable_dyad$stan012_p2) - 1)
feeling_mean_10 = round(cor(exchangeable_dyad$nra_feeling_p1, exchangeable_dyad$nra_feeling_p2, use = "complete.obs"), 
                        2)
exchangeable_dyad$nra_feeling_ordinal_p1 <- NA
exchangeable_dyad$nra_feeling_ordinal_p1[exchangeable_dyad$nra_feeling_p1 > 80] <- 5
exchangeable_dyad$nra_feeling_ordinal_p1[exchangeable_dyad$nra_feeling_p1 <= 80 & exchangeable_dyad$nra_feeling_p1 > 
                                           60] <- 4
exchangeable_dyad$nra_feeling_ordinal_p1[exchangeable_dyad$nra_feeling_p1 <= 60 & exchangeable_dyad$nra_feeling_p1 > 
                                           40] <- 3
exchangeable_dyad$nra_feeling_ordinal_p1[exchangeable_dyad$nra_feeling_p1 <= 40 & exchangeable_dyad$nra_feeling_p1 > 
                                           20] <- 2
exchangeable_dyad$nra_feeling_ordinal_p1[exchangeable_dyad$nra_feeling_p1 <= 20] <- 1
exchangeable_dyad$nra_feeling_ordinal_p2 <- NA
exchangeable_dyad$nra_feeling_ordinal_p2[exchangeable_dyad$nra_feeling_p2 > 80] <- 5
exchangeable_dyad$nra_feeling_ordinal_p2[exchangeable_dyad$nra_feeling_p2 <= 80 & exchangeable_dyad$nra_feeling_p2 > 
                                           60] <- 4
exchangeable_dyad$nra_feeling_ordinal_p2[exchangeable_dyad$nra_feeling_p2 <= 60 & exchangeable_dyad$nra_feeling_p2 > 
                                           40] <- 3
exchangeable_dyad$nra_feeling_ordinal_p2[exchangeable_dyad$nra_feeling_p2 <= 40 & exchangeable_dyad$nra_feeling_p2 > 
                                           20] <- 2
exchangeable_dyad$nra_feeling_ordinal_p2[exchangeable_dyad$nra_feeling_p2 <= 20] <- 1
# gay
exchangeable_dyad$gay_feeling_p1 <- 100 - (as.numeric(exchangeable_dyad$stan013_p1) - 1)
exchangeable_dyad$gay_feeling_p2 <- 100 - (as.numeric(exchangeable_dyad$stan013_p2) - 1)
feeling_mean_11 = round(cor(exchangeable_dyad$gay_feeling_p1, exchangeable_dyad$gay_feeling_p2, use = "complete.obs"), 
                        2)
exchangeable_dyad$gay_feeling_ordinal_p1 <- NA
exchangeable_dyad$gay_feeling_ordinal_p1[exchangeable_dyad$gay_feeling_p1 > 80] <- 5
exchangeable_dyad$gay_feeling_ordinal_p1[exchangeable_dyad$gay_feeling_p1 <= 80 & exchangeable_dyad$gay_feeling_p1 > 
                                           60] <- 4
exchangeable_dyad$gay_feeling_ordinal_p1[exchangeable_dyad$gay_feeling_p1 <= 60 & exchangeable_dyad$gay_feeling_p1 > 
                                           40] <- 3
exchangeable_dyad$gay_feeling_ordinal_p1[exchangeable_dyad$gay_feeling_p1 <= 40 & exchangeable_dyad$gay_feeling_p1 > 
                                           20] <- 2
exchangeable_dyad$gay_feeling_ordinal_p1[exchangeable_dyad$gay_feeling_p1 <= 20] <- 1
exchangeable_dyad$gay_feeling_ordinal_p2 <- NA
exchangeable_dyad$gay_feeling_ordinal_p2[exchangeable_dyad$gay_feeling_p2 > 80] <- 5
exchangeable_dyad$gay_feeling_ordinal_p2[exchangeable_dyad$gay_feeling_p2 <= 80 & exchangeable_dyad$gay_feeling_p2 > 
                                           60] <- 4
exchangeable_dyad$gay_feeling_ordinal_p2[exchangeable_dyad$gay_feeling_p2 <= 60 & exchangeable_dyad$gay_feeling_p2 > 
                                           40] <- 3
exchangeable_dyad$gay_feeling_ordinal_p2[exchangeable_dyad$gay_feeling_p2 <= 40 & exchangeable_dyad$gay_feeling_p2 > 
                                           20] <- 2
exchangeable_dyad$gay_feeling_ordinal_p2[exchangeable_dyad$gay_feeling_p2 <= 20] <- 1
# Christians
exchangeable_dyad$christians_feeling_p1 <- (as.numeric(exchangeable_dyad$stan014_p1) - 1)
exchangeable_dyad$christians_feeling_p2 <- (as.numeric(exchangeable_dyad$stan014_p2) - 1)
feeling_mean_12 = round(cor(exchangeable_dyad$christians_feeling_p1, exchangeable_dyad$christians_feeling_p2, 
                            use = "complete.obs"), 2)
exchangeable_dyad$christians_feeling_ordinal_p1 <- NA
exchangeable_dyad$christians_feeling_ordinal_p1[exchangeable_dyad$christians_feeling_p1 > 80] <- 5
exchangeable_dyad$christians_feeling_ordinal_p1[exchangeable_dyad$christians_feeling_p1 <= 80 & exchangeable_dyad$christians_feeling_p1 > 
                                                  60] <- 4
exchangeable_dyad$christians_feeling_ordinal_p1[exchangeable_dyad$christians_feeling_p1 <= 60 & exchangeable_dyad$christians_feeling_p1 > 
                                                  40] <- 3
exchangeable_dyad$christians_feeling_ordinal_p1[exchangeable_dyad$christians_feeling_p1 <= 40 & exchangeable_dyad$christians_feeling_p1 > 
                                                  20] <- 2
exchangeable_dyad$christians_feeling_ordinal_p1[exchangeable_dyad$christians_feeling_p1 <= 20] <- 1
exchangeable_dyad$christians_feeling_ordinal_p2 <- NA
exchangeable_dyad$christians_feeling_ordinal_p2[exchangeable_dyad$christians_feeling_p2 > 80] <- 5
exchangeable_dyad$christians_feeling_ordinal_p2[exchangeable_dyad$christians_feeling_p2 <= 80 & exchangeable_dyad$christians_feeling_p2 > 
                                                  60] <- 4
exchangeable_dyad$christians_feeling_ordinal_p2[exchangeable_dyad$christians_feeling_p2 <= 60 & exchangeable_dyad$christians_feeling_p2 > 
                                                  40] <- 3
exchangeable_dyad$christians_feeling_ordinal_p2[exchangeable_dyad$christians_feeling_p2 <= 40 & exchangeable_dyad$christians_feeling_p2 > 
                                                  20] <- 2
exchangeable_dyad$christians_feeling_ordinal_p2[exchangeable_dyad$christians_feeling_p2 <= 20] <- 1
# Atheists
exchangeable_dyad$atheists_feeling_p1 <- 100 - (as.numeric(exchangeable_dyad$stan015_p1) - 1)
exchangeable_dyad$atheists_feeling_p2 <- 100 - (as.numeric(exchangeable_dyad$stan015_p2) - 1)
feeling_mean_13 = round(cor(exchangeable_dyad$atheists_feeling_p1, exchangeable_dyad$atheists_feeling_p2, 
                            use = "complete.obs"), 2)
exchangeable_dyad$atheists_feeling_ordinal_p1 <- NA
exchangeable_dyad$atheists_feeling_ordinal_p1[exchangeable_dyad$atheists_feeling_p1 > 80] <- 5
exchangeable_dyad$atheists_feeling_ordinal_p1[exchangeable_dyad$atheists_feeling_p1 <= 80 & exchangeable_dyad$atheists_feeling_p1 > 
                                                60] <- 4
exchangeable_dyad$atheists_feeling_ordinal_p1[exchangeable_dyad$atheists_feeling_p1 <= 60 & exchangeable_dyad$atheists_feeling_p1 > 
                                                40] <- 3
exchangeable_dyad$atheists_feeling_ordinal_p1[exchangeable_dyad$atheists_feeling_p1 <= 40 & exchangeable_dyad$atheists_feeling_p1 > 
                                                20] <- 2
exchangeable_dyad$atheists_feeling_ordinal_p1[exchangeable_dyad$atheists_feeling_p1 <= 20] <- 1
exchangeable_dyad$atheists_feeling_ordinal_p2 <- NA
exchangeable_dyad$atheists_feeling_ordinal_p2[exchangeable_dyad$atheists_feeling_p2 > 80] <- 5
exchangeable_dyad$atheists_feeling_ordinal_p2[exchangeable_dyad$atheists_feeling_p2 <= 80 & exchangeable_dyad$atheists_feeling_p2 > 
                                                60] <- 4
exchangeable_dyad$atheists_feeling_ordinal_p2[exchangeable_dyad$atheists_feeling_p2 <= 60 & exchangeable_dyad$atheists_feeling_p2 > 
                                                40] <- 3
exchangeable_dyad$atheists_feeling_ordinal_p2[exchangeable_dyad$atheists_feeling_p2 <= 40 & exchangeable_dyad$atheists_feeling_p2 > 
                                                20] <- 2
exchangeable_dyad$atheists_feeling_ordinal_p2[exchangeable_dyad$atheists_feeling_p2 <= 20] <- 1
feeling_mean = mean(c(feeling_mean_1, feeling_mean_2, feeling_mean_3, feeling_mean_4, feeling_mean_5, 
                      feeling_mean_9, feeling_mean_10, feeling_mean_11, feeling_mean_12, feeling_mean_13))
# overal partisan attitudes
exchangeable_dyad$latent_partisan_attitudes_p1 <- with(exchangeable_dyad, fscores(mirt(cbind(Ideology_p1, 
                                                                                             Party_p1, tea_feeling_ordinal_p1, nra_feeling_ordinal_p1, gay_feeling_ordinal_p1, christians_feeling_ordinal_p1, 
                                                                                             atheists_feeling_ordinal_p1, dem_feeling_ordinal_p1, rep_feeling_ordinal_p1, obama_feeling_ordinal_p1, 
                                                                                             clinton_feeling_ordinal_p1, paul_feeling_ordinal_p1, bush_feeling_ordinal_p1, cruz_feeling_ordinal_p1, 
                                                                                             trump_feeling_ordinal_p1), 1, itemtype = "graded"), full.scores = TRUE, scores.only = TRUE))
exchangeable_dyad$latent_partisan_attitudes_p2 <- with(exchangeable_dyad, fscores(mirt(cbind(Ideology_p2, 
                                                                                             Party_p2, tea_feeling_ordinal_p2, nra_feeling_ordinal_p2, gay_feeling_ordinal_p2, christians_feeling_ordinal_p2, 
                                                                                             atheists_feeling_ordinal_p2, dem_feeling_ordinal_p2, rep_feeling_ordinal_p2, obama_feeling_ordinal_p2, 
                                                                                             clinton_feeling_ordinal_p2, paul_feeling_ordinal_p2, bush_feeling_ordinal_p2, cruz_feeling_ordinal_p2, 
                                                                                             trump_feeling_ordinal_p2), 1, itemtype = "graded"), full.scores = TRUE, scores.only = TRUE))
cor.test(exchangeable_dyad$latent_partisan_attitudes_p2, exchangeable_dyad$latent_partisan_attitudes_p1)
##################################### religious affiliation##################### religious service attendance
mean_religious_1 = polychor(recode(as.numeric(exchangeable_dyad$stan096_p1), "1=5;2=4;3=3;4=2;5=1"), 
                            recode(as.numeric(exchangeable_dyad$stan096_p2), "1=5;2=4;3=3;4=2;5=1"))
mean_religious_2 = polychor(recode(as.numeric(exchangeable_dyad$stan097_p1), "1=5;2=4;3=3;4=2;5=1"), 
                            recode(as.numeric(exchangeable_dyad$stan097_p2), "1=5;2=4;3=3;4=2;5=1"))
exchangeable_dyad$religious_attendance_p1 <- fscores(mirt(cbind(recode(as.numeric(exchangeable_dyad$stan096_p1), 
                                                                       "1=5;2=4;3=3;4=2;5=1"), recode(as.numeric(exchangeable_dyad$stan097_p1), "1=5;2=4;3=3;4=2;5=1"), 
                                                                exchangeable_dyad$christians_feeling_ordinal_p1, exchangeable_dyad$atheists_feeling_ordinal_p1), 
                                                          1, itemtype = "graded"), full.scores = TRUE, scores.only = TRUE)
exchangeable_dyad$religious_attendance_p2 <- fscores(mirt(cbind(recode(as.numeric(exchangeable_dyad$stan096_p2), 
                                                                       "1=5;2=4;3=3;4=2;5=1"), recode(as.numeric(exchangeable_dyad$stan097_p2), "1=5;2=4;3=3;4=2;5=1"), 
                                                                exchangeable_dyad$christians_feeling_ordinal_p2, exchangeable_dyad$atheists_feeling_ordinal_p2), 
                                                          1, itemtype = "graded"), full.scores = TRUE, scores.only = TRUE)
cor.test(exchangeable_dyad$religious_attendance_p1, exchangeable_dyad$religious_attendance_p2)
mean_religious = mean(c(mean_religious_1, mean_religious_2))
################################################ get latent ideology and party scores###########
exchangeable_dyad$latent_ideology_p1 <- fscores(mirt(data.frame(exchangeable_dyad[, c("immigrants_p1", 
                                                                                      "regulation_p1", "crime_p1", "income_p1", "services_p1", "healthcare_p1", "abortion_p1", "groundtroops_p1", 
                                                                                      "marijuana_p1", "marriage_p1")]), 1), full.scores = T, scores.only = T)
exchangeable_dyad$latent_ideology_p2 <- fscores(mirt(data.frame(exchangeable_dyad[, c("immigrants_p2", 
                                                                                      "regulation_p2", "crime_p2", "income_p2", "services_p2", "healthcare_p2", "abortion_p2", "groundtroops_p2", 
                                                                                      "marijuana_p2", "marriage_p2")]), 1), full.scores = T)
with(exchangeable_dyad, cor.test(latent_ideology_p1, latent_ideology_p2))
latent_party_model_p1 = mirt(data.frame(exchangeable_dyad[, c("Ideology_p1", "Party_p1", "dem_feeling_ordinal_p1", 
                                                              "rep_feeling_ordinal_p1", "tea_feeling_ordinal_p1", "obama_feeling_ordinal_p1", "clinton_feeling_ordinal_p1", 
                                                              "paul_feeling_ordinal_p1", "bush_feeling_ordinal_p1", "cruz_feeling_ordinal_p1", "trump_feeling_ordinal_p1", 
                                                              "nra_feeling_ordinal_p1", "gay_feeling_ordinal_p1")]), 1)  #,'christians_feeling_ordinal_p1','atheists_feeling_ordinal_p1')],1)
latent_party_model_p2 = mirt(data.frame(exchangeable_dyad[, c("Ideology_p2", "Party_p1", "dem_feeling_ordinal_p2", 
                                                              "rep_feeling_ordinal_p2", "tea_feeling_ordinal_p2", "obama_feeling_ordinal_p2", "clinton_feeling_ordinal_p2", 
                                                              "paul_feeling_ordinal_p2", "bush_feeling_ordinal_p2", "cruz_feeling_ordinal_p2", "trump_feeling_ordinal_p2", 
                                                              "nra_feeling_ordinal_p2", "gay_feeling_ordinal_p2")]), 1)  #,'christians_feeling_ordinal_p2', 'atheists_feeling_ordinal_p2')],1)
exchangeable_dyad$latent_party_p1 <- fscores(latent_party_model_p1, full.scores = T)
exchangeable_dyad$latent_party_p2 <- fscores(latent_party_model_p2, full.scores = T)
cor(exchangeable_dyad$latent_party_p1, exchangeable_dyad$latent_party_p2)
################################################ 
# Political Affect/Marriage items, just for outparty
# other parttisan attitudes
# trust, discussion, news
exchangeable_dyad$spousal_discussion_p1 <- with(exchangeable_dyad, recode(as.numeric(stan101_p1), "1=3;2=2;3=1;4=0"))
exchangeable_dyad$spousal_discussion_p2 <- with(exchangeable_dyad, recode(as.numeric(stan101_p2), "1=3;2=2;3=1;4=0"))
exchangeable_dyad$spousal_discussion_overall = (exchangeable_dyad$spousal_discussion_p1 + exchangeable_dyad$spousal_discussion_p2)/2
exchangeable_dyad$resentment_p1 <- fscores(mirt(cbind(recode(as.numeric(exchangeable_dyad$stan084_p1), 
                                                             "1=5;2=4;3=3;4=2;5=1"), recode(as.numeric(exchangeable_dyad$stan085_p1), "1=5;2=4;3=3;4=2;5=1"), 
                                                      recode(as.numeric(exchangeable_dyad$stan086_p1), "1=5;2=4;3=3;4=2;5=1"), as.numeric(exchangeable_dyad$stan087_p1)), 
                                                1, itemtype = "graded"), full.scores = TRUE, scores.only = TRUE)
exchangeable_dyad$resentment_p2 <- fscores(mirt(cbind(recode(as.numeric(exchangeable_dyad$stan084_p2), 
                                                             "1=5;2=4;3=3;4=2;5=1"), recode(as.numeric(exchangeable_dyad$stan085_p2), "1=5;2=4;3=3;4=2;5=1"), 
                                                      recode(as.numeric(exchangeable_dyad$stan086_p2), "1=5;2=4;3=3;4=2;5=1"), as.numeric(exchangeable_dyad$stan087_p2)), 
                                                1, itemtype = "graded"), full.scores = TRUE, scores.only = TRUE)

###interest#####
exchangeable_dyad$latent_interest_p1 <- fscores(mirt(cbind(as.numeric(exchangeable_dyad$stan061_p1), as.numeric(exchangeable_dyad$stan062_p1), 
                                                      as.numeric(exchangeable_dyad$stan063_p1), as.numeric(exchangeable_dyad$stan064_p1), as.numeric(exchangeable_dyad$stan065_p1), 
                                                      as.numeric(exchangeable_dyad$stan066_p1), as.numeric(exchangeable_dyad$stan067_p1), as.numeric(exchangeable_dyad$stan068_p1), 
                                                      as.numeric(exchangeable_dyad$stan069_p1), as.numeric(exchangeable_dyad$stan070_p1)), 1, itemtype = "graded"), 
                                           full.scores = T, scores.only = T)
exchangeable_dyad$latent_interest_p2 <- fscores(mirt(cbind(as.numeric(exchangeable_dyad$stan061_p2), as.numeric(exchangeable_dyad$stan062_p2), 
                                                      as.numeric(exchangeable_dyad$stan063_p2), as.numeric(exchangeable_dyad$stan064_p2), as.numeric(exchangeable_dyad$stan065_p2), 
                                                      as.numeric(exchangeable_dyad$stan066_p2), as.numeric(exchangeable_dyad$stan067_p2), as.numeric(exchangeable_dyad$stan068_p2), 
                                                      as.numeric(exchangeable_dyad$stan069_p2), as.numeric(exchangeable_dyad$stan070_p2)), 1, itemtype = "graded"), 
                                           full.scores = T, scores.only = T)
cor.test(exchangeable_dyad$latent_interest_p1, exchangeable_dyad$latent_interest_p2)

######################### Modeling#######################################################
#load("homogeneity_by_zip_registration_partisan_denominator.RData")
exchangeable_dyad <- data.table(data.frame(exchangeable_dyad))
exchangeable_dyad$inputzip_p1 <- as.character(exchangeable_dyad$inputzip_p1)
zip_code_just_partisan$zip <- as.character(zip_code_just_partisan$zip)
zip_code_just_partisan$zip = ifelse(nchar(zip_code_just_partisan$zip) == 3, paste0("00", zip_code_just_partisan$zip), 
                                    ifelse(nchar(zip_code_just_partisan$zip) == 4, paste0("0", zip_code_just_partisan$zip), zip_code_just_partisan$zip))
setkey(exchangeable_dyad, inputzip_p1)
setkey(zip_code_just_partisan, zip)
exchangeable_dyad = zip_code_just_partisan[exchangeable_dyad]
# recode variables, set up data frame allowing for clutsering convergence and non-linear age
exchangeable_dyad$delta_male_female = exchangeable_dyad$latent_party_p1 - exchangeable_dyad$latent_party_p2
# joint
exchangeable_dyad$delta_male_female_race = exchangeable_dyad$resentment_p1 - exchangeable_dyad$resentment_p2
exchangeable_dyad$delta_male_female_religion = exchangeable_dyad$religious_attendance_p1 - exchangeable_dyad$religious_attendance_p2
exchangeable_dyad$delta_male_female_interest = exchangeable_dyad$latent_interest_p1 - exchangeable_dyad$latent_interest_p2
exchangeable_dyad$delta_male_female_authoritarianism = exchangeable_dyad$latent_personality_p1 - exchangeable_dyad$latent_personality_p2
exchangeable_dyad$delta_male_female_race = exchangeable_dyad$resentment_p1 - exchangeable_dyad$resentment_p2
# agreement:
model_party_table <- lm(abs(delta_male_female) ~ marriage_length_p1 + education_overall + spousal_discussion_overall, 
                        data = exchangeable_dyad)
model_party_table$se <- coeftest(model_party_table, vcov = vcovHC(model_party_table, type = "HC1", cluster = "group"))[, 
                                                                                                                       2]
model_religion_table <- lm(abs(delta_male_female_religion) ~ marriage_length_p1 + education_overall + 
                             spousal_discussion_overall, data = exchangeable_dyad)
model_religion_table$se <- coeftest(model_religion_table, vcov = vcovHC(model_religion_table, type = "HC1", 
                                                                        cluster = "group"))[, 2]
model_authoritarianism_table <- lm(abs(delta_male_female_authoritarianism) ~ marriage_length_p1 + education_overall + 
                                     spousal_discussion_overall, data = exchangeable_dyad)
model_authoritarianism_table$se <- coeftest(model_authoritarianism_table, vcov = vcovHC(model_authoritarianism_table, 
                                                                                        type = "HC1", cluster = "group"))[, 2]
model_racial_resentment_table <- lm(abs(delta_male_female_race) ~ marriage_length_p1 + education_overall + 
                                      spousal_discussion_overall, data = exchangeable_dyad)
model_racial_resentment_table$se <- coeftest(model_racial_resentment_table, vcov = vcovHC(model_racial_resentment_table, 
                                                                                          type = "HC1", cluster = "group"))[, 2]
model_interest_table <- lm(abs(delta_male_female_interest) ~ marriage_length_p1 + education_overall + 
                             spousal_discussion_overall, data = exchangeable_dyad)
model_interest_table$se <- coeftest(model_interest_table, vcov = vcovHC(model_interest_table, 
                                                                        type = "HC1", cluster = "group"))[, 2]

apsrtable(model_party_table, model_religion_table, model_interest_table, 
          digits = 3, se = "robust")

model_authoritarianism_interaction <- lm(abs(delta_male_female) ~ education_overall + spousal_discussion_overall + 
                                           marriage_length_p1 * delta_male_female_authoritarianism, data = exchangeable_dyad)
model_authoritarianism_interaction$se <- coeftest(model_authoritarianism_interaction, vcov = vcovHC(model_authoritarianism_interaction, 
                                                                                                    type = "HC1", cluster = "group"))[, 2]

model_religion_interaction <- lm(abs(delta_male_female) ~ education_overall + spousal_discussion_overall + 
                                   marriage_length_p1 * delta_male_female_religion, data = exchangeable_dyad)
model_religion_interaction$se <- coeftest(model_religion_interaction, vcov = vcovHC(model_religion_interaction, 
                                                                                    type = "HC1", cluster = "group"))[, 2]
apsrtable(model_religion_interaction,  model_authoritarianism_interaction, digits = 3, 
          se = "robust", model.names = c("Religiosity","Authoritarianism"))

# use robust standard errors
#save(exchangeable_dyad, file = "exchangeable_dyad.RData")
######################################################################## 
######## now intergenerational########################################
#setwd("C:\\Users\\tobia\\Dropbox\\Partisan Homogeneity Project\\Deliverables_20150921")
#p_c_dyad <- as.data.set(spss.system.file("STAN0078_OUTPUT2.sav"))
p_c_dyad <- data.frame(p_c_dyad)
# summary statistics#######################
dim(p_c_dyad)
# random exchangeable dyad ordering
draws = sample(rep(c("parents", "kids"), nrow(p_c_dyad)), nrow(p_c_dyad), replace = T)
for (i in 1:nrow(p_c_dyad)) {
  if (draws[i] == "parents") 
    exchangeable_intergenerational_row_1 = p_c_dyad[i, unique(c(1:12, grep("parents", colnames(p_c_dyad))))] else (exchangeable_intergenerational_row_1 = p_c_dyad[i, unique(c(1:12, grep("kids", colnames(p_c_dyad))))])
  if (draws[i] == "parents") 
    exchangeable_intergenerational_row_2 = p_c_dyad[i, unique(c(1:12, grep("kids", colnames(p_c_dyad))))] else (exchangeable_intergenerational_row_2 = p_c_dyad[i, unique(c(1:12, grep("parents", colnames(p_c_dyad))))])
  if (draws[i] == "parents") {
    exchangeable_intergenerational_row_1 = exchangeable_intergenerational_row_1[, gsub("_parents", 
                                                                                       "", colnames(exchangeable_intergenerational_row_1)) %in% gsub("_kids", "", colnames(exchangeable_intergenerational_row_2))]
    exchangeable_intergenerational_row_2 = exchangeable_intergenerational_row_2[, gsub("_kids", "", 
                                                                                       colnames(exchangeable_intergenerational_row_2)) %in% gsub("_parents", "", colnames(exchangeable_intergenerational_row_1))]
  }
  if (draws[i] == "kids") {
    exchangeable_intergenerational_row_1 = exchangeable_intergenerational_row_1[, gsub("_kids", "", 
                                                                                       colnames(exchangeable_intergenerational_row_1)) %in% gsub("_parents", "", colnames(exchangeable_intergenerational_row_2))]
    exchangeable_intergenerational_row_2 = exchangeable_intergenerational_row_2[, gsub("_parents", 
                                                                                       "", colnames(exchangeable_intergenerational_row_2)) %in% gsub("_kids", "", colnames(exchangeable_intergenerational_row_1))]
  }
  colnames(exchangeable_intergenerational_row_1) <- gsub("parents", "p1", colnames(exchangeable_intergenerational_row_1))
  colnames(exchangeable_intergenerational_row_1) <- gsub("kids", "p1", colnames(exchangeable_intergenerational_row_1))
  colnames(exchangeable_intergenerational_row_2) <- gsub("parents", "p2", colnames(exchangeable_intergenerational_row_2))
  colnames(exchangeable_intergenerational_row_2) <- gsub("kids", "p2", colnames(exchangeable_intergenerational_row_2))
  colnames(exchangeable_intergenerational_row_1)[1:12] <- paste0(colnames(exchangeable_intergenerational_row_1)[1:12], 
                                                                 "_p1")
  colnames(exchangeable_intergenerational_row_2)[1:12] <- paste0(colnames(exchangeable_intergenerational_row_2)[1:12], 
                                                                 "_p2")
  if (i == 1) 
    exchangeable_intergenerational_row_p1 = exchangeable_intergenerational_row_1 else (exchangeable_intergenerational_row_p1 = rbind(exchangeable_intergenerational_row_1, exchangeable_intergenerational_row_p1))
  if (i == 1) 
    exchangeable_intergenerational_row_p2 = exchangeable_intergenerational_row_2 else (exchangeable_intergenerational_row_p2 = rbind(exchangeable_intergenerational_row_2, exchangeable_intergenerational_row_p2))
}
p_c_dyad = cbind(exchangeable_intergenerational_row_p1, exchangeable_intergenerational_row_p2)
# ideology and pid
p_c_dyad$Party_p2 = recode(as.numeric(p_c_dyad$pid7_p2), "8=NA")
p_c_dyad$Party_p1 = recode(as.numeric(p_c_dyad$pid7_p1), "8=NA")
polychor(p_c_dyad$Party_p1,p_c_dyad$Party_p2)
p_c_dyad$Party_f_simple <- recode(as.numeric(p_c_dyad$pid3_p1), "2=3;3=2;4=NA;5=NA")
p_c_dyad$Party_k_simple <- recode(as.numeric(p_c_dyad$pid3_p2), "2=3;3=2;4=NA;5=NA")
polychor(p_c_dyad$Party_k_simple,p_c_dyad$Party_f_simple)
# ideology
p_c_dyad$Ideology_p1 = recode(as.numeric(p_c_dyad$stan019_p1), "8=NA")
p_c_dyad$Ideology_p2 = recode(as.numeric(p_c_dyad$stan019_p2), "8=NA")
polychor(p_c_dyad$Ideology_p1,p_c_dyad$Ideology_p2)
# immigrants
p_c_dyad$immigrants_p2 <- recode(as.numeric(p_c_dyad$stan024_p2), "2=3;3=2")
p_c_dyad$immigrants_p1 <- recode(as.numeric(p_c_dyad$stan024_p1), "2=3;3=2;4=NA")
mean_issues_intergenerational_1 = polychor(p_c_dyad$immigrants_p2, p_c_dyad$immigrants_p1)
# gvt. regulation of business
p_c_dyad$regulation_p2 <- recode(as.numeric(p_c_dyad$stan025_p2), "2=3;3=2;4=NA")
p_c_dyad$regulation_p1 <- recode(as.numeric(p_c_dyad$stan025_p1), "2=3;3=2;4=NA")
mean_issues_intergenerational_2 = polychor(p_c_dyad$regulation_p2, p_c_dyad$regulation_p1)
# income inequality
p_c_dyad$income_p2 <- recode(as.numeric(p_c_dyad$stan028_p2), "2=3;3=2")
p_c_dyad$income_p1 <- recode(as.numeric(p_c_dyad$stan028_p1), "2=3;3=2")
mean_issues_intergenerational_3 = polychor(p_c_dyad$income_p2, p_c_dyad$income_p1)
# healthcare, no opinion as have no effect
p_c_dyad$healthcare_p2 <- recode(as.numeric(p_c_dyad$stan030_p2), "2=3;3=2;4=2")
p_c_dyad$healthcare_p1 <- recode(as.numeric(p_c_dyad$stan030_p1), "2=3;3=2;4=2")
mean_issues_intergenerational_4 = polychor(p_c_dyad$healthcare_p2, p_c_dyad$healthcare_p1)
# abortion
p_c_dyad$abortion_p2 <- as.numeric(p_c_dyad$stan031_p2)
p_c_dyad$abortion_p1 <- recode(as.numeric(p_c_dyad$stan031_p1), "1=3;2=1;3=2")
mean_issues_intergenerational_5 = polychor(p_c_dyad$abortion_p2, p_c_dyad$abortion_p1)
# troops for Isis
p_c_dyad$groundtroops_p2 <- recode(as.numeric(p_c_dyad$stan032_p2), "1=3;2=1;3=2")
p_c_dyad$groundtroops_p1 <- recode(as.numeric(p_c_dyad$stan032_p1), "1=3;2=1;3=2")
mean_issues_intergenerational_6 = polychor(p_c_dyad$groundtroops_p2, p_c_dyad$groundtroops_p1)
# marijuana
p_c_dyad$marijuana_p2 <- as.numeric(p_c_dyad$stan033_p2)
p_c_dyad$marijuana_p1 <- as.numeric(p_c_dyad$stan033_p1)
mean_issues_intergenerational_7 = polychor(p_c_dyad$marijuana_p2, p_c_dyad$marijuana_p1)
# gay marriage
p_c_dyad$marriage_p2 <- as.numeric(p_c_dyad$stan034_p2)
p_c_dyad$marriage_p1 <- recode(as.numeric(p_c_dyad$stan034_p1), "2=3;3=2")
mean_issues_intergenerational_8 = polychor(p_c_dyad$marriage_p1, p_c_dyad$marriage_p2)
mean_issues_intergenerational = mean(c(mean_issues_intergenerational_1, mean_issues_intergenerational_2, 
                                       mean_issues_intergenerational_3, mean_issues_intergenerational_4, mean_issues_intergenerational_5, 
                                       mean_issues_intergenerational_6, mean_issues_intergenerational_7, mean_issues_intergenerational_8))
# latent
p_c_dyad$latent_ideology_p2 <- fscores(mirt(p_c_dyad[, c("immigrants_p2", "regulation_p2", "healthcare_p2", 
                                                         "income_p2", "abortion_p2", "groundtroops_p2", "marijuana_p2", "marriage_p2")], 1, itemtype = "graded"), 
                                       full.scores = T, scores.only = T)
p_c_dyad$latent_ideology_p1 <- fscores(mirt(p_c_dyad[, c("immigrants_p1", "regulation_p1", "healthcare_p1", 
                                                         "income_p1", "abortion_p1", "groundtroops_p1", "marijuana_p1", "marriage_p1")], 1, itemtype = "graded"), 
                                       full.scores = T, scores.only = T)
cor.test(p_c_dyad$latent_ideology_p2, p_c_dyad$latent_ideology_p1)
############### thermoeter scores Democrat
p_c_dyad$dem_feeling_p2 <- 100 - (as.numeric(p_c_dyad$stan004_p2) - 1)
p_c_dyad$dem_feeling_p1 <- 100 - (as.numeric(p_c_dyad$stan004_p1) - 1)
feeling_mean_intergenerational_1 = round(cor(p_c_dyad$dem_feeling_p2, p_c_dyad$dem_feeling_p1, use = "complete.obs"), 
                                         2)
p_c_dyad$dem_feeling_ordinal_p2 <- NA
p_c_dyad$dem_feeling_ordinal_p2[p_c_dyad$dem_feeling_p2 > 80] <- 5
p_c_dyad$dem_feeling_ordinal_p2[p_c_dyad$dem_feeling_p2 <= 80 & p_c_dyad$dem_feeling_p2 > 60] <- 4
p_c_dyad$dem_feeling_ordinal_p2[p_c_dyad$dem_feeling_p2 <= 60 & p_c_dyad$dem_feeling_p2 > 40] <- 3
p_c_dyad$dem_feeling_ordinal_p2[p_c_dyad$dem_feeling_p2 <= 40 & p_c_dyad$dem_feeling_p2 > 20] <- 2
p_c_dyad$dem_feeling_ordinal_p2[p_c_dyad$dem_feeling_p2 <= 20] <- 1
p_c_dyad$dem_feeling_ordinal_p1 <- NA
p_c_dyad$dem_feeling_ordinal_p1[p_c_dyad$dem_feeling_p1 > 80] <- 5
p_c_dyad$dem_feeling_ordinal_p1[p_c_dyad$dem_feeling_p1 <= 80 & p_c_dyad$dem_feeling_p1 > 60] <- 4
p_c_dyad$dem_feeling_ordinal_p1[p_c_dyad$dem_feeling_p1 <= 60 & p_c_dyad$dem_feeling_p1 > 40] <- 3
p_c_dyad$dem_feeling_ordinal_p1[p_c_dyad$dem_feeling_p1 <= 40 & p_c_dyad$dem_feeling_p1 > 20] <- 2
p_c_dyad$dem_feeling_ordinal_p1[p_c_dyad$dem_feeling_p1 <= 20] <- 1
# Republican
p_c_dyad$rep_feeling_p2 <- (as.numeric(p_c_dyad$stan005_p2) - 1)
p_c_dyad$rep_feeling_p1 <- (as.numeric(p_c_dyad$stan005_p1) - 1)
feeling_mean_intergenerational_2 = round(cor(p_c_dyad$rep_feeling_p2, p_c_dyad$rep_feeling_p1, use = "complete.obs"), 
                                         2)
p_c_dyad$rep_feeling_ordinal_p2 <- NA
p_c_dyad$rep_feeling_ordinal_p2[p_c_dyad$rep_feeling_p2 > 80] <- 5
p_c_dyad$rep_feeling_ordinal_p2[p_c_dyad$rep_feeling_p2 <= 80 & p_c_dyad$rep_feeling_p2 > 60] <- 4
p_c_dyad$rep_feeling_ordinal_p2[p_c_dyad$rep_feeling_p2 <= 60 & p_c_dyad$rep_feeling_p2 > 40] <- 3
p_c_dyad$rep_feeling_ordinal_p2[p_c_dyad$rep_feeling_p2 <= 40 & p_c_dyad$rep_feeling_p2 > 20] <- 2
p_c_dyad$rep_feeling_ordinal_p2[p_c_dyad$rep_feeling_p2 <= 20] <- 1
p_c_dyad$rep_feeling_ordinal_p1 <- NA
p_c_dyad$rep_feeling_ordinal_p1[p_c_dyad$rep_feeling_p1 > 80] <- 5
p_c_dyad$rep_feeling_ordinal_p1[p_c_dyad$rep_feeling_p1 <= 80 & p_c_dyad$rep_feeling_p1 > 60] <- 4
p_c_dyad$rep_feeling_ordinal_p1[p_c_dyad$rep_feeling_p1 <= 60 & p_c_dyad$rep_feeling_p1 > 40] <- 3
p_c_dyad$rep_feeling_ordinal_p1[p_c_dyad$rep_feeling_p1 <= 40 & p_c_dyad$rep_feeling_p1 > 20] <- 2
p_c_dyad$rep_feeling_ordinal_p1[p_c_dyad$rep_feeling_p1 <= 20] <- 1
# Tea party
p_c_dyad$tea_feeling_p2 <- (as.numeric(p_c_dyad$stan006_p2) - 1)
p_c_dyad$tea_feeling_p1 <- (as.numeric(p_c_dyad$stan006_p1) - 1)
feeling_mean_intergenerational_3 = round(cor(p_c_dyad$tea_feeling_p2, p_c_dyad$tea_feeling_p1, use = "complete.obs"), 
                                         2)
p_c_dyad$tea_feeling_ordinal_p2 <- NA
p_c_dyad$tea_feeling_ordinal_p2[p_c_dyad$tea_feeling_p2 > 80] <- 5
p_c_dyad$tea_feeling_ordinal_p2[p_c_dyad$tea_feeling_p2 <= 80 & p_c_dyad$tea_feeling_p2 > 60] <- 4
p_c_dyad$tea_feeling_ordinal_p2[p_c_dyad$tea_feeling_p2 <= 60 & p_c_dyad$tea_feeling_p2 > 40] <- 3
p_c_dyad$tea_feeling_ordinal_p2[p_c_dyad$tea_feeling_p2 <= 40 & p_c_dyad$tea_feeling_p2 > 20] <- 2
p_c_dyad$tea_feeling_ordinal_p2[p_c_dyad$tea_feeling_p2 <= 20] <- 1
p_c_dyad$tea_feeling_ordinal_p1 <- NA
p_c_dyad$tea_feeling_ordinal_p1[p_c_dyad$tea_feeling_p1 > 80] <- 5
p_c_dyad$tea_feeling_ordinal_p1[p_c_dyad$tea_feeling_p1 <= 80 & p_c_dyad$tea_feeling_p1 > 60] <- 4
p_c_dyad$tea_feeling_ordinal_p1[p_c_dyad$tea_feeling_p1 <= 60 & p_c_dyad$tea_feeling_p1 > 40] <- 3
p_c_dyad$tea_feeling_ordinal_p1[p_c_dyad$tea_feeling_p1 <= 40 & p_c_dyad$tea_feeling_p1 > 20] <- 2
p_c_dyad$tea_feeling_ordinal_p1[p_c_dyad$tea_feeling_p1 <= 20] <- 1
# Obama
p_c_dyad$obama_feeling_p2 <- 100 - (as.numeric(p_c_dyad$stan007_p2) - 1)
p_c_dyad$obama_feeling_p1 <- 100 - (as.numeric(p_c_dyad$stan007_p1) - 1)
feeling_mean_intergenerational_4 = round(cor(p_c_dyad$obama_feeling_p2, p_c_dyad$obama_feeling_p1, use = "complete.obs"), 
                                         2)
p_c_dyad$obama_feeling_ordinal_p2 <- NA
p_c_dyad$obama_feeling_ordinal_p2[p_c_dyad$obama_feeling_p2 > 80] <- 5
p_c_dyad$obama_feeling_ordinal_p2[p_c_dyad$obama_feeling_p2 <= 80 & p_c_dyad$obama_feeling_p2 > 60] <- 4
p_c_dyad$obama_feeling_ordinal_p2[p_c_dyad$obama_feeling_p2 <= 60 & p_c_dyad$obama_feeling_p2 > 40] <- 3
p_c_dyad$obama_feeling_ordinal_p2[p_c_dyad$obama_feeling_p2 <= 40 & p_c_dyad$obama_feeling_p2 > 20] <- 2
p_c_dyad$obama_feeling_ordinal_p2[p_c_dyad$obama_feeling_p2 <= 20] <- 1
p_c_dyad$obama_feeling_ordinal_p1 <- NA
p_c_dyad$obama_feeling_ordinal_p1[p_c_dyad$obama_feeling_p1 > 80] <- 5
p_c_dyad$obama_feeling_ordinal_p1[p_c_dyad$obama_feeling_p1 <= 80 & p_c_dyad$obama_feeling_p1 > 60] <- 4
p_c_dyad$obama_feeling_ordinal_p1[p_c_dyad$obama_feeling_p1 <= 60 & p_c_dyad$obama_feeling_p1 > 40] <- 3
p_c_dyad$obama_feeling_ordinal_p1[p_c_dyad$obama_feeling_p1 <= 40 & p_c_dyad$obama_feeling_p1 > 20] <- 2
p_c_dyad$obama_feeling_ordinal_p1[p_c_dyad$obama_feeling_p1 <= 20] <- 1
# Clinton
p_c_dyad$clinton_feeling_p2 <- 100 - (as.numeric(p_c_dyad$stan008_p2) - 1)
p_c_dyad$clinton_feeling_p1 <- 100 - (as.numeric(p_c_dyad$stan008_p1) - 1)
feeling_mean_intergenerational_5 = round(cor(p_c_dyad$clinton_feeling_p1, p_c_dyad$clinton_feeling_p2, 
                                             use = "complete.obs"), 2)
p_c_dyad$clinton_feeling_ordinal_p2 <- NA
p_c_dyad$clinton_feeling_ordinal_p2[p_c_dyad$clinton_feeling_p2 > 80] <- 5
p_c_dyad$clinton_feeling_ordinal_p2[p_c_dyad$clinton_feeling_p2 <= 80 & p_c_dyad$clinton_feeling_p2 > 
                                      60] <- 4
p_c_dyad$clinton_feeling_ordinal_p2[p_c_dyad$clinton_feeling_p2 <= 60 & p_c_dyad$clinton_feeling_p2 > 
                                      40] <- 3
p_c_dyad$clinton_feeling_ordinal_p2[p_c_dyad$clinton_feeling_p2 <= 40 & p_c_dyad$clinton_feeling_p2 > 
                                      20] <- 2
p_c_dyad$clinton_feeling_ordinal_p2[p_c_dyad$clinton_feeling_p2 <= 20] <- 1
p_c_dyad$clinton_feeling_ordinal_p1 <- NA
p_c_dyad$clinton_feeling_ordinal_p1[p_c_dyad$clinton_feeling_p1 > 80] <- 5
p_c_dyad$clinton_feeling_ordinal_p1[p_c_dyad$clinton_feeling_p1 <= 80 & p_c_dyad$clinton_feeling_p1 > 
                                      60] <- 4
p_c_dyad$clinton_feeling_ordinal_p1[p_c_dyad$clinton_feeling_p1 <= 60 & p_c_dyad$clinton_feeling_p1 > 
                                      40] <- 3
p_c_dyad$clinton_feeling_ordinal_p1[p_c_dyad$clinton_feeling_p1 <= 40 & p_c_dyad$clinton_feeling_p1 > 
                                      20] <- 2
p_c_dyad$clinton_feeling_ordinal_p1[p_c_dyad$clinton_feeling_p1 <= 20] <- 1
# Paul
p_c_dyad$paul_feeling_p2 <- (as.numeric(p_c_dyad$stan009_p2) - 1)
p_c_dyad$paul_feeling_p1 <- (as.numeric(p_c_dyad$stan009_p1) - 1)
feeling_mean_intergenerational_6 = round(cor(p_c_dyad$paul_feeling_p2, p_c_dyad$paul_feeling_p1, use = "complete.obs"), 
                                         2)
p_c_dyad$paul_feeling_ordinal_p2 <- NA
p_c_dyad$paul_feeling_ordinal_p2[p_c_dyad$paul_feeling_p2 > 80] <- 5
p_c_dyad$paul_feeling_ordinal_p2[p_c_dyad$paul_feeling_p2 <= 80 & p_c_dyad$paul_feeling_p2 > 60] <- 4
p_c_dyad$paul_feeling_ordinal_p2[p_c_dyad$paul_feeling_p2 <= 60 & p_c_dyad$paul_feeling_p2 > 40] <- 3
p_c_dyad$paul_feeling_ordinal_p2[p_c_dyad$paul_feeling_p2 <= 40 & p_c_dyad$paul_feeling_p2 > 20] <- 2
p_c_dyad$paul_feeling_ordinal_p2[p_c_dyad$paul_feeling_p2 <= 20] <- 1
p_c_dyad$paul_feeling_ordinal_p1 <- NA
p_c_dyad$paul_feeling_ordinal_p1[p_c_dyad$paul_feeling_p1 > 80] <- 5
p_c_dyad$paul_feeling_ordinal_p1[p_c_dyad$paul_feeling_p1 <= 80 & p_c_dyad$paul_feeling_p1 > 60] <- 4
p_c_dyad$paul_feeling_ordinal_p1[p_c_dyad$paul_feeling_p1 <= 60 & p_c_dyad$paul_feeling_p1 > 40] <- 3
p_c_dyad$paul_feeling_ordinal_p1[p_c_dyad$paul_feeling_p1 <= 40 & p_c_dyad$paul_feeling_p1 > 20] <- 2
p_c_dyad$paul_feeling_ordinal_p1[p_c_dyad$paul_feeling_p1 <= 20] <- 1
# Bush
p_c_dyad$bush_feeling_p2 <- (as.numeric(p_c_dyad$stan010_p2) - 1)
p_c_dyad$bush_feeling_p1 <- (as.numeric(p_c_dyad$stan010_p1) - 1)
feeling_mean_intergenerational_7 = round(cor(p_c_dyad$bush_feeling_p2, p_c_dyad$bush_feeling_p1, use = "complete.obs"), 
                                         2)
p_c_dyad$bush_feeling_ordinal_p2 <- NA
p_c_dyad$bush_feeling_ordinal_p2[p_c_dyad$bush_feeling_p2 > 80] <- 5
p_c_dyad$bush_feeling_ordinal_p2[p_c_dyad$bush_feeling_p2 <= 80 & p_c_dyad$bush_feeling_p2 > 60] <- 4
p_c_dyad$bush_feeling_ordinal_p2[p_c_dyad$bush_feeling_p2 <= 60 & p_c_dyad$bush_feeling_p2 > 40] <- 3
p_c_dyad$bush_feeling_ordinal_p2[p_c_dyad$bush_feeling_p2 <= 40 & p_c_dyad$bush_feeling_p2 > 20] <- 2
p_c_dyad$bush_feeling_ordinal_p2[p_c_dyad$bush_feeling_p2 <= 20] <- 1
p_c_dyad$bush_feeling_ordinal_p1 <- NA
p_c_dyad$bush_feeling_ordinal_p1[p_c_dyad$bush_feeling_p1 > 80] <- 5
p_c_dyad$bush_feeling_ordinal_p1[p_c_dyad$bush_feeling_p1 <= 80 & p_c_dyad$bush_feeling_p1 > 60] <- 4
p_c_dyad$bush_feeling_ordinal_p1[p_c_dyad$bush_feeling_p1 <= 60 & p_c_dyad$bush_feeling_p1 > 40] <- 3
p_c_dyad$bush_feeling_ordinal_p1[p_c_dyad$bush_feeling_p1 <= 40 & p_c_dyad$bush_feeling_p1 > 20] <- 2
p_c_dyad$bush_feeling_ordinal_p1[p_c_dyad$bush_feeling_p1 <= 20] <- 1
# Cruz
p_c_dyad$cruz_feeling_p2 <- (as.numeric(p_c_dyad$stan011_p2) - 1)
p_c_dyad$cruz_feeling_p1 <- (as.numeric(p_c_dyad$stan011_p1) - 1)
feeling_mean_intergenerational_8 = round(cor(p_c_dyad$cruz_feeling_p2, p_c_dyad$cruz_feeling_p1, use = "complete.obs"), 
                                         2)
p_c_dyad$cruz_feeling_ordinal_p2 <- NA
p_c_dyad$cruz_feeling_ordinal_p2[p_c_dyad$cruz_feeling_p2 > 80] <- 5
p_c_dyad$cruz_feeling_ordinal_p2[p_c_dyad$cruz_feeling_p2 <= 80 & p_c_dyad$cruz_feeling_p2 > 60] <- 4
p_c_dyad$cruz_feeling_ordinal_p2[p_c_dyad$cruz_feeling_p2 <= 60 & p_c_dyad$cruz_feeling_p2 > 40] <- 3
p_c_dyad$cruz_feeling_ordinal_p2[p_c_dyad$cruz_feeling_p2 <= 40 & p_c_dyad$cruz_feeling_p2 > 20] <- 2
p_c_dyad$cruz_feeling_ordinal_p2[p_c_dyad$cruz_feeling_p2 <= 20] <- 1
p_c_dyad$cruz_feeling_ordinal_p1 <- NA
p_c_dyad$cruz_feeling_ordinal_p1[p_c_dyad$cruz_feeling_p1 > 80] <- 5
p_c_dyad$cruz_feeling_ordinal_p1[p_c_dyad$cruz_feeling_p1 <= 80 & p_c_dyad$cruz_feeling_p1 > 60] <- 4
p_c_dyad$cruz_feeling_ordinal_p1[p_c_dyad$cruz_feeling_p1 <= 60 & p_c_dyad$cruz_feeling_p1 > 40] <- 3
p_c_dyad$cruz_feeling_ordinal_p1[p_c_dyad$cruz_feeling_p1 <= 40 & p_c_dyad$cruz_feeling_p1 > 20] <- 2
p_c_dyad$cruz_feeling_ordinal_p1[p_c_dyad$cruz_feeling_p1 <= 20] <- 1
# Trump
p_c_dyad$trump_feeling_p2 <- (as.numeric(p_c_dyad$stan011b_p2) - 1)
p_c_dyad$trump_feeling_p1 <- (as.numeric(p_c_dyad$stan011b_p1) - 1)
feeling_mean_intergenerational_9 = round(cor(p_c_dyad$trump_feeling_p2, p_c_dyad$trump_feeling_p1, use = "complete.obs"), 
                                         2)
p_c_dyad$trump_feeling_ordinal_p2 <- NA
p_c_dyad$trump_feeling_ordinal_p2[p_c_dyad$trump_feeling_p2 > 80] <- 5
p_c_dyad$trump_feeling_ordinal_p2[p_c_dyad$trump_feeling_p2 <= 80 & p_c_dyad$trump_feeling_p2 > 60] <- 4
p_c_dyad$trump_feeling_ordinal_p2[p_c_dyad$trump_feeling_p2 <= 60 & p_c_dyad$trump_feeling_p2 > 40] <- 3
p_c_dyad$trump_feeling_ordinal_p2[p_c_dyad$trump_feeling_p2 <= 40 & p_c_dyad$trump_feeling_p2 > 20] <- 2
p_c_dyad$trump_feeling_ordinal_p2[p_c_dyad$trump_feeling_p2 <= 20] <- 1
p_c_dyad$trump_feeling_ordinal_p1 <- NA
p_c_dyad$trump_feeling_ordinal_p1[p_c_dyad$trump_feeling_p1 > 80] <- 5
p_c_dyad$trump_feeling_ordinal_p1[p_c_dyad$trump_feeling_p1 <= 80 & p_c_dyad$trump_feeling_p1 > 60] <- 4
p_c_dyad$trump_feeling_ordinal_p1[p_c_dyad$trump_feeling_p1 <= 60 & p_c_dyad$trump_feeling_p1 > 40] <- 3
p_c_dyad$trump_feeling_ordinal_p1[p_c_dyad$trump_feeling_p1 <= 40 & p_c_dyad$trump_feeling_p1 > 20] <- 2
p_c_dyad$trump_feeling_ordinal_p1[p_c_dyad$trump_feeling_p1 <= 20] <- 1
# NRA
p_c_dyad$nra_feeling_p2 <- (as.numeric(p_c_dyad$stan012_p2) - 1)
p_c_dyad$nra_feeling_p1 <- (as.numeric(p_c_dyad$stan012_p1) - 1)
feeling_mean_intergenerational_10 = round(cor(p_c_dyad$nra_feeling_p2, p_c_dyad$nra_feeling_p1, use = "complete.obs"), 
                                          2)
p_c_dyad$nra_feeling_ordinal_p2 <- NA
p_c_dyad$nra_feeling_ordinal_p2[p_c_dyad$nra_feeling_p2 > 80] <- 5
p_c_dyad$nra_feeling_ordinal_p2[p_c_dyad$nra_feeling_p2 <= 80 & p_c_dyad$nra_feeling_p2 > 60] <- 4
p_c_dyad$nra_feeling_ordinal_p2[p_c_dyad$nra_feeling_p2 <= 60 & p_c_dyad$nra_feeling_p2 > 40] <- 3
p_c_dyad$nra_feeling_ordinal_p2[p_c_dyad$nra_feeling_p2 <= 40 & p_c_dyad$nra_feeling_p2 > 20] <- 2
p_c_dyad$nra_feeling_ordinal_p2[p_c_dyad$nra_feeling_p2 <= 20] <- 1
p_c_dyad$nra_feeling_ordinal_p1 <- NA
p_c_dyad$nra_feeling_ordinal_p1[p_c_dyad$nra_feeling_p1 > 80] <- 5
p_c_dyad$nra_feeling_ordinal_p1[p_c_dyad$nra_feeling_p1 <= 80 & p_c_dyad$nra_feeling_p1 > 60] <- 4
p_c_dyad$nra_feeling_ordinal_p1[p_c_dyad$nra_feeling_p1 <= 60 & p_c_dyad$nra_feeling_p1 > 40] <- 3
p_c_dyad$nra_feeling_ordinal_p1[p_c_dyad$nra_feeling_p1 <= 40 & p_c_dyad$nra_feeling_p1 > 20] <- 2
p_c_dyad$nra_feeling_ordinal_p1[p_c_dyad$nra_feeling_p1 <= 20] <- 1
# gay
p_c_dyad$gay_feeling_p2 <- 100 - (as.numeric(p_c_dyad$stan013_p2) - 1)
p_c_dyad$gay_feeling_p1 <- 100 - (as.numeric(p_c_dyad$stan013_p1) - 1)
feeling_mean_intergenerational_11 = round(cor(p_c_dyad$gay_feeling_p2, p_c_dyad$gay_feeling_p1, use = "complete.obs"), 
                                          2)
p_c_dyad$gay_feeling_ordinal_p2 <- NA
p_c_dyad$gay_feeling_ordinal_p2[p_c_dyad$gay_feeling_p2 > 80] <- 5
p_c_dyad$gay_feeling_ordinal_p2[p_c_dyad$gay_feeling_p2 <= 80 & p_c_dyad$gay_feeling_p2 > 60] <- 4
p_c_dyad$gay_feeling_ordinal_p2[p_c_dyad$gay_feeling_p2 <= 60 & p_c_dyad$gay_feeling_p2 > 40] <- 3
p_c_dyad$gay_feeling_ordinal_p2[p_c_dyad$gay_feeling_p2 <= 40 & p_c_dyad$gay_feeling_p2 > 20] <- 2
p_c_dyad$gay_feeling_ordinal_p2[p_c_dyad$gay_feeling_p2 <= 20] <- 1
p_c_dyad$gay_feeling_ordinal_p1 <- NA
p_c_dyad$gay_feeling_ordinal_p1[p_c_dyad$gay_feeling_p1 > 80] <- 5
p_c_dyad$gay_feeling_ordinal_p1[p_c_dyad$gay_feeling_p1 <= 80 & p_c_dyad$gay_feeling_p1 > 60] <- 4
p_c_dyad$gay_feeling_ordinal_p1[p_c_dyad$gay_feeling_p1 <= 60 & p_c_dyad$gay_feeling_p1 > 40] <- 3
p_c_dyad$gay_feeling_ordinal_p1[p_c_dyad$gay_feeling_p1 <= 40 & p_c_dyad$gay_feeling_p1 > 20] <- 2
p_c_dyad$gay_feeling_ordinal_p1[p_c_dyad$gay_feeling_p1 <= 20] <- 1
# Christians
p_c_dyad$christians_feeling_p2 <- (as.numeric(p_c_dyad$stan014_p2) - 1)
p_c_dyad$christians_feeling_p1 <- (as.numeric(p_c_dyad$stan014_p1) - 1)
feeling_mean_intergenerational_12 = round(cor(p_c_dyad$christians_feeling_p2, p_c_dyad$christians_feeling_p1, 
                                              use = "complete.obs"), 2)
p_c_dyad$christians_feeling_ordinal_p2 <- NA
p_c_dyad$christians_feeling_ordinal_p2[p_c_dyad$christians_feeling_p2 > 80] <- 5
p_c_dyad$christians_feeling_ordinal_p2[p_c_dyad$christians_feeling_p2 <= 80 & p_c_dyad$christians_feeling_p2 > 
                                         60] <- 4
p_c_dyad$christians_feeling_ordinal_p2[p_c_dyad$christians_feeling_p2 <= 60 & p_c_dyad$christians_feeling_p2 > 
                                         40] <- 3
p_c_dyad$christians_feeling_ordinal_p2[p_c_dyad$christians_feeling_p2 <= 40 & p_c_dyad$christians_feeling_p2 > 
                                         20] <- 2
p_c_dyad$christians_feeling_ordinal_p2[p_c_dyad$christians_feeling_p2 <= 20] <- 1
p_c_dyad$christians_feeling_ordinal_p1 <- NA
p_c_dyad$christians_feeling_ordinal_p1[p_c_dyad$christians_feeling_p1 > 80] <- 5
p_c_dyad$christians_feeling_ordinal_p1[p_c_dyad$christians_feeling_p1 <= 80 & p_c_dyad$christians_feeling_p1 > 
                                         60] <- 4
p_c_dyad$christians_feeling_ordinal_p1[p_c_dyad$christians_feeling_p1 <= 60 & p_c_dyad$christians_feeling_p1 > 
                                         40] <- 3
p_c_dyad$christians_feeling_ordinal_p1[p_c_dyad$christians_feeling_p1 <= 40 & p_c_dyad$christians_feeling_p1 > 
                                         20] <- 2
p_c_dyad$christians_feeling_ordinal_p1[p_c_dyad$christians_feeling_p1 <= 20] <- 1
# Atheists
p_c_dyad$atheists_feeling_p2 <- 100 - (as.numeric(p_c_dyad$stan015_p2) - 1)
p_c_dyad$atheists_feeling_p1 <- 100 - (as.numeric(p_c_dyad$stan015_p1) - 1)
feeling_mean_intergenerational_13 = round(cor(p_c_dyad$atheists_feeling_p2, p_c_dyad$atheists_feeling_p1, 
                                              use = "complete.obs"), 2)
p_c_dyad$atheists_feeling_ordinal_p2 <- NA
p_c_dyad$atheists_feeling_ordinal_p2[p_c_dyad$atheists_feeling_p2 > 80] <- 5
p_c_dyad$atheists_feeling_ordinal_p2[p_c_dyad$atheists_feeling_p2 <= 80 & p_c_dyad$atheists_feeling_p2 > 
                                       60] <- 4
p_c_dyad$atheists_feeling_ordinal_p2[p_c_dyad$atheists_feeling_p2 <= 60 & p_c_dyad$atheists_feeling_p2 > 
                                       40] <- 3
p_c_dyad$atheists_feeling_ordinal_p2[p_c_dyad$atheists_feeling_p2 <= 40 & p_c_dyad$atheists_feeling_p2 > 
                                       20] <- 2
p_c_dyad$atheists_feeling_ordinal_p2[p_c_dyad$atheists_feeling_p2 <= 20] <- 1
p_c_dyad$atheists_feeling_ordinal_p1 <- NA
p_c_dyad$atheists_feeling_ordinal_p1[p_c_dyad$atheists_feeling_p1 > 80] <- 5
p_c_dyad$atheists_feeling_ordinal_p1[p_c_dyad$atheists_feeling_p1 <= 80 & p_c_dyad$atheists_feeling_p1 > 
                                       60] <- 4
p_c_dyad$atheists_feeling_ordinal_p1[p_c_dyad$atheists_feeling_p1 <= 60 & p_c_dyad$atheists_feeling_p1 > 
                                       40] <- 3
p_c_dyad$atheists_feeling_ordinal_p1[p_c_dyad$atheists_feeling_p1 <= 40 & p_c_dyad$atheists_feeling_p1 > 
                                       20] <- 2
p_c_dyad$atheists_feeling_ordinal_p1[p_c_dyad$atheists_feeling_p1 <= 20] <- 1
feeling_mean_intergenerational = mean(c(feeling_mean_intergenerational_1, feeling_mean_intergenerational_2, 
                                        feeling_mean_intergenerational_3, feeling_mean_intergenerational_4, feeling_mean_intergenerational_5, 
                                        feeling_mean_intergenerational_9, feeling_mean_intergenerational_10, feeling_mean_intergenerational_11, 
                                        feeling_mean_intergenerational_12, feeling_mean_intergenerational_13))
# groups, porbably better to do in terms of quintiles
p_c_dyad$latent_partisan_attitudes_p2 <- with(p_c_dyad, fscores(mirt(cbind(Ideology_p2, Party_p2, tea_feeling_ordinal_p2, 
                                                                           nra_feeling_ordinal_p2, gay_feeling_ordinal_p2, christians_feeling_ordinal_p2, atheists_feeling_ordinal_p2, 
                                                                           dem_feeling_ordinal_p2, rep_feeling_ordinal_p2, obama_feeling_ordinal_p2, clinton_feeling_ordinal_p2, 
                                                                           paul_feeling_ordinal_p2, bush_feeling_ordinal_p2, cruz_feeling_ordinal_p2, trump_feeling_ordinal_p2), 
                                                                     1, itemtype = "graded"), full.scores = TRUE, scores.only = TRUE))
p_c_dyad$latent_partisan_attitudes_p1 <- with(p_c_dyad, fscores(mirt(cbind(Ideology_p1, Party_p1, tea_feeling_ordinal_p1, 
                                                                           nra_feeling_ordinal_p1, gay_feeling_ordinal_p1, christians_feeling_ordinal_p1, atheists_feeling_ordinal_p1, 
                                                                           dem_feeling_ordinal_p1, rep_feeling_ordinal_p1, obama_feeling_ordinal_p1, clinton_feeling_ordinal_p1, 
                                                                           paul_feeling_ordinal_p1, bush_feeling_ordinal_p1, cruz_feeling_ordinal_p1, trump_feeling_ordinal_p1), 
                                                                     1, itemtype = "graded"), full.scores = TRUE, scores.only = TRUE))
cor.test(p_c_dyad$latent_partisan_attitudes_p1, p_c_dyad$latent_partisan_attitudes_p2)
# include party, ideology non-policy domains
#### Personality of Children Respect for elders/independence
p_c_dyad$independence_p2 <- as.numeric(p_c_dyad$stan020_p2)
p_c_dyad$independence_p1 <- as.numeric(p_c_dyad$stan020_p1)
mean_personality_intergenerational_1 = polychor(p_c_dyad$independence_p1, p_c_dyad$independence_p2)
# obedience/self-reliance
p_c_dyad$obedience_p2 <- recode(as.numeric(p_c_dyad$stan021_p2), "1=2;2=1")
p_c_dyad$obedience_p1 <- recode(as.numeric(p_c_dyad$stan021_p1), "1=2;2=1")
mean_personality_intergenerational_2 = polychor(p_c_dyad$obedience_p2, p_c_dyad$obedience_p1)
# good manners/curiosity
p_c_dyad$manners_p2 <- as.numeric(p_c_dyad$stan022_p2)
p_c_dyad$manners_p1 <- as.numeric(p_c_dyad$stan022_p1)
mean_personality_intergenerational_3 = polychor(p_c_dyad$manners_p2, p_c_dyad$manners_p1)
# considerate vs. well-behaved
p_c_dyad$behaved_p2 <- recode(as.numeric(p_c_dyad$stan023_p2), "1=2;2=1")
p_c_dyad$behaved_p1 <- recode(as.numeric(p_c_dyad$stan023_p1), "1=2;2=1")
mean_personality_intergenerational_4 = polychor(p_c_dyad$behaved_p2, p_c_dyad$behaved_p1)
mean_personality_intergenerational = mean(c(mean_personality_intergenerational_1, mean_personality_intergenerational_2, 
                                            mean_personality_intergenerational_3, mean_personality_intergenerational_4))
# latent
p_c_dyad$latent_personality_p2 <- with(p_c_dyad, fscores(mirt(cbind(independence_p2, obedience_p2, manners_p2, 
                                                                    behaved_p2), 1, itemtype = "graded"), full.scores = T, scores.only = T))
p_c_dyad$latent_personality_p1 <- with(p_c_dyad, fscores(mirt(cbind(independence_p1, obedience_p1, manners_p1, 
                                                                    behaved_p1), 1, itemtype = "graded"), full.scores = T, scores.only = T))
cor.test(p_c_dyad$latent_personality_p2, p_c_dyad$latent_personality_p1)
# religious service attendance
mean_religion_intergenerational_1 = polychor(recode(as.numeric(p_c_dyad$stan096_p2), "1=5;2=4;3=3;4=2;5=1"), 
                                             recode(as.numeric(p_c_dyad$stan096_p1), "1=5;2=4;3=3;4=2;5=1"))
mean_religion_intergenerational_2 = polychor(recode(as.numeric(p_c_dyad$stan097_p2), "1=5;2=4;3=3;4=2;5=1"), 
                                             recode(as.numeric(p_c_dyad$stan097_p1), "1=5;2=4;3=3;4=2;5=1"))
mean_religion_intergenerational = mean(c(mean_religion_intergenerational_1, mean_religion_intergenerational_2))
p_c_dyad$religious_attendance_p2 <- fscores(mirt(cbind(recode(as.numeric(p_c_dyad$stan096_p2), "1=5;2=4;3=3;4=2;5=1"), 
                                                       recode(as.numeric(p_c_dyad$stan097_p2), "1=5;2=4;3=3;4=2;5=1"), p_c_dyad$christians_p1eeling_ordinal_p2, 
                                                       p_c_dyad$atheists_p1eeling_ordinal_p2), 1, itemtype = "graded"), full.scores = TRUE, scores.only = TRUE)
p_c_dyad$religious_attendance_p1 <- fscores(mirt(cbind(recode(as.numeric(p_c_dyad$stan096_p1), "1=5;2=4;3=3;4=2;5=1"), 
                                                       recode(as.numeric(p_c_dyad$stan097_p1), "1=5;2=4;3=3;4=2;5=1"), p_c_dyad$christians_p1eeling_ordinal_p1, 
                                                       p_c_dyad$atheists_p1eeling_ordinal_p1), 1, itemtype = "graded"), full.scores = TRUE, scores.only = TRUE)
cor.test(p_c_dyad$religious_attendance_p1, p_c_dyad$religious_attendance_p2)
#save(p_c_dyad, file = "exchangeable_dyad_intergenerational.RData")