############# read in data#############################################
#setwd("C:\\Users\\tobia\\Dropbox\\Partisan Homogeneity Project\\Jennings Data")
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
library(boot)
library(psych)
library(SDMTools)
library(weights)
# read in data##############################
set.seed(2141)
#couple_1965 <- read.dta("07286-0001-Data.dta")
############################################################### Split in student-parent and parent-parent-student dyads#######################################
############################################################### single parent-student data
student_parent <- couple_1965[as.numeric(couple_1965$V318) != 9 & !duplicated(couple_1965$V319) & !duplicated(couple_1965$V319, 
                                                                                                              fromLast = T), ]
# pair
# how to proceed creating table
summary(couple_1965$V318)
summary(couple_1965$V2)
# based on parents
couple_1965 = subset(couple_1965, V318 == 2 | V318 == 3)
subset_df <- data.frame(subset_fathers = which(duplicated(couple_1965$V319)), subset_mothers = which(duplicated(couple_1965$V319, 
                                                                                                                fromLast = T)))
subset_df$assign_mothers = sample(rep(c("p1", "p2"), nrow(subset_df)), nrow(subset_df), replace = T)
subset_df$assign_fathers = ifelse(subset_df$assign_mothers == "p1", "p2", "p1")
subset_couple_1965_p1 <- couple_1965[c(subset_df$subset_fathers[subset_df$assign_fathers == "p1"], subset_df$subset_mothers[subset_df$assign_mothers == 
                                                                                                                              "p1"]), ]
subset_couple_1965_p2 <- couple_1965[c(subset_df$subset_mothers[subset_df$assign_mothers == "p2"], subset_df$subset_fathers[subset_df$assign_fathers == 
                                                                                                                              "p2"]), ]
# pull out fatthers from subset_couple_1965_p2 and vice-versa, must be erroneous coding!
# now split cbind parents responses
which(student_parent$V599 %in% subset_couple_1965_p2$V599)
which(colnames(couple_1965) == "V2")  #col 2 flag if mother, child or father, not working
which(colnames(couple_1965) == "V3")  #col 3 identifier matching for parents
which(colnames(couple_1965) == "V318")  #col 382; mother-child-father-identifier
which(colnames(couple_1965) == "V596")  #col 700; Ideology
which(colnames(couple_1965) == "V322")  #col 386; weight for Data divided by parental sex
which(colnames(couple_1965) == "V149")  #col 202; PID (7 scale_) student
which(colnames(couple_1965) == "V382")  #col 466; PID (7 scale_) parents
# check if all match
which(subset_couple_1965_p1$V319 != subset_couple_1965_p2$V319)  #identifier
which(subset_couple_1965_p1$V318 == subset_couple_1965_p2$V318)  #gender
which(subset_couple_1965_p1$V322 != subset_couple_1965_p2$V322)  #weight parent corss-section
which(subset_couple_1965_p1$V321 != subset_couple_1965_p2$V321)  #weight
###################################### party
subset_couple_1965_p2$Party <- recode(subset_couple_1965_p2$V382, "0=NA;9=NA")
subset_couple_1965_p1$Party <- recode(subset_couple_1965_p1$V382, "0=NA;9=NA")
polychoric(cbind(subset_couple_1965_p2$Party, subset_couple_1965_p1$Party), weight = subset_couple_1965_p1$V322)$rho
polychoric(cbind(car::recode(subset_couple_1965_p2$Party,"1=1;2=1;3=1;4=2;5=3;6=3;7=3"), car::recode(subset_couple_1965_p1$Party,"1=1;2=1;3=1;4=2;5=3;6=3;7=3")), weight = subset_couple_1965_p1$V322)$rho


subset_couple_1965_p1$Party_kid <- recode(subset_couple_1965_p1$V149, "11=1;12=2;13=3;14=4;15=5;16=6;17=7;20=NA;30=NA;40=NA;88=NA;99=NA")
subset_couple_1965_p2$Party_kid <- recode(subset_couple_1965_p2$V149, "11=1;12=2;13=3;14=4;15=5;16=6;17=7;20=NA;30=NA;40=NA;88=NA;99=NA")
student_parent$Party_kid <- recode(student_parent$V149, "11=1;12=2;13=3;14=4;15=5;16=6;17=7;20=NA;30=NA;40=NA;88=NA;99=NA")
# party_id
student_parent$Party <- recode(student_parent$V382, "0=NA;8=NA;9=NA")
student_parent$Party_kid <- recode(student_parent$V149, "11=1;12=2;13=3;14=4;15=5;16=6;17=7;20=NA;30=NA;40=NA;88=NA;99=NA")
# Intergenerational
new_data <- data.frame(rbind(as.matrix(student_parent[, c("Party_kid", "Party", "V321")]), as.matrix(subset_couple_1965_p2[, 
                                                                                                                           c("Party_kid", "Party", "V321")]), as.matrix(subset_couple_1965_p1[, c("Party_kid", "Party", "V321")])))
new_data$party_kid <- sample(c(0, 1), nrow(new_data), replace = T)
new_data$party_kid_new <- ifelse(new_data$party_kid == 1, new_data$Party_kid, new_data$Party)
new_data$party_parent_new <- ifelse(new_data$party_kid == 1, new_data$Party, new_data$Party_kid)
polychoric(cbind(new_data[, 5], new_data[, 6]), weight = new_data[, 3])$rho
polychoric(cbind(car::recode(new_data[, 5],"1=1;2=1;3=1;4=2;5=3;6=3;7=3"), car::recode(new_data[, 6],"1=1;2=1;3=1;4=2;5=3;6=3;7=3")), weight = new_data[, 3])$rho

########## isues##################################################### how to code issue-agreement? get issue
########## agreement, 1=liberal, 2=middle; 3=conservative school prayer
subset_couple_1965_p1$school_prayer <- recode(subset_couple_1965_p1$V365, "0=2;1=3;3=2;5=1;8=2;9=NA")
subset_couple_1965_p1$school_prayer_kid <- recode(subset_couple_1965_p1$V200, "0=2;1=3;3=2;5=1;8=2;9=NA")
subset_couple_1965_p2$school_prayer <- recode(subset_couple_1965_p2$V365, "0=2;1=3;3=2;5=1;8=2;9=NA")
subset_couple_1965_p2$school_prayer_kid <- recode(subset_couple_1965_p2$V200, "0=2;1=3;3=2;5=1;8=2;9=NA")
# recode single-parent data
student_parent$school_prayer_family <- recode(student_parent$V365, "0=2;1=3;3=2;5=1;8=2;9=NA")
student_parent$school_prayer_kid <- recode(student_parent$V200, "0=2;1=3;3=2;5=1;8=2;9=NA")
mean_issues_1 = polychoric(cbind(subset_couple_1965_p2$school_prayer, subset_couple_1965_p1$school_prayer), 
                           weight = subset_couple_1965_p1$V322)$rho[1, 2]
# Free speech against church
subset_couple_1965_p1$speech_church <- recode(subset_couple_1965_p1$V356, "1=1;3=2;5=3;8=2;9=NA")
subset_couple_1965_p2$speech_church <- recode(subset_couple_1965_p2$V356, "1=1;3=2;5=3;8=2;9=NA")
subset_couple_1965_p1$speech_church_kid <- recode(subset_couple_1965_p1$V121, "1=1;3=2;5=3;8=2;9=NA")
subset_couple_1965_p2$speech_church_kid <- recode(subset_couple_1965_p2$V121, "1=1;3=2;5=3;8=2;9=NA")
# recode single-parent data
student_parent$speech_church_family <- recode(student_parent$V356, "1=1;3=2;5=3;8=2;9=NA")
student_parent$speech_church_kid <- recode(student_parent$V121, "1=1;3=2;5=3;8=2;9=NA")
mean_issues_2 = polychoric(cbind(subset_couple_1965_p2$speech_church, subset_couple_1965_p1$speech_church), 
                           weight = subset_couple_1965_p1$V322)$rho[1, 2]
# allow communist in office
subset_couple_1965_p1$communist <- recode(subset_couple_1965_p1$V358, "1=1;3=2;5=3;6=NA;8=2;9=NA")
subset_couple_1965_p2$communist <- recode(subset_couple_1965_p2$V358, "1=1;3=2;5=3;6=NA;8=2;9=NA")
subset_couple_1965_p1$communist_kid <- recode(subset_couple_1965_p1$V122, "1=1;3=2;5=3;6=NA;8=2;9=NA")
subset_couple_1965_p2$communist_kid <- recode(subset_couple_1965_p2$V122, "1=1;3=2;5=3;6=NA;8=2;9=NA")
# recode single-parent data
student_parent$communist_family <- recode(student_parent$V358, "1=1;3=2;5=3;6=NA;8=2;9=NA")
student_parent$communist_kid <- recode(student_parent$V122, "1=1;3=2;5=3;6=NA;8=2;9=NA")
mean_issues_3 = polychoric(cbind(subset_couple_1965_p2$communist, subset_couple_1965_p1$communist), weight = subset_couple_1965_p1$V322)$rho[1, 
                                                                                                                                             2]
# residential integration, no kid data
subset_couple_1965_p1$integration <- recode(subset_couple_1965_p1$V366, "1=1;5=2;8=3;9=NA")
subset_couple_1965_p2$integration <- recode(subset_couple_1965_p2$V366, "1=1;5=2;8=3;9=NA")
subset_couple_1965_p2$integration_kid <- recode(subset_couple_1965_p2$V201, "1=1;5=2;8=3;9=NA")
subset_couple_1965_p1$integration_kid <- recode(subset_couple_1965_p1$V201, "1=1;5=2;8=3;9=NA")
# recode single-parent data
student_parent$integration_family <- recode(student_parent$V366, "1=1;5=2;8=3;9=NA")
student_parent$integration_kid <- recode(student_parent$V201, "1=1;5=2;8=3;9=NA")
mean_issues_4 = polychoric(cbind(subset_couple_1965_p2$integration, subset_couple_1965_p1$integration), 
                           weight = subset_couple_1965_p1$V322)$rho[1, 2]
mean_issues = mean(c(mean_issues_1, mean_issues_2, mean_issues_3, mean_issues_4))
# 0.44 thermometer######################################################
# Partisan Attitudes
subset_couple_1965_p1$feeling_unions <- 100 - subset_couple_1965_p1$V436
subset_couple_1965_p1$feeling_unions_k <- 100 - subset_couple_1965_p1$V192
student_parent$feeling_unions_k <- 100 - student_parent$V192
student_parent$feeling_unions_f <- 100 - student_parent$V436
subset_couple_1965_p2$feeling_unions <- 100 - subset_couple_1965_p2$V436
subset_couple_1965_p2$feeling_unions_k <- 100 - subset_couple_1965_p2$V192
mean_feelings_1 = corr(cbind(subset_couple_1965_p1$feeling_unions, subset_couple_1965_p2$feeling_unions), 
                       w = subset_couple_1965_p1$V322)
### big business
subset_couple_1965_p1$feeling_big_business <- subset_couple_1965_p1$V439
subset_couple_1965_p1$feeling_big_business_k <- subset_couple_1965_p1$V195
subset_couple_1965_p2$feeling_big_business <- subset_couple_1965_p2$V439
subset_couple_1965_p2$feeling_big_business_k <- subset_couple_1965_p2$V195
student_parent$feeling_big_business_k <- student_parent$V195
student_parent$feeling_big_business_f <- student_parent$V439
mean_feelings_2 = corr(cbind(subset_couple_1965_p1$feeling_big_business, subset_couple_1965_p2$feeling_big_business), 
                       w = subset_couple_1965_p1$V322)
# 0.10/0.02 correlation with Party
# Catholics
subset_couple_1965_p1$feeling_catholics <- 100 - subset_couple_1965_p1$V438
subset_couple_1965_p1$feeling_catholics_k <- 100 - subset_couple_1965_p1$V194
student_parent$feeling_catholics_k <- 100 - student_parent$V194
student_parent$feeling_catholics_f <- 100 - student_parent$V438
subset_couple_1965_p2$feeling_catholics <- 100 - subset_couple_1965_p2$V438
subset_couple_1965_p2$feeling_catholics_k <- 100 - subset_couple_1965_p2$V194
mean_feelings_3 = corr(cbind(subset_couple_1965_p1$feeling_catholics, subset_couple_1965_p2$feeling_catholics), 
                       w = subset_couple_1965_p1$V322)
# 0.06/0.03 correlation with Party
# jews
subset_couple_1965_p1$feeling_jews <- 100 - subset_couple_1965_p1$V440
subset_couple_1965_p1$feeling_jews_k <- 100 - subset_couple_1965_p1$V196
student_parent$feeling_jews_k <- 100 - student_parent$V196
student_parent$feeling_jews_f <- 100 - student_parent$V440
subset_couple_1965_p2$feeling_jews <- 100 - subset_couple_1965_p2$V440
subset_couple_1965_p2$feeling_jews_k <- 100 - subset_couple_1965_p2$V196
mean_feelings_4 = corr(cbind(subset_couple_1965_p1$feeling_jews, subset_couple_1965_p2$feeling_jews), 
                       w = subset_couple_1965_p1$V322)
#-0.02/-0.01 correlation with Party
# whites
subset_couple_1965_p1$feeling_whites <- 100 - subset_couple_1965_p1$V441
subset_couple_1965_p1$feeling_whites_k <- 100 - subset_couple_1965_p1$V197
student_parent$feeling_whites_k <- 100 - student_parent$V197
student_parent$feeling_whites_f <- 100 - student_parent$V441
subset_couple_1965_p2$feeling_whites <- 100 - subset_couple_1965_p2$V441
subset_couple_1965_p2$feeling_whites_k <- 100 - subset_couple_1965_p2$V197
#-0.01/-0.03 correlation with Party
mean_feelings_5 = corr(cbind(subset_couple_1965_p1$feeling_whites, subset_couple_1965_p2$feeling_whites), 
                       w = subset_couple_1965_p1$V322)
mean_feelings = mean(c(mean_feelings_1, mean_feelings_2, mean_feelings_3, mean_feelings_4, mean_feelings_5))
################################## Intergenerational Data############################
####### ISUES############################## school Prayer create new data for student parent comparison
new_data <- data.frame(rbind(as.matrix(subset_couple_1965_p2[, c("school_prayer", "school_prayer_kid", 
                                                                 "V321")]), as.matrix(subset_couple_1965_p1[, c("school_prayer", "school_prayer_kid", "V321")]), as.matrix(student_parent[, 
                                                                                                                                                                                          c("school_prayer_family", "school_prayer_kid", "V321")])))
# results for inter-generational( families with (both two parent and one parent families))
new_data$party_kid <- sample(c(0, 1), nrow(new_data), replace = T)
new_data$school_prayer_kid_new <- ifelse(new_data$party_kid == 1, new_data$school_prayer_kid, new_data$school_prayer)
new_data$school_prayer_parent_new <- ifelse(new_data$party_kid == 1, new_data$school_prayer, new_data$school_prayer_kid)
mean_issues_intergenerational_1 = polychoric(cbind(new_data[, 5], new_data[, 6]), weight = new_data[, 
                                                                                                    3])$rho[1, 2]
# 0.34
# Church Speech create new data for student parent comparison
new_data <- data.frame(rbind(as.matrix(subset_couple_1965_p2[, c("speech_church", "speech_church_kid", 
                                                                 "V321")]), as.matrix(subset_couple_1965_p1[, c("speech_church", "speech_church_kid", "V321")]), as.matrix(student_parent[, 
                                                                                                                                                                                          c("speech_church_family", "speech_church_kid", "V321")])))
## intergeneration
new_data$party_kid <- sample(c(0, 1), nrow(new_data), replace = T)
new_data$speech_church_kid_new <- ifelse(new_data$party_kid == 1, new_data$speech_church_kid, new_data$speech_church)
new_data$speech_church_parent_new <- ifelse(new_data$party_kid == 1, new_data$speech_church, new_data$speech_church_kid)
mean_issues_intergenerational_2 = polychoric(cbind(new_data[, 5], new_data[, 6]), weight = new_data[, 
                                                                                                    3])$rho[1, 2]
# .08 Communist
# create new data for student parent comparison
new_data <- data.frame(rbind(as.matrix(subset_couple_1965_p2[, c("communist", "communist_kid", "V321")]), 
                             as.matrix(subset_couple_1965_p1[, c("communist", "communist_kid", "V321")]), as.matrix(student_parent[, 
                                                                                                                                   c("communist_family", "communist_kid", "V321")])))
new_data$party_kid <- sample(c(0, 1), nrow(new_data), replace = T)
new_data$communist_kid_new <- ifelse(new_data$party_kid == 1, new_data$communist_kid, new_data$communist)
new_data$communist_parent_new <- ifelse(new_data$party_kid == 1, new_data$communist, new_data$communist_kid)
mean_issues_intergenerational_3 = polychoric(cbind(new_data[, 5], new_data[, 6]), weight = new_data[, 
                                                                                                    3])$rho[1, 2]
# inegration create new data for student parent comparison
new_data <- data.frame(rbind(as.matrix(subset_couple_1965_p2[, c("integration", "integration_kid", "V321")]), 
                             as.matrix(subset_couple_1965_p1[, c("integration", "integration_kid", "V321")]), as.matrix(student_parent[, 
                                                                                                                                       c("integration_family", "integration_kid", "V321")])))
# results for inter-generational( families with (both two parent and one parent families))
new_data$party_kid <- sample(c(0, 1), nrow(new_data), replace = T)
new_data$integration_kid_new <- ifelse(new_data$party_kid == 1, new_data$integration_kid, new_data$integration)
new_data$integration_parent_new <- ifelse(new_data$party_kid == 1, new_data$integration, new_data$integration_kid)
mean_issues_intergenerational_4 = polychoric(cbind(new_data[, 5], new_data[, 6]), weight = new_data[, 
                                                                                                    3])$rho[1, 2]
mean_issues_intergenerational = mean(c(mean_issues_intergenerational_1, mean_issues_intergenerational_2, 
                                       mean_issues_intergenerational_3, mean_issues_intergenerational_4))
# 0.20
##### Thermometers###################
# Unions
new_data <- data.frame(rbind(as.matrix(subset_couple_1965_p2[, c("feeling_unions", "feeling_unions_k", 
                                                                 "V321")]), as.matrix(subset_couple_1965_p1[, c("feeling_unions", "feeling_unions_k", "V321")]), as.matrix(student_parent[, 
                                                                                                                                                                                          c("feeling_unions_f", "feeling_unions_k", "V321")])))
new_data$party_kid <- sample(c(0, 1), nrow(new_data), replace = T)
new_data$feeling_unions_kid_new <- ifelse(new_data$party_kid == 1, new_data$feeling_unions_k, new_data$feeling_unions)
new_data$feeling_unions_parent_new <- ifelse(new_data$party_kid == 1, new_data$feeling_unions, new_data$feeling_unions_k)
mean_feelings_intergenerational_1 = corr(cbind(new_data[, 5], new_data[, 6]), w = new_data[, 3])
# Big Business
new_data <- data.frame(rbind(as.matrix(subset_couple_1965_p2[, c("feeling_big_business", "feeling_big_business_k", 
                                                                 "V321")]), as.matrix(subset_couple_1965_p1[, c("feeling_big_business", "feeling_big_business_k", 
                                                                                                                "V321")]), as.matrix(student_parent[, c("feeling_big_business_f", "feeling_big_business_k", "V321")])))
new_data$party_kid <- sample(c(0, 1), nrow(new_data), replace = T)
new_data$feeling_big_business_kid_new <- ifelse(new_data$party_kid == 1, new_data$feeling_big_business_k, 
                                                new_data$feeling_big_business)
new_data$feeling_big_business_parent_new <- ifelse(new_data$party_kid == 1, new_data$feeling_big_business, 
                                                   new_data$feeling_big_business_k)
mean_feelings_intergenerational_2 = corr(cbind(new_data[, 5], new_data[, 6]), w = new_data[, 3])
# catholics
new_data <- data.frame(rbind(as.matrix(subset_couple_1965_p2[, c("feeling_catholics", "feeling_catholics_k", 
                                                                 "V321")]), as.matrix(subset_couple_1965_p1[, c("feeling_catholics", "feeling_catholics_k", "V321")]), 
                             as.matrix(student_parent[, c("feeling_catholics_f", "feeling_catholics_k", "V321")])))
new_data$party_kid <- sample(c(0, 1), nrow(new_data), replace = T)
new_data$feeling_catholics_kid_new <- ifelse(new_data$party_kid == 1, new_data$feeling_catholics_k, new_data$feeling_catholics)
new_data$feeling_catholics_new <- ifelse(new_data$party_kid == 1, new_data$feeling_catholics, new_data$feeling_catholics_k)
mean_feelings_intergenerational_3 = corr(cbind(new_data[, 5], new_data[, 6]), w = new_data[, 3])
# Jews
new_data <- data.frame(rbind(as.matrix(subset_couple_1965_p2[, c("feeling_jews", "feeling_jews_k", "V321")]), 
                             as.matrix(subset_couple_1965_p1[, c("feeling_jews", "feeling_jews_k", "V321")]), as.matrix(student_parent[, 
                                                                                                                                       c("feeling_jews_f", "feeling_jews_k", "V321")])))
new_data$party_kid <- sample(c(0, 1), nrow(new_data), replace = T)
new_data$feeling_jews_kid_new <- ifelse(new_data$party_kid == 1, new_data$feeling_jews_k, new_data$feeling_jews)
new_data$feeling_jews_parent_new <- ifelse(new_data$party_kid == 1, new_data$feeling_jews, new_data$feeling_jews_k)
mean_feelings_intergenerational_4 = corr(cbind(new_data[, 5], new_data[, 6]), w = new_data[, 3])
# whites
new_data <- data.frame(rbind(as.matrix(subset_couple_1965_p2[, c("feeling_whites", "feeling_whites_k", 
                                                                 "V321")]), as.matrix(subset_couple_1965_p1[, c("feeling_whites", "feeling_whites_k", "V321")]), as.matrix(student_parent[, 
                                                                                                                                                                                          c("feeling_whites_f", "feeling_whites_k", "V321")])))
new_data$party_kid <- sample(c(0, 1), nrow(new_data), replace = T)
new_data$feeling_whites_kid_new <- ifelse(new_data$party_kid == 1, new_data$feeling_whites, new_data$feeling_whites_k)
new_data$feeling_whites_new <- ifelse(new_data$party_kid == 1, new_data$feeling_whites_k, new_data$feeling_whites)
mean_feelings_intergenerational_5 = corr(cbind(new_data[, 5], new_data[, 6]), w = new_data[, 3])
mean_feelings_intergenerational = mean(c(mean_feelings_intergenerational_1, mean_feelings_intergenerational_2, 
                                         mean_feelings_intergenerational_3, mean_feelings_intergenerational_4, mean_feelings_intergenerational_5))
############## religion######################
# V266 church attendance, V558 church attendance
subset_couple_1965_p1$religious_attendance <- recode(subset_couple_1965_p1$V558, "1=4;2=3;3=2;4=1;9=NA;0=NA")
subset_couple_1965_p1$religious_attendance_kids <- recode(subset_couple_1965_p1$V266, "1=4;2=3;3=2;4=1;9=NA;0=NA")
subset_couple_1965_p2$religious_attendance <- recode(subset_couple_1965_p2$V558, "1=4;2=3;3=2;4=1;9=NA;0=NA")
subset_couple_1965_p2$religious_attendance_kids <- recode(subset_couple_1965_p2$V266, "1=4;2=3;3=2;4=1;9=NA;0=NA")
student_parent$religious_attendance_parent <- recode(student_parent$V558, "1=4;2=3;3=2;4=1;9=NA;0=NA")
student_parent$religious_attendance_kid <- recode(student_parent$V266, "1=4;2=3;3=2;4=1;9=NA;0=NA")
mean_religion_1 = polychoric(cbind(subset_couple_1965_p2$religious_attendance, subset_couple_1965_p1$religious_attendance), 
                             weight = subset_couple_1965_p1$V322)$rho[1, 2]
# now intergenerational
new_data <- data.frame(rbind(as.matrix(subset_couple_1965_p2[, c("religious_attendance", "religious_attendance_kids", 
                                                                 "V321")]), as.matrix(subset_couple_1965_p1[, c("religious_attendance", "religious_attendance_kids", 
                                                                                                                "V321")]), as.matrix(student_parent[, c("religious_attendance_parent", "religious_attendance_kid", 
                                                                                                                                                        "V321")])))
new_data$party_kid <- sample(c(0, 1), nrow(new_data), replace = T)
new_data$religious_attendance_kid_new <- ifelse(new_data$party_kid == 1, new_data$religious_attendance_kids, 
                                                new_data$religious_attendance)
new_data$religious_attendance_parent_new <- ifelse(new_data$party_kid == 1, new_data$religious_attendance, 
                                                   new_data$religious_attendance_kids)
mean_religion_intergenerational_1 = polychoric(cbind(new_data[, 5], new_data[, 6]), weight = new_data[, 
                                                                                                      3])$rho[1, 2]
# bible for word V267 bible word for word kids, V559 bible word for word adults
subset_couple_1965_p1$bible <- recode(subset_couple_1965_p1$V559, "1=4;2=3;3=2;4=1;9=NA;8=NA")
subset_couple_1965_p1$bible_kids <- recode(subset_couple_1965_p1$V267, "1=4;2=3;3=2;4=1;9=NA;8=NA")
subset_couple_1965_p2$bible <- recode(subset_couple_1965_p2$V559, "1=4;2=3;3=2;4=1;9=NA;8=NA")
subset_couple_1965_p2$bible_kids <- recode(subset_couple_1965_p2$V267, "1=4;2=3;3=2;4=1;9=NA;8=NA")
student_parent$bible_parent <- recode(student_parent$V559, "1=4;2=3;3=2;4=1;9=NA;8=NA")
student_parent$bible_kid <- recode(student_parent$V267, "1=4;2=3;3=2;4=1;9=NA;8=NA")
mean_religion_2 = polychoric(cbind(subset_couple_1965_p1$bible, subset_couple_1965_p2$bible))$rho[1, 
                                                                                                  2]
# intergenerational
new_data <- data.frame(rbind(as.matrix(subset_couple_1965_p2[, c("bible", "bible_kids", "V321")]), as.matrix(subset_couple_1965_p1[, 
                                                                                                                                   c("bible", "bible_kids", "V321")]), as.matrix(student_parent[, c("bible_parent", "bible_kid", "V321")])))
new_data$party_kid <- sample(c(0, 1), nrow(new_data), replace = T)
new_data$bible_kid_new <- ifelse(new_data$party_kid == 1, new_data$bible_kids, new_data$bible)
new_data$bible_parent_new <- ifelse(new_data$party_kid == 1, new_data$bible, new_data$bible_kids)
mean_religion_intergenerational_2 = polychoric(cbind(new_data[, 5], new_data[, 6]), weight = new_data[, 
                                                                                                      3])$rho[1, 2]
mean_religon <- mean(c(mean_religion_1, mean_religion_2))
mean_religon_intergenerational <- mean(c(mean_religion_intergenerational_2, mean_religion_intergenerational_1))
########################################################################################################### 
# latent Models
############################################################################################### 
################################################## scaled ideology and party################################ rescale for thermometer for models
subset_couple_1965_p1$feeling_big_business <- ifelse(subset_couple_1965_p1$feeling_big_business <= 20, 
                                                     1, ifelse(subset_couple_1965_p1$feeling_big_business <= 40, 2, ifelse(subset_couple_1965_p1$feeling_big_business <= 
                                                                                                                             60, 3, ifelse(subset_couple_1965_p1$feeling_big_business <= 80, 4, 5))))
subset_couple_1965_p2$feeling_big_business <- ifelse(subset_couple_1965_p2$feeling_big_business <= 20, 
                                                     1, ifelse(subset_couple_1965_p2$feeling_big_business <= 40, 2, ifelse(subset_couple_1965_p2$feeling_big_business <= 
                                                                                                                             60, 3, ifelse(subset_couple_1965_p2$feeling_big_business <= 80, 4, 5))))
subset_couple_1965_p1$feeling_big_business_k <- ifelse(subset_couple_1965_p1$feeling_big_business_k <= 
                                                         20, 1, ifelse(subset_couple_1965_p1$feeling_big_business_k <= 40, 2, ifelse(subset_couple_1965_p1$feeling_big_business_k <= 
                                                                                                                                       60, 3, ifelse(subset_couple_1965_p1$feeling_big_business_k <= 80, 4, 5))))
subset_couple_1965_p2$feeling_big_business_k <- ifelse(subset_couple_1965_p2$feeling_big_business_k <= 
                                                         20, 1, ifelse(subset_couple_1965_p2$feeling_big_business_k <= 40, 2, ifelse(subset_couple_1965_p2$feeling_big_business_k <= 
                                                                                                                                       60, 3, ifelse(subset_couple_1965_p2$feeling_big_business_k <= 80, 4, 5))))
student_parent$feeling_big_business_k <- ifelse(student_parent$feeling_big_business_k <= 20, 1, ifelse(student_parent$feeling_big_business_k <= 
                                                                                                         40, 2, ifelse(student_parent$feeling_big_business_k <= 60, 3, ifelse(student_parent$feeling_big_business_k <= 
                                                                                                                                                                                80, 4, 5))))
student_parent$feeling_big_business_f <- ifelse(student_parent$feeling_big_business_f <= 20, 1, ifelse(student_parent$feeling_big_business_f <= 
                                                                                                         40, 2, ifelse(student_parent$feeling_big_business_f <= 60, 3, ifelse(student_parent$feeling_big_business_f <= 
                                                                                                                                                                                80, 4, 5))))
subset_couple_1965_p1$feeling_unions <- ifelse(subset_couple_1965_p1$feeling_unions <= 20, 1, ifelse(subset_couple_1965_p1$feeling_unions <= 
                                                                                                       40, 2, ifelse(subset_couple_1965_p1$feeling_unions <= 60, 3, ifelse(subset_couple_1965_p1$feeling_unions <= 
                                                                                                                                                                             80, 4, 5))))
subset_couple_1965_p2$feeling_unions <- ifelse(subset_couple_1965_p2$feeling_unions <= 20, 1, ifelse(subset_couple_1965_p2$feeling_unions <= 
                                                                                                       40, 2, ifelse(subset_couple_1965_p2$feeling_unions <= 60, 3, ifelse(subset_couple_1965_p2$feeling_unions <= 
                                                                                                                                                                             80, 4, 5))))
subset_couple_1965_p1$feeling_unions_k <- ifelse(subset_couple_1965_p1$feeling_unions_k <= 20, 1, ifelse(subset_couple_1965_p1$feeling_unions_k <= 
                                                                                                           40, 2, ifelse(subset_couple_1965_p1$feeling_unions_k <= 60, 3, ifelse(subset_couple_1965_p1$feeling_unions_k <= 
                                                                                                                                                                                   80, 4, 5))))
subset_couple_1965_p2$feeling_unions_k <- ifelse(subset_couple_1965_p2$feeling_unions_k <= 20, 1, ifelse(subset_couple_1965_p2$feeling_unions_k <= 
                                                                                                           40, 2, ifelse(subset_couple_1965_p2$feeling_unions_k <= 60, 3, ifelse(subset_couple_1965_p2$feeling_unions_k <= 
                                                                                                                                                                                   80, 4, 5))))
student_parent$feeling_unions_k <- ifelse(student_parent$feeling_unions_k <= 20, 1, ifelse(student_parent$feeling_unions_k <= 
                                                                                             40, 2, ifelse(student_parent$feeling_unions_k <= 60, 3, ifelse(student_parent$feeling_unions_k <= 
                                                                                                                                                              80, 4, 5))))
student_parent$feeling_unions_f <- ifelse(student_parent$feeling_unions_f <= 20, 1, ifelse(student_parent$feeling_unions_f <= 
                                                                                             40, 2, ifelse(student_parent$feeling_unions_f <= 60, 3, ifelse(student_parent$feeling_unions_f <= 
                                                                                                                                                              80, 4, 5))))
###################### 
subset_couple_1965_p1$feeling_jews <- ifelse(subset_couple_1965_p1$feeling_jews <= 20, 1, ifelse(subset_couple_1965_p1$feeling_jews <= 
                                                                                                   40, 2, ifelse(subset_couple_1965_p1$feeling_jews <= 60, 3, ifelse(subset_couple_1965_p1$feeling_jews <= 
                                                                                                                                                                       80, 4, 5))))
subset_couple_1965_p2$feeling_jews <- ifelse(subset_couple_1965_p2$feeling_jews <= 20, 1, ifelse(subset_couple_1965_p2$feeling_jews <= 
                                                                                                   40, 2, ifelse(subset_couple_1965_p2$feeling_jews <= 60, 3, ifelse(subset_couple_1965_p2$feeling_jews <= 
                                                                                                                                                                       80, 4, 5))))
subset_couple_1965_p1$feeling_jews_k <- ifelse(subset_couple_1965_p1$feeling_jews_k <= 20, 1, ifelse(subset_couple_1965_p1$feeling_jews_k <= 
                                                                                                       40, 2, ifelse(subset_couple_1965_p1$feeling_jews_k <= 60, 3, ifelse(subset_couple_1965_p1$feeling_jews_k <= 
                                                                                                                                                                             80, 4, 5))))
subset_couple_1965_p2$feeling_jews_k <- ifelse(subset_couple_1965_p2$feeling_jews_k <= 20, 1, ifelse(subset_couple_1965_p2$feeling_jews_k <= 
                                                                                                       40, 2, ifelse(subset_couple_1965_p2$feeling_jews_k <= 60, 3, ifelse(subset_couple_1965_p2$feeling_jews_k <= 
                                                                                                                                                                             80, 4, 5))))
student_parent$feeling_jews_k <- ifelse(student_parent$feeling_jews_k <= 20, 1, ifelse(student_parent$feeling_jews_k <= 
                                                                                         40, 2, ifelse(student_parent$feeling_jews_k <= 60, 3, ifelse(student_parent$feeling_jews_k <= 80, 
                                                                                                                                                      4, 5))))
student_parent$feeling_jews_f <- ifelse(student_parent$feeling_jews_f <= 20, 1, ifelse(student_parent$feeling_jews_f <= 
                                                                                         40, 2, ifelse(student_parent$feeling_jews_f <= 60, 3, ifelse(student_parent$feeling_jews_f <= 80, 
                                                                                                                                                      4, 5))))
subset_couple_1965_p1$feeling_whites <- ifelse(subset_couple_1965_p1$feeling_whites <= 20, 1, ifelse(subset_couple_1965_p1$feeling_whites <= 
                                                                                                       40, 2, ifelse(subset_couple_1965_p1$feeling_whites <= 60, 3, ifelse(subset_couple_1965_p1$feeling_whites <= 
                                                                                                                                                                             80, 4, 5))))
subset_couple_1965_p2$feeling_whites <- ifelse(subset_couple_1965_p2$feeling_whites <= 20, 1, ifelse(subset_couple_1965_p2$feeling_whites <= 
                                                                                                       40, 2, ifelse(subset_couple_1965_p2$feeling_whites <= 60, 3, ifelse(subset_couple_1965_p2$feeling_whites <= 
                                                                                                                                                                             80, 4, 5))))
subset_couple_1965_p1$feeling_whites_k <- ifelse(subset_couple_1965_p1$feeling_whites_k <= 20, 1, ifelse(subset_couple_1965_p1$feeling_whites_k <= 
                                                                                                           40, 2, ifelse(subset_couple_1965_p1$feeling_whites_k <= 60, 3, ifelse(subset_couple_1965_p1$feeling_whites_k <= 
                                                                                                                                                                                   80, 4, 5))))
subset_couple_1965_p2$feeling_whites_k <- ifelse(subset_couple_1965_p2$feeling_whites_k <= 20, 1, ifelse(subset_couple_1965_p2$feeling_whites_k <= 
                                                                                                           40, 2, ifelse(subset_couple_1965_p2$feeling_whites_k <= 60, 3, ifelse(subset_couple_1965_p2$feeling_whites_k <= 
                                                                                                                                                                                   80, 4, 5))))
student_parent$feeling_whites_k <- ifelse(student_parent$feeling_whites_k <= 20, 1, ifelse(student_parent$feeling_whites_k <= 
                                                                                             40, 2, ifelse(student_parent$feeling_whites_k <= 60, 3, ifelse(student_parent$feeling_whites_k <= 
                                                                                                                                                              80, 4, 5))))
student_parent$feeling_whites_f <- ifelse(student_parent$feeling_whites_f <= 20, 1, ifelse(student_parent$feeling_whites_f <= 
                                                                                             40, 2, ifelse(student_parent$feeling_whites_f <= 60, 3, ifelse(student_parent$feeling_whites_f <= 
                                                                                                                                                              80, 4, 5))))
subset_couple_1965_p1$feeling_catholics <- ifelse(subset_couple_1965_p1$feeling_catholics <= 20, 1, ifelse(subset_couple_1965_p1$feeling_catholics <= 
                                                                                                             40, 2, ifelse(subset_couple_1965_p1$feeling_catholics <= 60, 3, ifelse(subset_couple_1965_p1$feeling_catholics <= 
                                                                                                                                                                                      80, 4, 5))))
subset_couple_1965_p2$feeling_catholics <- ifelse(subset_couple_1965_p2$feeling_catholics <= 20, 1, ifelse(subset_couple_1965_p2$feeling_catholics <= 
                                                                                                             40, 2, ifelse(subset_couple_1965_p2$feeling_catholics <= 60, 3, ifelse(subset_couple_1965_p2$feeling_catholics <= 
                                                                                                                                                                                      80, 4, 5))))
subset_couple_1965_p1$feeling_catholics_k <- ifelse(subset_couple_1965_p1$feeling_catholics_k <= 20, 
                                                    1, ifelse(subset_couple_1965_p1$feeling_catholics_k <= 40, 2, ifelse(subset_couple_1965_p1$feeling_catholics_k <= 
                                                                                                                           60, 3, ifelse(subset_couple_1965_p1$feeling_catholics_k <= 80, 4, 5))))
subset_couple_1965_p2$feeling_catholics_k <- ifelse(subset_couple_1965_p2$feeling_catholics_k <= 20, 
                                                    1, ifelse(subset_couple_1965_p2$feeling_catholics_k <= 40, 2, ifelse(subset_couple_1965_p2$feeling_catholics_k <= 
                                                                                                                           60, 3, ifelse(subset_couple_1965_p2$feeling_catholics_k <= 80, 4, 5))))
student_parent$feeling_catholics_k <- ifelse(student_parent$feeling_catholics_k <= 20, 1, ifelse(student_parent$feeling_catholics_k <= 
                                                                                                   40, 2, ifelse(student_parent$feeling_catholics_k <= 60, 3, ifelse(student_parent$feeling_catholics_k <= 
                                                                                                                                                                       80, 4, 5))))
student_parent$feeling_catholics_f <- ifelse(student_parent$feeling_catholics_f <= 20, 1, ifelse(student_parent$feeling_catholics_f <= 
                                                                                                   40, 2, ifelse(student_parent$feeling_catholics_f <= 60, 3, ifelse(student_parent$feeling_catholics_f <= 
                                                                                                                                                                       80, 4, 5))))
# authoritarianism
subset_couple_1965_p2$teenagers <- recode(as.numeric(subset_couple_1965_p2$V330), "5=3;8=NA;9=NA")
subset_couple_1965_p1$teenagers <- recode(as.numeric(subset_couple_1965_p1$V330), "5=3;8=NA;9=NA")
subset_couple_1965_p2$child_popular <- recode(as.numeric(subset_couple_1965_p2$V332), "0=3;6=4;8=NA;9=NA")
subset_couple_1965_p1$child_popular <- recode(as.numeric(subset_couple_1965_p1$V332), "0=3;6=4;8=NA;9=NA")
subset_couple_1965_p2$child_religious <- recode(as.numeric(subset_couple_1965_p2$V333), "0=3;6=4;8=NA;9=NA")
subset_couple_1965_p1$child_religious <- recode(as.numeric(subset_couple_1965_p1$V333), "0=3;6=4;8=NA;9=NA")
subset_couple_1965_p2$child_successfull <- recode(as.numeric(subset_couple_1965_p2$V334), "0=3;6=4;8=NA;9=NA")
subset_couple_1965_p1$child_successfull <- recode(as.numeric(subset_couple_1965_p1$V334), "0=3;6=4;8=NA;9=NA")
subset_couple_1965_p2$child_tolerant <- recode(as.numeric(subset_couple_1965_p2$V335), "0=3;6=4;8=NA;9=NA")
subset_couple_1965_p1$child_tolerant <- recode(as.numeric(subset_couple_1965_p1$V335), "0=3;6=4;8=NA;9=NA")
subset_couple_1965_p2$child_cultured <- recode(as.numeric(subset_couple_1965_p2$V336), "0=3;6=4;8=NA;9=NA")
subset_couple_1965_p1$child_cultured <- recode(as.numeric(subset_couple_1965_p1$V336), "0=3;6=4;8=NA;9=NA")
subset_couple_1965_p2$child_improve <- recode(as.numeric(subset_couple_1965_p2$V337), "0=3;6=4;8=NA;9=NA")
subset_couple_1965_p1$child_improve <- recode(as.numeric(subset_couple_1965_p1$V337), "0=3;6=4;8=NA;9=NA")

mean_authoritarianism_1 = polychoric(cbind(subset_couple_1965_p2$teenagers, subset_couple_1965_p1$teenagers), weight = subset_couple_1965_p1$V322)$rho[1,2] 
mean_authoritarianism_2 = polychoric(cbind(subset_couple_1965_p2$child_popular, subset_couple_1965_p1$child_popular), weight = subset_couple_1965_p1$V322)$rho[1,2] 
mean_authoritarianism_3 = polychoric(cbind(subset_couple_1965_p2$child_religious, subset_couple_1965_p1$child_religious), weight = subset_couple_1965_p1$V322)$rho[1,2] 
mean_authoritarianism_4 = polychoric(cbind(subset_couple_1965_p2$child_successfull, subset_couple_1965_p1$child_successfull), weight = subset_couple_1965_p1$V322)$rho[1,2] 
mean_authoritarianism_5 = polychoric(cbind(subset_couple_1965_p2$child_tolerant, subset_couple_1965_p1$child_tolerant), weight = subset_couple_1965_p1$V322)$rho[1,2] 
mean_authoritarianism_6 = polychoric(cbind(subset_couple_1965_p2$child_cultured, subset_couple_1965_p1$child_cultured), weight = subset_couple_1965_p1$V322)$rho[1,2] 
mean_authoritarianism_7 = polychoric(cbind(subset_couple_1965_p2$child_improve, subset_couple_1965_p1$child_improve), weight = subset_couple_1965_p1$V322)$rho[1,2] 



# race
subset_couple_1965_p1$race <- recode(as.numeric(subset_couple_1965_p1$V584), "1=0;2=1;8=0;9=NA")
subset_couple_1965_p2$race <- recode(as.numeric(subset_couple_1965_p2$V584), "1=0;2=1;8=0;9=NA")
subset_couple_1965_p1$race_kid <- recode(as.numeric(subset_couple_1965_p1$V297), "1=0;2=1;8=0;9=NA")
subset_couple_1965_p2$race_kid <- recode(as.numeric(subset_couple_1965_p2$V297), "1=0;2=1;8=0;9=NA")
student_parent$race_f <- recode(as.numeric(student_parent$V584), "1=0;2=1;8=0;9=NA")
student_parent$race_k <- recode(as.numeric(student_parent$V297), "1=0;2=1;8=0;9=NA")
# latent religious attendance
subset_couple_1965_p2$religious_attendance_latent <- fscores(mirt(with(subset_couple_1965_p2, cbind(bible, 
                                                                                                    religious_attendance, feeling_jews, feeling_catholics)), 1, itemtype = "graded", survey.weights = subset_couple_1965_p1$V322), 
                                                             full.scores = TRUE, scores.only = TRUE)
subset_couple_1965_p1$religious_attendance_latent <- fscores(mirt(with(subset_couple_1965_p1, cbind(bible, 
                                                                                                    religious_attendance, feeling_jews, feeling_catholics)), 1, itemtype = "graded", survey.weights = subset_couple_1965_p1$V322), 
                                                             full.scores = TRUE, scores.only = TRUE)
cor.test(subset_couple_1965_p1$religious_attendance_latent, subset_couple_1965_p2$religious_attendance_latent)
# .42 latent intergenerational
new_data <- data.frame(rbind(as.matrix(subset_couple_1965_p2[, c("religious_attendance", "bible", "religious_attendance_kids", 
                                                                 "bible_kids")]), as.matrix(subset_couple_1965_p1[, c("religious_attendance", "bible", "religious_attendance_kids", 
                                                                                                                      "bible_kids")]), as.matrix(student_parent[, c("religious_attendance_parent", "bible_parent", "religious_attendance_kid", 
                                                                                                                                                                    "bible_kid")])))
# which missing?
new_data$party_kid <- sample(c(0, 1), nrow(new_data), replace = T)
new_data$religious_attendance_parents_new <- ifelse(new_data$party_kid == 1, new_data$religious_attendance, 
                                                    new_data$religious_attendance_kid)
new_data$religious_attendance_kids_new <- ifelse(new_data$party_kid == 1, new_data$religious_attendance_kid, 
                                                 new_data$religious_attendance)
new_data$bible_parents_new <- ifelse(new_data$party_kid == 1, new_data$bible, new_data$bible_kid)
new_data$bible_kids_new <- ifelse(new_data$party_kid == 1, new_data$bible_kid, new_data$bible)
latent_religious_attitudes_f = fscores(mirt(new_data[-c(42, 82, 111, 563, 872, 967, 1079, 1426, 76, 153, 
                                                        160, 248, 338, 414, 415, 506, 583, 590, 678, 768, 844, 845, 904, 905, 906, 1031, 1032, 1033, 1034, 
                                                        1038, 1469, 1470, 1471, 1699, 1752, 1753, 1754, 1902, 1903), c(6, 8)], 1, survey.weights = c(subset_couple_1965_p2$V321, 
                                                                                                                                                     subset_couple_1965_p1$V321, student_parent$V321)[-c(42, 82, 111, 563, 872, 967, 1079, 1426, 76, 153, 
                                                                                                                                                                                                         160, 248, 338, 414, 415, 506, 583, 590, 678, 768, 844, 845, 904, 905, 906, 1031, 1032, 1033, 1034, 
                                                                                                                                                                                                         1038, 1469, 1470, 1471, 1699, 1752, 1753, 1754, 1902, 1903)]), full.scores = T)
latent_religious_attitudes_k = fscores(mirt(new_data[-c(42, 82, 111, 563, 872, 967, 1079, 1426, 76, 153, 
                                                        160, 248, 338, 414, 415, 506, 583, 590, 678, 768, 844, 845, 904, 905, 906, 1031, 1032, 1033, 1034, 
                                                        1038, 1469, 1470, 1471, 1699, 1752, 1753, 1754, 1902, 1903), c(7, 9)], 1, survey.weights = c(subset_couple_1965_p2$V321, 
                                                                                                                                                     subset_couple_1965_p1$V321, student_parent$V321)[-c(42, 82, 111, 563, 872, 967, 1079, 1426, 76, 153, 
                                                                                                                                                                                                         160, 248, 338, 414, 415, 506, 583, 590, 678, 768, 844, 845, 904, 905, 906, 1031, 1032, 1033, 1034, 
                                                                                                                                                                                                         1038, 1469, 1470, 1471, 1699, 1752, 1753, 1754, 1902, 1903)]), full.scores = T)
cor.test(latent_religious_attitudes_f, latent_religious_attitudes_k)
# 0.23 latent
# authoritarianism
subset_couple_1965_p2$non_political <- append(fscores(mirt(subset_couple_1965_p2[-277, c("child_popular", 
                                                                                         "child_religious", "child_successfull", "child_tolerant", "child_cultured", "child_improve")], 1, 
                                                           survey.weights = subset_couple_1965_p1$V322[-277]), full.scores = T), NA, 276)
subset_couple_1965_p1$non_political <- append(append(append(append(append(append(fscores(mirt(subset_couple_1965_p1[-c(30, 
                                                                                                                       133, 218, 264, 340, 401), c("child_popular", "child_religious", "child_successfull", "child_tolerant", 
                                                                                                                                                   "child_cultured", "child_improve")], 1, survey.weights = subset_couple_1965_p1$V322[-c(30, 133, 218, 
                                                                                                                                                                                                                                          264, 340, 401)]), full.scores = T), NA, 29), NA, 132), NA, 217), NA, 263), NA, 339), NA, 400)
cor.test(subset_couple_1965_p2$non_political, subset_couple_1965_p1$non_political, use = "complete.obs")
# latent partisan attitudes
subset_couple_1965_p2$latent_partisan_attitudes <- fscores(mirt(subset_couple_1965_p2[, c("Party", "race", 
                                                                                          "feeling_jews", "feeling_whites", "feeling_catholics", "feeling_unions", "feeling_big_business")], 
                                                                1, survey.weights = subset_couple_1965_p1$V322), full.scores = T)
subset_couple_1965_p1$latent_partisan_attitudes <- fscores(mirt(subset_couple_1965_p1[, c("Party", "race", 
                                                                                          "feeling_jews", "feeling_whites", "feeling_catholics", "feeling_unions", "feeling_big_business")], 
                                                                1, survey.weights = subset_couple_1965_p1$V322), full.scores = T)
cor.test(subset_couple_1965_p2$latent_partisan_attitudes, subset_couple_1965_p1$latent_partisan_attitudes)
# intergenerational
new_data <- data.frame(rbind(as.matrix(subset_couple_1965_p2[, c("Party", "race", "feeling_jews", "feeling_whites", 
                                                                 "feeling_catholics", "feeling_unions", "feeling_big_business", "Party_kid", "race_kid", "feeling_jews_k", 
                                                                 "feeling_whites_k", "feeling_catholics_k", "feeling_unions_k", "feeling_big_business_k")]), as.matrix(subset_couple_1965_p1[, 
                                                                                                                                                                                             c("Party", "race", "feeling_jews", "feeling_whites", "feeling_catholics", "feeling_unions", "feeling_big_business", 
                                                                                                                                                                                               "Party_kid", "race_kid", "feeling_jews_k", "feeling_whites_k", "feeling_catholics_k", "feeling_unions_k", 
                                                                                                                                                                                               "feeling_big_business_k")]), as.matrix(student_parent[, c("Party", "race_f", "feeling_jews_f", 
                                                                                                                                                                                                                                                         "feeling_whites_f", "feeling_catholics_f", "feeling_unions_f", "feeling_big_business_f", "Party_kid", 
                                                                                                                                                                                                                                                         "race_k", "feeling_jews_k", "feeling_whites_k", "feeling_catholics_k", "feeling_unions_k", "feeling_big_business_k")])))
latent_partisan_attitudes_k = fscores(mirt(new_data[, 8:14], 1, survey.weights = c(student_parent$V321, 
                                                                                   subset_couple_1965_p1$V321, subset_couple_1965_p2$V321)), full.scores = T)
latent_partisan_attitudes_f = fscores(mirt(new_data[, 1:7], 1, survey.weights = c(student_parent$V321, 
                                                                                  subset_couple_1965_p1$V321, subset_couple_1965_p2$V321)), full.scores = T)
cor.test(latent_partisan_attitudes_f, latent_partisan_attitudes_k)
new_data$party_kid <- sample(c(0, 1), nrow(new_data), replace = T)
new_data$party_parents_new <- ifelse(new_data$party_kid == 1, new_data$Party, new_data$Party_kid)
new_data$party_kids_new <- ifelse(new_data$party_kid == 1, new_data$Party_kid, new_data$Party)
new_data$race_parents_new <- ifelse(new_data$party_kid == 1, new_data$race, new_data$race_kid)
new_data$race_kids_new <- ifelse(new_data$party_kid == 1, new_data$race_kid, new_data$race)
new_data$feeling_jews_parents_new <- ifelse(new_data$party_kid == 1, new_data$feeling_jews, new_data$feeling_jews_k)
new_data$feeling_jews_kids_new <- ifelse(new_data$party_kid == 1, new_data$feeling_jews_k, new_data$feeling_jews)
new_data$feeling_whites_parents_new <- ifelse(new_data$party_kid == 1, new_data$feeling_whites, new_data$feeling_whites_k)
new_data$feeling_whites_kids_new <- ifelse(new_data$party_kid == 1, new_data$feeling_whites_k, new_data$feeling_whites)
new_data$feeling_catholics_parents_new <- ifelse(new_data$party_kid == 1, new_data$feeling_catholics, 
                                                 new_data$feeling_catholics_k)
new_data$feeling_catholics_kids_new <- ifelse(new_data$party_kid == 1, new_data$feeling_catholics_k, 
                                              new_data$feeling_catholics)
new_data$feeling_unions_parents_new <- ifelse(new_data$party_kid == 1, new_data$feeling_unions, new_data$feeling_unions_k)
new_data$feeling_unions_kids_new <- ifelse(new_data$party_kid == 1, new_data$feeling_unions_k, new_data$feeling_unions)
new_data$feeling_big_business_parents_new <- ifelse(new_data$party_kid == 1, new_data$feeling_big_business, 
                                                    new_data$feeling_big_business_k)
new_data$feeling_big_business_kids_new <- ifelse(new_data$party_kid == 1, new_data$feeling_big_business_k, 
                                                 new_data$feeling_big_business)
latent_partisan_attitudes_k = fscores(mirt(new_data[, c("party_kids_new", "race_kids_new", "feeling_jews_kids_new", 
                                                        "feeling_whites_kids_new", "feeling_catholics_kids_new", "feeling_unions_kids_new", "feeling_big_business_kids_new")], 
                                           1, survey.weights = c(subset_couple_1965_p2$V321, subset_couple_1965_p1$V321, student_parent$V321)), 
                                      full.scores = T)
latent_partisan_attitudes_f = fscores(mirt(new_data[, c("party_parents_new", "race_parents_new", "feeling_jews_parents_new", 
                                                        "feeling_whites_parents_new", "feeling_catholics_parents_new", "feeling_unions_parents_new", "feeling_big_business_parents_new")], 
                                           1, survey.weights = c(subset_couple_1965_p2$V321, subset_couple_1965_p1$V321, student_parent$V321)), 
                                      full.scores = T)
cor.test(latent_partisan_attitudes_f, latent_partisan_attitudes_k)
#### ????? new_data=rbind(student_parent[,c('feeling_big_business_k','feeling_big_business_f',
#### 'feeling_unions_k','feeling_unions_f','school_prayer_kid', 'school_prayer_family',
#### 'speech_church_kid', 'speech_church_family', 'communist_kid', 'communist_family')],
#### subset_couple_1965_p1[,c('feeling_big_business_k','feeling_big_business_f',
#### 'feeling_unions_k','feeling_unions_f','school_prayer_kid', 'school_prayer', 'speech_church_kid',
#### 'speech_church', 'communist_kid', 'communist')])
subset_couple_1965_p2$latent_ideology <- fscores(mirt(subset_couple_1965_p2[, c("school_prayer", "speech_church", 
                                                                                "communist", "integration")], 1, survey.weights = subset_couple_1965_p1$V322), full.scores = T)
subset_couple_1965_p1$latent_ideology <- fscores(mirt(subset_couple_1965_p1[, c("school_prayer", "speech_church", 
                                                                                "communist", "integration")], 1, survey.weights = subset_couple_1965_p1$V322), full.scores = T)
student_parent$latent_ideology <- append(fscores(mirt(student_parent[-1072, c("school_prayer_family", 
                                                                              "speech_church_family", "communist_family", "integration_family")], 1, survey.weights = student_parent$V321[-1072]), 
                                                 full.scores = T), NA, after = 1071)
cor.test(subset_couple_1965_p2$latent_ideology, subset_couple_1965_p1$latent_ideology)
# create new data for student parent comparison
new_data <- data.frame(rbind(as.matrix(subset_couple_1965_p2[, c("school_prayer", "speech_church", "communist", 
                                                                 "school_prayer_kid", "speech_church_kid", "communist_kid")]), as.matrix(subset_couple_1965_p1[, c("school_prayer", 
                                                                                                                                                                   "speech_church", "communist", "school_prayer_kid", "speech_church_kid", "communist_kid")]), as.matrix(student_parent[, 
                                                                                                                                                                                                                                                                                        c("school_prayer_family", "speech_church_family", "communist_family", "school_prayer_kid", "speech_church_kid", 
                                                                                                                                                                                                                                                                                          "communist_kid")])))
latent_ideology_f = append(fscores(mirt(new_data[-1932, 1:3], 1, survey.weights = c(subset_couple_1965_p2$V321, 
                                                                                    subset_couple_1965_p1$V321, student_parent$V321)[-1932]), full.scores = T), NA, after = 1931)
latent_ideology_k = fscores(mirt(new_data[, 4:6], 1, survey.weights = c(subset_couple_1965_p2$V321, subset_couple_1965_p1$V321, 
                                                                        student_parent$V321)), full.scores = T)
cor.test(latent_ideology_f, latent_ideology_k)
new_data$party_kid <- sample(c(0, 1), nrow(new_data), replace = T)
new_data$school_prayer_parents_new <- ifelse(new_data$party_kid == 1, new_data$school_prayer, new_data$school_prayer_kid)
new_data$school_prayer_kids_new <- ifelse(new_data$party_kid == 1, new_data$school_prayer_kid, new_data$school_prayer)
new_data$speech_church_parents_new <- ifelse(new_data$party_kid == 1, new_data$speech_church, new_data$speech_church_kid)
new_data$speech_church_kids_new <- ifelse(new_data$party_kid == 1, new_data$speech_church_kid, new_data$speech_church)
new_data$communist_parents_new <- ifelse(new_data$party_kid == 1, new_data$communist, new_data$communist_kid)
new_data$communist_kids_new <- ifelse(new_data$party_kid == 1, new_data$communist_kid, new_data$communist)
latent_ideology_f = append(fscores(mirt(new_data[-1932, c("school_prayer_parents_new", "speech_church_parents_new", 
                                                          "communist_parents_new")], 1, survey.weights = c(student_parent$V321, subset_couple_1965_p1$V321, 
                                                                                                           subset_couple_1965_p2$V321)[-1932]), full.scores = T), NA, after = 1931)
latent_ideology_k = fscores(mirt(new_data[, c("school_prayer_kids_new", "speech_church_kids_new", "communist_kids_new")], 
                                 1, survey.weights = c(student_parent$V321, subset_couple_1965_p1$V321, subset_couple_1965_p2$V321)), 
                            full.scores = T)
cor.test(latent_ideology_f, latent_ideology_k)