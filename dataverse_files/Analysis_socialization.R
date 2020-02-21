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
library(psych)
library(SDMTools)
library(weights)
library(boot)
# read in data##############################
set.seed(2141)
#couple_1965 <- read.dta("07286-0001-Data.dta")
############################################################### Split in student-parent and parent-parent-student dyads####################################### single parent-student data
student_parent <- couple_1965[as.numeric(couple_1965$V318) != 9 & !duplicated(couple_1965$V319) & !duplicated(couple_1965$V319, fromLast = T), ]
# pair how to proceed creating table
summary(couple_1965$V318)
summary(couple_1965$V2)
# based on parents
subset_fathers <- which(duplicated(couple_1965$V319))
subset_mothers <- which(duplicated(couple_1965$V319, fromLast = T))
summary(couple_1965$V599)
subset_couple_1965_fathers <- couple_1965[subset_fathers, ]
subset_couple_1965_mothers <- couple_1965[subset_mothers, ]
# pull out fatthers from subset_couple_1965_mothers and vice-versa, must be erroneous coding!
subset_couple_1965_fathers <- rbind(subset_couple_1965_fathers, subset_couple_1965_mothers[subset_couple_1965_mothers$V318 == 3, ])
subset_couple_1965_mothers <- rbind(subset_couple_1965_mothers, subset_couple_1965_fathers[subset_couple_1965_fathers$V318 == 2, ])
subset_couple_1965_mothers <- subset(subset_couple_1965_mothers, V318 == 2)
subset_couple_1965_fathers <- subset(subset_couple_1965_fathers, V318 == 3)
# now split cbind parents responses
which(student_parent$V599 %in% subset_couple_1965_mothers$V599)
which(colnames(couple_1965) == "V2")  #col 2 flag if mother, child or father, not working
which(colnames(couple_1965) == "V3")  #col 3 identifier matching for parents
which(colnames(couple_1965) == "V318")  #col 382; mother-child-father-identifier
which(colnames(couple_1965) == "V596")  #col 700; Ideology
which(colnames(couple_1965) == "V322")  #col 386; weight for Data divided by parental sex
which(colnames(couple_1965) == "V149")  #col 202; PID (7 scale_) student
which(colnames(couple_1965) == "V382")  #col 466; PID (7 scale_) parents
# check if all match
which(subset_couple_1965_fathers$V319 != subset_couple_1965_mothers$V319)  #identifier
which(subset_couple_1965_fathers$V318 == subset_couple_1965_mothers$V318)  #gender
which(subset_couple_1965_fathers$V322 != subset_couple_1965_mothers$V322)  #weight parent corss-section
which(subset_couple_1965_fathers$V321 != subset_couple_1965_mothers$V321)  #weight
###################################### party education
subset_couple_1965_fathers$education <- NA
subset_couple_1965_fathers$education[subset_couple_1965_fathers$V520 < 71] <- 1  #hs/less
subset_couple_1965_fathers$education[subset_couple_1965_fathers$V520 == 71 | subset_couple_1965_fathers$V520 == 72] <- 2  # some cl
subset_couple_1965_fathers$education[subset_couple_1965_fathers$V520 >= 81 & subset_couple_1965_fathers$V520 < 88] <- 3  #college+
subset_couple_1965_mothers$education <- NA
subset_couple_1965_mothers$education[subset_couple_1965_mothers$V520 < 71] <- 1  #hs/less
subset_couple_1965_mothers$education[subset_couple_1965_mothers$V520 == 71 | subset_couple_1965_mothers$V520 == 72] <- 2  # some cl
subset_couple_1965_mothers$education[subset_couple_1965_mothers$V520 >= 81 & subset_couple_1965_mothers$V520 < 88] <- 3  #college+
student_parent$education <- NA
student_parent$education[student_parent$V520 < 71] <- 1  #hs/less
student_parent$education[student_parent$V520 == 71 | student_parent$V520 == 72] <- 2  # some cl
student_parent$education[student_parent$V520 >= 81 & student_parent$V520 < 88] <- 3  #college+
subset_couple_1965_mothers$education_overall = round((subset_couple_1965_mothers$education + subset_couple_1965_fathers$education)/2)
subset_couple_1965_mothers$education_overall = factor(subset_couple_1965_mothers$education, labels = c("HS or less", "Some college", "College+"))
subset_couple_1965_fathers$discussion <- recode(subset_couple_1965_fathers$V141, "5=1;3=2;2=3;1=4;4=NA;8=NA;9=NA")
subset_couple_1965_mothers$discussion <- recode(subset_couple_1965_mothers$V141, "5=1;3=2;2=3;1=4;4=NA;8=NA;9=NA")
student_parent$discussion_overall <- recode(student_parent$V141, "5=1;3=2;2=3;1=4;4=NA;8=NA;9=NA")
subset_couple_1965_mothers$discussion_overall <- ifelse(subset_couple_1965_fathers$discussion > subset_couple_1965_mothers$discussion, subset_couple_1965_fathers$discussion, 
                                                        subset_couple_1965_mothers$discussion)
subset_couple_1965_fathers$discussion_overall <- ifelse(subset_couple_1965_fathers$discussion > subset_couple_1965_mothers$discussion, subset_couple_1965_fathers$discussion, 
                                                        subset_couple_1965_mothers$discussion)

subset_couple_1965_mothers$Party <- recode(subset_couple_1965_mothers$V382, "0=NA;9=NA")
subset_couple_1965_fathers$Party <- recode(subset_couple_1965_fathers$V382, "0=NA;9=NA")
subset_couple_1965_fathers$Party_kid <- recode(subset_couple_1965_fathers$V149, "11=1;12=2;13=3;14=4;15=5;16=6;17=7;20=NA;30=NA;40=NA;88=NA;99=NA")
subset_couple_1965_mothers$Party_kid <- recode(subset_couple_1965_mothers$V149, "11=1;12=2;13=3;14=4;15=5;16=6;17=7;20=NA;30=NA;40=NA;88=NA;99=NA")
student_parent$Party_kid <- recode(student_parent$V149, "11=1;12=2;13=3;14=4;15=5;16=6;17=7;20=NA;30=NA;40=NA;88=NA;99=NA")
student_parent$Party <- recode(student_parent$V382, "0=NA;8=NA;9=NA")
student_parent$Party_kid <- recode(student_parent$V149, "11=1;12=2;13=3;14=4;15=5;16=6;17=7;20=NA;30=NA;40=NA;88=NA;99=NA")
polychoric(cbind(subset_couple_1965_mothers$Party, subset_couple_1965_fathers$Party), weight = subset_couple_1965_fathers$V322)$rho

subset_couple_1965_mothers$homogeneity <- NA
subset_couple_1965_mothers$homogeneity[subset_couple_1965_mothers$Party < 4 & subset_couple_1965_fathers$Party < 4] <- 1
subset_couple_1965_mothers$homogeneity[subset_couple_1965_mothers$Party < 4 & subset_couple_1965_fathers$Party > 4] <- 0
subset_couple_1965_mothers$homogeneity[subset_couple_1965_mothers$Party > 4 & subset_couple_1965_fathers$Party < 4] <- 0
subset_couple_1965_mothers$homogeneity[subset_couple_1965_mothers$Party > 4 & subset_couple_1965_fathers$Party > 4] <- 1
subset_couple_1965_mothers$homogeneity[subset_couple_1965_mothers$Party == 4 & subset_couple_1965_fathers$Party == 4] <- 1
subset_couple_1965_mothers$homogeneity[subset_couple_1965_mothers$Party > 4 & subset_couple_1965_fathers$Party == 4] <- 0
subset_couple_1965_mothers$homogeneity[subset_couple_1965_mothers$Party == 4 & subset_couple_1965_fathers$Party > 4] <- 0
subset_couple_1965_mothers$homogeneity[subset_couple_1965_mothers$Party < 4 & subset_couple_1965_fathers$Party == 4] <- 0
subset_couple_1965_mothers$homogeneity[subset_couple_1965_mothers$Party == 4 & subset_couple_1965_fathers$Party < 4] <- 0

subset_couple_1965_fathers$homogeneity <- subset_couple_1965_mothers$homogeneity
# Intergenerational


new_data <- rbind(as.matrix(student_parent[, c("Party_kid", "Party", "V321", "discussion_overall")]), as.matrix(subset_couple_1965_mothers[, c("Party_kid", 
                                                                                                                                               "Party", "V321", "discussion_overall")]), as.matrix(subset_couple_1965_fathers[, c("Party_kid", "Party", "V321", "discussion_overall")]))
polychoric(cbind(new_data[, 1], new_data[, 2]), weight = new_data[, 3])$rho

polychoric(cbind(new_data[, 1], new_data[, 2]), weight = new_data[, 3])$rho

crosstab(new_data[, 1], new_data[, 2], prop.c = F, prop.r = F, prop.chisq = F, weight = new_data[, 3])
round((180 + 108 + 30 + 147 + 167 + 39 + 63 + 68 + 38 + 50 + 17 + 34 + 45 + 23 + 92 + 61 + 9 + 35 + 64)/1851 * 100, 2)
# standard error
sqrt(68.61156 * (100 - 68.61156)/1851)
# disagreement
round((2 + 5 + 3 + 9 + 38 + 17 + 14 + 30 + 14 + 9 + 13 + 14 + 16 + 30 + 14 + 9 + 13 + 4)/1851 * 100, 2)


new_data <- data.frame(new_data)
new_data$Party_kid <- car::recode(new_data$Party_kid, "1=1;2=1;3=1;4=2;5=3;6=3;7=3")
new_data$Party <- car::recode(new_data$Party, "1=1;2=1;3=1;4=2;5=3;6=3;7=3")
with(new_data[new_data$discussion_overall == 1, ], wt.mean(Party_kid == Party, wt = V321))
with(new_data[new_data$discussion_overall != 1, ], wt.mean(Party_kid == Party, wt = V321))

with(new_data[new_data$discussion_overall == 2, ], wt.mean(Party_kid == Party, wt = V321))
with(new_data[new_data$discussion_overall == 3, ], wt.mean(Party_kid == Party, wt = V321))
with(new_data[new_data$discussion_overall == 4, ], wt.mean(Party_kid == Party, wt = V321))

# partisan agreement
crosstab(subset_couple_1965_mothers$Party, subset_couple_1965_fathers$Party, weight = subset_couple_1965_fathers$V322)
# partisan agreement no independents
crosstab(subset_couple_1965_mothers$Party[subset_couple_1965_mothers$Party != 4 & subset_couple_1965_fathers$Party != 4], subset_couple_1965_fathers$Party[subset_couple_1965_mothers$Party != 
                                                                                                                                                             4 & subset_couple_1965_fathers$Party != 4], weight = subset_couple_1965_mothers$V322[subset_couple_1965_mothers$Party != 4 & subset_couple_1965_fathers$Party != 
                                                                                                                                                                                                                                                    4], dnn = c("Party1", "Party2"))
student_parent$Party_kid_3 = recode(student_parent$Party_kid, "1=1;2=1;3=1;4=2;5=3;6=3;7=3")
student_parent$Party_3 = recode(student_parent$Party, "1=1;2=1;3=1;4=2;5=3;6=3;7=3")
subset_couple_1965_mothers$Party_3 = recode(subset_couple_1965_mothers$Party, "1=1;2=1;3=1;4=2;5=3;6=3;7=3")
subset_couple_1965_mothers$Party_kid_3 = recode(subset_couple_1965_mothers$Party_kid, "1=1;2=1;3=1;4=2;5=3;6=3;7=3")
subset_couple_1965_fathers$Party_3 = recode(subset_couple_1965_fathers$Party, "1=1;2=1;3=1;4=2;5=3;6=3;7=3")
subset_couple_1965_fathers$Party_kid_3 = recode(subset_couple_1965_fathers$Party_kid, "1=1;2=1;3=1;4=2;5=3;6=3;7=3")

new_data <- rbind(as.matrix(student_parent[, c("Party_kid_3", "Party_3", "V321")]), as.matrix(subset_couple_1965_mothers[, c("Party_kid_3", "Party_3", 
                                                                                                                             "V321")]), as.matrix(subset_couple_1965_fathers[, c("Party_kid_3", "Party_3", "V321")]))
polychoric(cbind(new_data[, 1], new_data[, 2]), weight = new_data[, 3])$rho

# 7 point scale #homogeneity (1,2,3) (4) and (5, 6, 7));
round((56 + 26 + 5 + 39 + 59 + 17 + 6 + 7 + 17 + 12 + 8 + 6 + 5 + 6 + 37 + 12 + 4 + 17 + 32)/507 * 100, 2)
# standard error
sqrt(73.17554 * (100 - 73.17554)/507)
# disagreement
round((0 + 1 + 1 + 4 + 13 + 3 + 9 + 5 + 3 + 1 + 4 + 0 + 1 + 13 + 5 + 2 + 1)/507 * 100, 2)
# 7 point scale homogeneity no independents (1,2,3)(5,6,7)
round((56 + 26 + 5 + 39 + 59 + 17 + 8 + 6 + 5 + 6 + 37 + 12 + 4 + 17 + 32)/425, 2)

wt.mean(subset_couple_1965_mothers$homogeneity, wt = subset_couple_1965_mothers$V322)
crosstab(subset_couple_1965_mothers$homogeneity, subset_couple_1965_mothers$education_overall, prop.c = T)

# transmission rate by education and parental homogeneity

new_data <- cbind(subset_couple_1965_mothers[, c("Party_kid", "Party", "V321", "homogeneity")], subset_couple_1965_fathers[, "Party"])
new_data[, 1] <- car::recode(new_data[, 1], "1=1;2=1;3=1;4=2;5=3;6=3;7=3")
new_data[, 2] <- car::recode(new_data[, 2], "1=1;2=1;3=1;4=2;5=3;6=3;7=3")
new_data[, 5] <- car::recode(new_data[, 5], "1=1;2=1;3=1;4=2;5=3;6=3;7=3")
colnames(new_data)[c(2, 5)] <- c("Party_mothers", "Party_fathers")
with(new_data[new_data$homogeneity == 1, ], wt.mean(Party_kid != Party_mothers & Party_kid != Party_fathers, wt = V321))
with(new_data[new_data$homogeneity == 0, ], wt.mean(Party_kid != Party_mothers & Party_kid != Party_fathers, wt = V321))

# 0.71 isues##################################################### how to code issue-agreement? get issue agreement, 1=liberal, 2=middle;
# 3=conservative school prayer
subset_couple_1965_fathers$school_prayer <- recode(subset_couple_1965_fathers$V365, "0=2;1=3;3=2;5=1;8=2;9=NA")
subset_couple_1965_fathers$school_prayer_kid <- recode(subset_couple_1965_fathers$V200, "0=2;1=3;3=2;5=1;8=2;9=NA")
subset_couple_1965_mothers$school_prayer <- recode(subset_couple_1965_mothers$V365, "0=2;1=3;3=2;5=1;8=2;9=NA")
subset_couple_1965_mothers$school_prayer_kid <- recode(subset_couple_1965_mothers$V200, "0=2;1=3;3=2;5=1;8=2;9=NA")
# recode single-parent data
student_parent$school_prayer_family <- recode(student_parent$V365, "0=2;1=3;3=2;5=1;8=2;9=NA")
student_parent$school_prayer_kid <- recode(student_parent$V200, "0=2;1=3;3=2;5=1;8=2;9=NA")
polychoric(cbind(subset_couple_1965_mothers$school_prayer, subset_couple_1965_fathers$school_prayer), weight = subset_couple_1965_fathers$V322)$rho
wt.mean(abs(subset_couple_1965_mothers$school_prayer - subset_couple_1965_fathers$school_prayer), subset_couple_1965_fathers$V322)/wt.sd(c(subset_couple_1965_fathers$school_prayer), 
                                                                                                                                         subset_couple_1965_fathers$V322)
wtd.t.test(subset_couple_1965_fathers$school_prayer, subset_couple_1965_mothers$school_prayer, weight = subset_couple_1965_fathers$V322)
# Free speech against church
subset_couple_1965_fathers$speech_church <- recode(subset_couple_1965_fathers$V356, "1=1;3=2;5=3;8=2;9=NA")
subset_couple_1965_mothers$speech_church <- recode(subset_couple_1965_mothers$V356, "1=1;3=2;5=3;8=2;9=NA")
subset_couple_1965_fathers$speech_church_kid <- recode(subset_couple_1965_fathers$V121, "1=1;3=2;5=3;8=2;9=NA")
subset_couple_1965_mothers$speech_church_kid <- recode(subset_couple_1965_mothers$V121, "1=1;3=2;5=3;8=2;9=NA")
# recode single-parent data
student_parent$speech_church_family <- recode(student_parent$V356, "1=1;3=2;5=3;8=2;9=NA")
student_parent$speech_church_kid <- recode(student_parent$V121, "1=1;3=2;5=3;8=2;9=NA")
polychoric(cbind(subset_couple_1965_mothers$speech_church, subset_couple_1965_fathers$speech_church), weight = subset_couple_1965_fathers$V322)$rho
# allow communist in office
subset_couple_1965_fathers$communist <- recode(subset_couple_1965_fathers$V358, "1=1;3=2;5=3;6=NA;8=2;9=NA")
subset_couple_1965_mothers$communist <- recode(subset_couple_1965_mothers$V358, "1=1;3=2;5=3;6=NA;8=2;9=NA")
subset_couple_1965_fathers$communist_kid <- recode(subset_couple_1965_fathers$V122, "1=1;3=2;5=3;6=NA;8=2;9=NA")
subset_couple_1965_mothers$communist_kid <- recode(subset_couple_1965_mothers$V122, "1=1;3=2;5=3;6=NA;8=2;9=NA")
# recode single-parent data
student_parent$communist_family <- recode(student_parent$V358, "1=1;3=2;5=3;6=NA;8=2;9=NA")
student_parent$communist_kid <- recode(student_parent$V122, "1=1;3=2;5=3;6=NA;8=2;9=NA")
polychoric(cbind(subset_couple_1965_mothers$communist, subset_couple_1965_fathers$communist), weight = subset_couple_1965_fathers$V322)$rho
# residential integration, no kid data
subset_couple_1965_fathers$integration <- recode(subset_couple_1965_fathers$V366, "1=1;5=2;8=3;9=NA")
subset_couple_1965_mothers$integration <- recode(subset_couple_1965_mothers$V366, "1=1;5=2;8=3;9=NA")
subset_couple_1965_mothers$integration_kid <- recode(subset_couple_1965_mothers$V201, "1=1;5=2;8=3;9=NA")
subset_couple_1965_fathers$integration_kid <- recode(subset_couple_1965_fathers$V201, "1=1;5=2;8=3;9=NA")
# recode single-parent data
student_parent$integration_family <- recode(student_parent$V366, "1=1;5=2;8=3;9=NA")
student_parent$integration_kid <- recode(student_parent$V201, "1=1;5=2;8=3;9=NA")
polychoric(cbind(subset_couple_1965_mothers$integration, subset_couple_1965_fathers$integration), weight = subset_couple_1965_fathers$V322)$rho
mean(c(mean(c(subset_couple_1965_fathers$Party < 4 & subset_couple_1965_fathers$communist < 2, subset_couple_1965_mothers$Party < 4 & subset_couple_1965_mothers$communist < 
                2) | c(subset_couple_1965_fathers$Party > 4 & subset_couple_1965_fathers$communist > 2, subset_couple_1965_mothers$Party > 4 & subset_couple_1965_mothers$communist > 
                         2), na.rm = T), mean(c(subset_couple_1965_fathers$Party < 4 & subset_couple_1965_fathers$speech_church < 2, subset_couple_1965_mothers$Party < 
                                                  4 & subset_couple_1965_mothers$speech_church < 2) | c(subset_couple_1965_fathers$Party > 4 & subset_couple_1965_fathers$speech_church > 2, subset_couple_1965_mothers$Party > 
                                                                                                          4 & subset_couple_1965_mothers$speech_church > 2), na.rm = T), mean(c(subset_couple_1965_fathers$Party < 4 & subset_couple_1965_fathers$school_prayer < 
                                                                                                                                                                                  2, subset_couple_1965_mothers$Party < 4 & subset_couple_1965_mothers$school_prayer < 2) | c(subset_couple_1965_fathers$Party > 4 & subset_couple_1965_fathers$school_prayer > 
                                                                                                                                                                                                                                                                                2, subset_couple_1965_mothers$Party > 4 & subset_couple_1965_mothers$school_prayer > 2), na.rm = T), mean(c(subset_couple_1965_fathers$Party < 
                                                                                                                                                                                                                                                                                                                                                                                              4 & subset_couple_1965_fathers$integration < 2, subset_couple_1965_mothers$Party < 4 & subset_couple_1965_mothers$integration < 2) | c(subset_couple_1965_fathers$Party > 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       4 & subset_couple_1965_fathers$integration > 2, subset_couple_1965_mothers$Party > 4 & subset_couple_1965_mothers$integration > 2), na.rm = T)))
# 0.40 thermometer###################################################### Partisan Attitudes
subset_couple_1965_fathers$feeling_unions <- 100 - subset_couple_1965_fathers$V436
subset_couple_1965_fathers$feeling_unions_k <- 100 - subset_couple_1965_fathers$V192
student_parent$feeling_unions_k <- 100 - student_parent$V192
student_parent$feeling_unions_f <- 100 - student_parent$V436
subset_couple_1965_mothers$feeling_unions <- 100 - subset_couple_1965_mothers$V436
subset_couple_1965_mothers$feeling_unions_k <- 100 - subset_couple_1965_mothers$V192
corr(cbind(subset_couple_1965_fathers$feeling_unions, subset_couple_1965_mothers$feeling_unions), w = subset_couple_1965_fathers$V322)
### big business
subset_couple_1965_fathers$feeling_big_business <- subset_couple_1965_fathers$V439
subset_couple_1965_fathers$feeling_big_business_k <- subset_couple_1965_fathers$V195
subset_couple_1965_mothers$feeling_big_business <- subset_couple_1965_mothers$V439
subset_couple_1965_mothers$feeling_big_business_k <- subset_couple_1965_mothers$V195
student_parent$feeling_big_business_k <- student_parent$V195
student_parent$feeling_big_business_f <- student_parent$V439
corr(cbind(subset_couple_1965_fathers$feeling_big_business, subset_couple_1965_mothers$feeling_big_business), w = subset_couple_1965_fathers$V322)
# 0.10/0.02 correlation with Party Catholics
subset_couple_1965_fathers$feeling_catholics <- 100 - subset_couple_1965_fathers$V438
subset_couple_1965_fathers$feeling_catholics_k <- 100 - subset_couple_1965_fathers$V194
student_parent$feeling_catholics_k <- 100 - student_parent$V194
student_parent$feeling_catholics_f <- 100 - student_parent$V438
subset_couple_1965_mothers$feeling_catholics <- 100 - subset_couple_1965_mothers$V438
subset_couple_1965_mothers$feeling_catholics_k <- 100 - subset_couple_1965_mothers$V194
corr(cbind(subset_couple_1965_fathers$feeling_catholics, subset_couple_1965_mothers$feeling_catholics), w = subset_couple_1965_fathers$V322)
# 0.06/0.03 correlation with Party jews
subset_couple_1965_fathers$feeling_jews <- 100 - subset_couple_1965_fathers$V440
subset_couple_1965_fathers$feeling_jews_k <- 100 - subset_couple_1965_fathers$V196
student_parent$feeling_jews_k <- 100 - student_parent$V196
student_parent$feeling_jews_f <- 100 - student_parent$V440
subset_couple_1965_mothers$feeling_jews <- 100 - subset_couple_1965_mothers$V440
subset_couple_1965_mothers$feeling_jews_k <- 100 - subset_couple_1965_mothers$V196
corr(cbind(subset_couple_1965_fathers$feeling_jews, subset_couple_1965_mothers$feeling_jews), w = subset_couple_1965_fathers$V322)
#-0.02/-0.01 correlation with Party
# whites
subset_couple_1965_fathers$feeling_whites <- 100 - subset_couple_1965_fathers$V441
subset_couple_1965_fathers$feeling_whites_k <- 100 - subset_couple_1965_fathers$V197
student_parent$feeling_whites_k <- 100 - student_parent$V197
student_parent$feeling_whites_f <- 100 - student_parent$V441
subset_couple_1965_mothers$feeling_whites <- 100 - subset_couple_1965_mothers$V441
subset_couple_1965_mothers$feeling_whites_k <- 100 - subset_couple_1965_mothers$V197
#-0.01/-0.03 correlation with Party
corr(cbind(subset_couple_1965_fathers$feeling_whites, subset_couple_1965_mothers$feeling_whites), w = subset_couple_1965_fathers$V322)
################################## Intergenerational Data############################ ISUES############################## school Prayer create new data for student parent
################################## comparison
new_data <- rbind(as.matrix(subset_couple_1965_mothers[, c("school_prayer", "school_prayer_kid", "V321")]), as.matrix(subset_couple_1965_fathers[, 
                                                                                                                                                 c("school_prayer", "school_prayer_kid", "V321")]), as.matrix(student_parent[, c("school_prayer_family", "school_prayer_kid", "V321")]))
# results for inter-generational( families with (both two parent and one parent families))
polychoric(cbind(new_data[, 1], new_data[, 2]), new_data[, 3])$rho
# 0.28 Church Speech create new data for student parent comparison
new_data <- rbind(as.matrix(subset_couple_1965_mothers[, c("speech_church", "speech_church_kid", "V321")]), as.matrix(subset_couple_1965_fathers[, 
                                                                                                                                                 c("speech_church", "speech_church_kid", "V321")]), as.matrix(student_parent[, c("speech_church_family", "speech_church_kid", "V321")]))
## intergeneration
polychoric(cbind(new_data[, 1], new_data[, 2]), new_data[, 3])$rho
# Communist create new data for student parent comparison
new_data <- rbind(as.matrix(subset_couple_1965_mothers[, c("communist", "communist_kid", "V321")]), as.matrix(subset_couple_1965_fathers[, c("communist", 
                                                                                                                                             "communist_kid", "V321")]), as.matrix(student_parent[, c("communist_family", "communist_kid", "V321")]))
## intergeneration
polychoric(cbind(new_data[, 1], new_data[, 2]), new_data[, 3])$rho
# inegration create new data for student parent comparison
new_data <- rbind(as.matrix(subset_couple_1965_mothers[, c("integration", "integration_kid", "V321")]), as.matrix(subset_couple_1965_fathers[, c("integration", 
                                                                                                                                                 "integration_kid", "V321")]), as.matrix(student_parent[, c("integration_family", "integration_kid", "V321")]))
# results for inter-generational( families with (both two parent and one parent families))
polychoric(cbind(new_data[, 1], new_data[, 2]), new_data[, 3])$rho
# 0.15 Thermometers################### Unions
new_data <- rbind(as.matrix(subset_couple_1965_mothers[, c("feeling_unions", "feeling_unions_k", "V321")]), as.matrix(subset_couple_1965_fathers[, 
                                                                                                                                                 c("feeling_unions", "feeling_unions_k", "V321")]), as.matrix(student_parent[, c("feeling_unions_f", "feeling_unions_k", "V321")]))
corr(cbind(new_data[, 1], new_data[, 2]), w = new_data[, 3])
# Big Business
new_data <- rbind(as.matrix(subset_couple_1965_mothers[, c("feeling_big_business", "feeling_big_business_k", "V321")]), as.matrix(subset_couple_1965_fathers[, 
                                                                                                                                                             c("feeling_big_business", "feeling_big_business_k", "V321")]), as.matrix(student_parent[, c("feeling_big_business_f", "feeling_big_business_k", 
                                                                                                                                                                                                                                                         "V321")]))
corr(cbind(new_data[, 1], new_data[, 2]), w = new_data[, 3])
# catholics
new_data <- rbind(as.matrix(subset_couple_1965_mothers[, c("feeling_catholics", "feeling_catholics_k", "V321")]), as.matrix(subset_couple_1965_fathers[, 
                                                                                                                                                       c("feeling_catholics", "feeling_catholics_k", "V321")]), as.matrix(student_parent[, c("feeling_catholics_f", "feeling_catholics_k", "V321")]))
corr(cbind(new_data[, 1], new_data[, 2]), w = new_data[, 3])
# Jews
new_data <- rbind(as.matrix(subset_couple_1965_mothers[, c("feeling_jews", "feeling_jews_k", "V321")]), as.matrix(subset_couple_1965_fathers[, c("feeling_jews", 
                                                                                                                                                 "feeling_jews_k", "V321")]), as.matrix(student_parent[, c("feeling_jews_f", "feeling_jews_k", "V321")]))
corr(cbind(new_data[, 1], new_data[, 2]), w = new_data[, 3])
# whites
new_data <- rbind(as.matrix(subset_couple_1965_mothers[, c("feeling_whites", "feeling_whites_k", "V321")]), as.matrix(subset_couple_1965_fathers[, 
                                                                                                                                                 c("feeling_whites", "feeling_whites_k", "V321")]), as.matrix(student_parent[, c("feeling_whites_f", "feeling_whites_k", "V321")]))
corr(cbind(new_data[, 1], new_data[, 2]), w = new_data[, 3])
################################################## scaled ideology and party################################ rescale for thermometer
subset_couple_1965_fathers$feeling_big_business <- ifelse(subset_couple_1965_fathers$feeling_big_business <= 20, 1, ifelse(subset_couple_1965_fathers$feeling_big_business <= 
                                                                                                                             40, 2, ifelse(subset_couple_1965_fathers$feeling_big_business <= 60, 3, ifelse(subset_couple_1965_fathers$feeling_big_business <= 80, 4, 5))))
subset_couple_1965_mothers$feeling_big_business <- ifelse(subset_couple_1965_mothers$feeling_big_business <= 20, 1, ifelse(subset_couple_1965_mothers$feeling_big_business <= 
                                                                                                                             40, 2, ifelse(subset_couple_1965_mothers$feeling_big_business <= 60, 3, ifelse(subset_couple_1965_mothers$feeling_big_business <= 80, 4, 5))))
subset_couple_1965_fathers$feeling_big_business_k <- ifelse(subset_couple_1965_fathers$feeling_big_business_k <= 20, 1, ifelse(subset_couple_1965_fathers$feeling_big_business_k <= 
                                                                                                                                 40, 2, ifelse(subset_couple_1965_fathers$feeling_big_business_k <= 60, 3, ifelse(subset_couple_1965_fathers$feeling_big_business_k <= 80, 4, 
                                                                                                                                                                                                                  5))))
subset_couple_1965_mothers$feeling_big_business_k <- ifelse(subset_couple_1965_mothers$feeling_big_business_k <= 20, 1, ifelse(subset_couple_1965_mothers$feeling_big_business_k <= 
                                                                                                                                 40, 2, ifelse(subset_couple_1965_mothers$feeling_big_business_k <= 60, 3, ifelse(subset_couple_1965_mothers$feeling_big_business_k <= 80, 4, 
                                                                                                                                                                                                                  5))))
student_parent$feeling_big_business_k <- ifelse(student_parent$feeling_big_business_k <= 20, 1, ifelse(student_parent$feeling_big_business_k <= 
                                                                                                         40, 2, ifelse(student_parent$feeling_big_business_k <= 60, 3, ifelse(student_parent$feeling_big_business_k <= 80, 4, 5))))
student_parent$feeling_big_business_f <- ifelse(student_parent$feeling_big_business_f <= 20, 1, ifelse(student_parent$feeling_big_business_f <= 
                                                                                                         40, 2, ifelse(student_parent$feeling_big_business_f <= 60, 3, ifelse(student_parent$feeling_big_business_f <= 80, 4, 5))))
subset_couple_1965_fathers$feeling_unions <- ifelse(subset_couple_1965_fathers$feeling_unions <= 20, 1, ifelse(subset_couple_1965_fathers$feeling_unions <= 
                                                                                                                 40, 2, ifelse(subset_couple_1965_fathers$feeling_unions <= 60, 3, ifelse(subset_couple_1965_fathers$feeling_unions <= 80, 4, 5))))
subset_couple_1965_mothers$feeling_unions <- ifelse(subset_couple_1965_mothers$feeling_unions <= 20, 1, ifelse(subset_couple_1965_mothers$feeling_unions <= 
                                                                                                                 40, 2, ifelse(subset_couple_1965_mothers$feeling_unions <= 60, 3, ifelse(subset_couple_1965_mothers$feeling_unions <= 80, 4, 5))))
subset_couple_1965_fathers$feeling_unions_k <- ifelse(subset_couple_1965_fathers$feeling_unions_k <= 20, 1, ifelse(subset_couple_1965_fathers$feeling_unions_k <= 
                                                                                                                     40, 2, ifelse(subset_couple_1965_fathers$feeling_unions_k <= 60, 3, ifelse(subset_couple_1965_fathers$feeling_unions_k <= 80, 4, 5))))
subset_couple_1965_mothers$feeling_unions_k <- ifelse(subset_couple_1965_mothers$feeling_unions_k <= 20, 1, ifelse(subset_couple_1965_mothers$feeling_unions_k <= 
                                                                                                                     40, 2, ifelse(subset_couple_1965_mothers$feeling_unions_k <= 60, 3, ifelse(subset_couple_1965_mothers$feeling_unions_k <= 80, 4, 5))))
student_parent$feeling_unions_k <- ifelse(student_parent$feeling_unions_k <= 20, 1, ifelse(student_parent$feeling_unions_k <= 40, 2, ifelse(student_parent$feeling_unions_k <= 
                                                                                                                                              60, 3, ifelse(student_parent$feeling_unions_k <= 80, 4, 5))))
student_parent$feeling_unions_f <- ifelse(student_parent$feeling_unions_f <= 20, 1, ifelse(student_parent$feeling_unions_f <= 40, 2, ifelse(student_parent$feeling_unions_f <= 
                                                                                                                                              60, 3, ifelse(student_parent$feeling_unions_f <= 80, 4, 5))))
###################### 
subset_couple_1965_fathers$feeling_jews <- ifelse(subset_couple_1965_fathers$feeling_jews <= 20, 1, ifelse(subset_couple_1965_fathers$feeling_jews <= 
                                                                                                             40, 2, ifelse(subset_couple_1965_fathers$feeling_jews <= 60, 3, ifelse(subset_couple_1965_fathers$feeling_jews <= 80, 4, 5))))
subset_couple_1965_mothers$feeling_jews <- ifelse(subset_couple_1965_mothers$feeling_jews <= 20, 1, ifelse(subset_couple_1965_mothers$feeling_jews <= 
                                                                                                             40, 2, ifelse(subset_couple_1965_mothers$feeling_jews <= 60, 3, ifelse(subset_couple_1965_mothers$feeling_jews <= 80, 4, 5))))
subset_couple_1965_fathers$feeling_jews_k <- ifelse(subset_couple_1965_fathers$feeling_jews_k <= 20, 1, ifelse(subset_couple_1965_fathers$feeling_jews_k <= 
                                                                                                                 40, 2, ifelse(subset_couple_1965_fathers$feeling_jews_k <= 60, 3, ifelse(subset_couple_1965_fathers$feeling_jews_k <= 80, 4, 5))))
subset_couple_1965_mothers$feeling_jews_k <- ifelse(subset_couple_1965_mothers$feeling_jews_k <= 20, 1, ifelse(subset_couple_1965_mothers$feeling_jews_k <= 
                                                                                                                 40, 2, ifelse(subset_couple_1965_mothers$feeling_jews_k <= 60, 3, ifelse(subset_couple_1965_mothers$feeling_jews_k <= 80, 4, 5))))
student_parent$feeling_jews_k <- ifelse(student_parent$feeling_jews_k <= 20, 1, ifelse(student_parent$feeling_jews_k <= 40, 2, ifelse(student_parent$feeling_jews_k <= 
                                                                                                                                        60, 3, ifelse(student_parent$feeling_jews_k <= 80, 4, 5))))
student_parent$feeling_jews_f <- ifelse(student_parent$feeling_jews_f <= 20, 1, ifelse(student_parent$feeling_jews_f <= 40, 2, ifelse(student_parent$feeling_jews_f <= 
                                                                                                                                        60, 3, ifelse(student_parent$feeling_jews_f <= 80, 4, 5))))
subset_couple_1965_fathers$feeling_whites <- ifelse(subset_couple_1965_fathers$feeling_whites <= 20, 1, ifelse(subset_couple_1965_fathers$feeling_whites <= 
                                                                                                                 40, 2, ifelse(subset_couple_1965_fathers$feeling_whites <= 60, 3, ifelse(subset_couple_1965_fathers$feeling_whites <= 80, 4, 5))))
subset_couple_1965_mothers$feeling_whites <- ifelse(subset_couple_1965_mothers$feeling_whites <= 20, 1, ifelse(subset_couple_1965_mothers$feeling_whites <= 
                                                                                                                 40, 2, ifelse(subset_couple_1965_mothers$feeling_whites <= 60, 3, ifelse(subset_couple_1965_mothers$feeling_whites <= 80, 4, 5))))
subset_couple_1965_fathers$feeling_whites_k <- ifelse(subset_couple_1965_fathers$feeling_whites_k <= 20, 1, ifelse(subset_couple_1965_fathers$feeling_whites_k <= 
                                                                                                                     40, 2, ifelse(subset_couple_1965_fathers$feeling_whites_k <= 60, 3, ifelse(subset_couple_1965_fathers$feeling_whites_k <= 80, 4, 5))))
subset_couple_1965_mothers$feeling_whites_k <- ifelse(subset_couple_1965_mothers$feeling_whites_k <= 20, 1, ifelse(subset_couple_1965_mothers$feeling_whites_k <= 
                                                                                                                     40, 2, ifelse(subset_couple_1965_mothers$feeling_whites_k <= 60, 3, ifelse(subset_couple_1965_mothers$feeling_whites_k <= 80, 4, 5))))
student_parent$feeling_whites_k <- ifelse(student_parent$feeling_whites_k <= 20, 1, ifelse(student_parent$feeling_whites_k <= 40, 2, ifelse(student_parent$feeling_whites_k <= 
                                                                                                                                              60, 3, ifelse(student_parent$feeling_whites_k <= 80, 4, 5))))
student_parent$feeling_whites_f <- ifelse(student_parent$feeling_whites_f <= 20, 1, ifelse(student_parent$feeling_whites_f <= 40, 2, ifelse(student_parent$feeling_whites_f <= 
                                                                                                                                              60, 3, ifelse(student_parent$feeling_whites_f <= 80, 4, 5))))
subset_couple_1965_fathers$feeling_catholics <- ifelse(subset_couple_1965_fathers$feeling_catholics <= 20, 1, ifelse(subset_couple_1965_fathers$feeling_catholics <= 
                                                                                                                       40, 2, ifelse(subset_couple_1965_fathers$feeling_catholics <= 60, 3, ifelse(subset_couple_1965_fathers$feeling_catholics <= 80, 4, 5))))
subset_couple_1965_mothers$feeling_catholics <- ifelse(subset_couple_1965_mothers$feeling_catholics <= 20, 1, ifelse(subset_couple_1965_mothers$feeling_catholics <= 
                                                                                                                       40, 2, ifelse(subset_couple_1965_mothers$feeling_catholics <= 60, 3, ifelse(subset_couple_1965_mothers$feeling_catholics <= 80, 4, 5))))
subset_couple_1965_fathers$feeling_catholics_k <- ifelse(subset_couple_1965_fathers$feeling_catholics_k <= 20, 1, ifelse(subset_couple_1965_fathers$feeling_catholics_k <= 
                                                                                                                           40, 2, ifelse(subset_couple_1965_fathers$feeling_catholics_k <= 60, 3, ifelse(subset_couple_1965_fathers$feeling_catholics_k <= 80, 4, 5))))
subset_couple_1965_mothers$feeling_catholics_k <- ifelse(subset_couple_1965_mothers$feeling_catholics_k <= 20, 1, ifelse(subset_couple_1965_mothers$feeling_catholics_k <= 
                                                                                                                           40, 2, ifelse(subset_couple_1965_mothers$feeling_catholics_k <= 60, 3, ifelse(subset_couple_1965_mothers$feeling_catholics_k <= 80, 4, 5))))
student_parent$feeling_catholics_k <- ifelse(student_parent$feeling_catholics_k <= 20, 1, ifelse(student_parent$feeling_catholics_k <= 40, 2, ifelse(student_parent$feeling_catholics_k <= 
                                                                                                                                                       60, 3, ifelse(student_parent$feeling_catholics_k <= 80, 4, 5))))
student_parent$feeling_catholics_f <- ifelse(student_parent$feeling_catholics_f <= 20, 1, ifelse(student_parent$feeling_catholics_f <= 40, 2, ifelse(student_parent$feeling_catholics_f <= 
                                                                                                                                                       60, 3, ifelse(student_parent$feeling_catholics_f <= 80, 4, 5))))
########################################################################################################### latent party models code race for latent party model
subset_couple_1965_fathers$race <- recode(as.numeric(subset_couple_1965_fathers$V584), "1=0;2=1;8=0;9=NA")
subset_couple_1965_mothers$race <- recode(as.numeric(subset_couple_1965_mothers$V584), "1=0;2=1;8=0;9=NA")
subset_couple_1965_fathers$race_kid <- recode(as.numeric(subset_couple_1965_fathers$V297), "1=0;2=1;8=0;9=NA")
subset_couple_1965_mothers$race_kid <- recode(as.numeric(subset_couple_1965_mothers$V297), "1=0;2=1;8=0;9=NA")
student_parent$race_f <- recode(as.numeric(student_parent$V584), "1=0;2=1;8=0;9=NA")
student_parent$race_k <- recode(as.numeric(student_parent$V297), "1=0;2=1;8=0;9=NA")
subset_couple_1965_mothers$latent_partisan_attitudes <- fscores(mirt(subset_couple_1965_mothers[, c("Party", "race", "feeling_jews", "feeling_whites", 
                                                                                                    "feeling_catholics", "feeling_unions", "feeling_big_business")], 1, survey.weights = subset_couple_1965_fathers$V322), full.scores = T)
subset_couple_1965_fathers$latent_partisan_attitudes <- fscores(mirt(subset_couple_1965_fathers[, c("Party", "race", "feeling_jews", "feeling_whites", 
                                                                                                    "feeling_catholics", "feeling_unions", "feeling_big_business")], 1, survey.weights = subset_couple_1965_fathers$V322), full.scores = T)
cor.test(subset_couple_1965_mothers$latent_partisan_attitudes, subset_couple_1965_fathers$latent_partisan_attitudes)
# intergenerational
latent_partisan_attitudes_f = fscores(mirt(rbind(as.matrix(student_parent[, c("Party", "race_f", "feeling_jews_f", "feeling_whites_f", "feeling_catholics_f", 
                                                                              "feeling_unions_f", "feeling_big_business_f")]), as.matrix(subset_couple_1965_fathers[, c("Party", "race", "feeling_jews", "feeling_whites", 
                                                                                                                                                                        "feeling_catholics", "feeling_unions", "feeling_big_business")]), as.matrix(subset_couple_1965_mothers[, c("Party", "race", "feeling_jews", 
                                                                                                                                                                                                                                                                                   "feeling_whites", "feeling_catholics", "feeling_unions", "feeling_big_business")])), 1, survey.weights = c(student_parent$V321, subset_couple_1965_fathers$V321, 
                                                                                                                                                                                                                                                                                                                                                                                              subset_couple_1965_mothers$V321)), full.scores = T)
latent_partisan_attitudes_k = fscores(mirt(rbind(as.matrix(student_parent[, c("Party_kid", "race_k", "feeling_jews_k", "feeling_whites_k", "feeling_catholics_k", 
                                                                              "feeling_unions_k", "feeling_big_business_k")]), as.matrix(subset_couple_1965_fathers[, c("Party_kid", "race_kid", "feeling_jews_k", "feeling_whites_k", 
                                                                                                                                                                        "feeling_catholics_k", "feeling_unions_k", "feeling_big_business_k")]), as.matrix(subset_couple_1965_mothers[, c("Party_kid", "race_kid", "feeling_jews_k", 
                                                                                                                                                                                                                                                                                         "feeling_whites_k", "feeling_catholics_k", "feeling_unions_k", "feeling_big_business_k")])), 1, survey.weights = c(student_parent$V321, subset_couple_1965_fathers$V321, 
                                                                                                                                                                                                                                                                                                                                                                                                            subset_couple_1965_mothers$V321)), full.scores = T)
cor.test(latent_partisan_attitudes_f, latent_partisan_attitudes_k)
t.test(abs(latent_partisan_attitudes_f - latent_partisan_attitudes_k), na.rm = T)
# new_data=rbind(student_parent[,c('feeling_big_business_k','feeling_big_business_f', 'feeling_unions_k','feeling_unions_f','school_prayer_kid',
# 'school_prayer_family', 'speech_church_kid', 'speech_church_family', 'communist_kid', 'communist_family')],
# subset_couple_1965_fathers[,c('feeling_big_business_k','feeling_big_business_f', 'feeling_unions_k','feeling_unions_f','school_prayer_kid',
# 'school_prayer', 'speech_church_kid', 'speech_church', 'communist_kid', 'communist')])
subset_couple_1965_mothers$latent_ideology <- fscores(mirt(subset_couple_1965_mothers[, c("school_prayer", "speech_church", "communist", "integration")], 
                                                           1, survey.weights = subset_couple_1965_fathers$V322), full.scores = T)
subset_couple_1965_fathers$latent_ideology <- fscores(mirt(subset_couple_1965_fathers[, c("school_prayer", "speech_church", "communist", "integration")], 
                                                           1, survey.weights = subset_couple_1965_fathers$V322), full.scores = T)
student_parent$latent_ideology <- append(fscores(mirt(student_parent[-1072, c("school_prayer_family", "speech_church_family", "communist_family", 
                                                                              "integration_family")], 1, survey.weights = student_parent$V321[-1072]), full.scores = T), NA, after = 1071)
cor.test(subset_couple_1965_mothers$latent_ideology, subset_couple_1965_fathers$latent_ideology)
# create new data for student parent comparison
ideology_f = fscores(mirt(rbind(as.matrix(student_parent[, c("school_prayer_family", "speech_church_family", "communist_family")]), as.matrix(subset_couple_1965_fathers[, 
                                                                                                                                                                         c("school_prayer", "speech_church", "communist")]), as.matrix(subset_couple_1965_mothers[, c("school_prayer", "speech_church", "communist")]))[-c(1072, 
                                                                                                                                                                                                                                                                                                                           1551), ], 1, survey.weights = c(student_parent$V321, subset_couple_1965_fathers$V321, subset_couple_1965_mothers$V321)[-c(1072, 1551)]), full.scores = T)
ideology_k = fscores(mirt(rbind(as.matrix(student_parent[, c("school_prayer_kid", "speech_church_kid", "communist_kid")]), as.matrix(subset_couple_1965_fathers[, 
                                                                                                                                                                c("school_prayer_kid", "speech_church_kid", "communist_kid")]), as.matrix(subset_couple_1965_mothers[, c("school_prayer_kid", "speech_church_kid", 
                                                                                                                                                                                                                                                                         "communist_kid")]))[-c(1072, 1551), ], 1, survey.weights = c(student_parent$V321, subset_couple_1965_fathers$V321, subset_couple_1965_mothers$V321)[-c(1072, 
                                                                                                                                                                                                                                                                                                                                                                                                                                1551)]), full.scores = T)
cor.test(ideology_k, ideology_f)
subset_couple_1965_mothers$latent_ideology_factor <- as.numeric(ifelse(subset_couple_1965_mothers$latent_ideology <= quantile(subset_couple_1965_mothers$latent_ideology, 
                                                                                                                              1/3), 1, ifelse(subset_couple_1965_mothers$latent_ideology <= quantile(subset_couple_1965_mothers$latent_ideology, 2/3), 2, 3)))
subset_couple_1965_fathers$latent_ideology_factor <- as.numeric(ifelse(subset_couple_1965_fathers$latent_ideology <= quantile(subset_couple_1965_fathers$latent_ideology, 
                                                                                                                              1/3), 1, ifelse(subset_couple_1965_fathers$latent_ideology <= quantile(subset_couple_1965_fathers$latent_ideology, 2/3), 2, 3)))
student_parent$latent_ideology_factor <- as.numeric(ifelse(student_parent$latent_ideology <= quantile(student_parent$latent_ideology, 1/3, na.rm = T), 
                                                           1, ifelse(student_parent$latent_ideology <= quantile(student_parent$latent_ideology, 2/3, na.rm = T), 2, 3)))
# Sorting and latent ideology correlation
subset_couple_1965_mothers$sorted <- with(subset_couple_1965_mothers, ifelse((Party < 4 & as.numeric(latent_ideology_factor) < 2) | (as.numeric(Party) > 
                                                                                                                                       4 & as.numeric(latent_ideology_factor) > 2) | (as.numeric(Party) == 4 & as.numeric(latent_ideology_factor) == 2), 1, ifelse((as.numeric(Party) > 
                                                                                                                                                                                                                                                                      4 & as.numeric(latent_ideology_factor) < 2) | (as.numeric(Party) < 4 & as.numeric(latent_ideology_factor) > 2), -1, ifelse((as.numeric(Party) > 
                                                                                                                                                                                                                                                                                                                                                                                                    4 & as.numeric(latent_ideology_factor) == 2) | (as.numeric(Party) < 4 & as.numeric(latent_ideology_factor) == 2) | (as.numeric(Party) == 4 & 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          as.numeric(latent_ideology_factor) > 2) | (as.numeric(Party) == 4 & as.numeric(latent_ideology_factor) < 2), 0, NA))))
subset_couple_1965_fathers$sorted <- with(subset_couple_1965_fathers, ifelse((Party < 4 & as.numeric(latent_ideology_factor) < 2) | (as.numeric(Party) > 
                                                                                                                                       4 & as.numeric(latent_ideology_factor) > 2) | (as.numeric(Party) == 4 & as.numeric(latent_ideology_factor) == 1), 1, ifelse((as.numeric(Party) > 
                                                                                                                                                                                                                                                                      4 & as.numeric(latent_ideology_factor) < 2) | (as.numeric(Party) < 4 & as.numeric(latent_ideology_factor) > 2), -1, ifelse((as.numeric(Party) > 
                                                                                                                                                                                                                                                                                                                                                                                                    4 & as.numeric(latent_ideology_factor) == 2) | (as.numeric(Party) < 4 & as.numeric(latent_ideology_factor) == 2) | (as.numeric(Party) == 4 & 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          as.numeric(latent_ideology_factor) > 2) | (as.numeric(Party) == 4 & as.numeric(latent_ideology_factor) < 2), 0, NA))))
student_parent$sorted <- with(student_parent, ifelse((Party < 4 & as.numeric(latent_ideology_factor) < 2) | (as.numeric(Party) > 4 & as.numeric(latent_ideology_factor) > 
                                                                                                               2) | (as.numeric(Party) == 4 & as.numeric(latent_ideology_factor) == 1), 1, ifelse((as.numeric(Party) > 4 & as.numeric(latent_ideology_factor) < 
                                                                                                                                                                                                     2) | (as.numeric(Party) < 4 & as.numeric(latent_ideology_factor) > 2), -1, ifelse((as.numeric(Party) > 4 & as.numeric(latent_ideology_factor) == 
                                                                                                                                                                                                                                                                                          2) | (as.numeric(Party) < 4 & as.numeric(latent_ideology_factor) == 2) | (as.numeric(Party) == 4 & as.numeric(latent_ideology_factor) > 2) | 
                                                                                                                                                                                                                                                                                         (as.numeric(Party) == 4 & as.numeric(latent_ideology_factor) < 2), 0, NA))))
xtable(crosstab(subset_couple_1965_mothers$sorted, subset_couple_1965_fathers$sorted, weight = subset_couple_1965_fathers$V322, plot = F, prop.t = F))
round(60/486 * 100, 2)  #11.04% as completely sorted
# transmission among sorted couples and sorted-one-parent households
new_data <- rbind(as.matrix(student_parent[, c("Party_kid", "Party")]), as.matrix(subset_couple_1965_mothers[, c("Party_kid", "Party")]), as.matrix(subset_couple_1965_fathers[, 
                                                                                                                                                                               c("Party_kid", "Party")]))
polychor(new_data[, 1], new_data[, 2])
subset_couple_1965_fathers$party_latent_independents <- NA
subset_couple_1965_fathers$party_latent_independents[subset_couple_1965_fathers$latent_party < quantile(subset_couple_1965_fathers$latent_party, 
                                                                                                        0.5)] <- 3
subset_couple_1965_fathers$party_latent_independents[subset_couple_1965_fathers$latent_party > quantile(subset_couple_1965_fathers$latent_party, 
                                                                                                        0.5)] <- 5
subset_couple_1965_fathers$party_latent_independents[is.na(subset_couple_1965_fathers$party_latent_independents)] <- sample(c(3, 5), 1)
subset_couple_1965_mothers$party_latent_independents <- NA
subset_couple_1965_mothers$party_latent_independents[subset_couple_1965_mothers$latent_party < quantile(subset_couple_1965_mothers$latent_party, 
                                                                                                        0.5)] <- 3
subset_couple_1965_mothers$party_latent_independents[subset_couple_1965_mothers$latent_party > quantile(subset_couple_1965_mothers$latent_party, 
                                                                                                        0.5)] <- 5
subset_couple_1965_mothers$party_latent_independents[is.na(subset_couple_1965_mothers$party_latent_independents)] <- sample(c(3, 5), 1)
subset_couple_1965_mothers$party_latent_independents <- ifelse(subset_couple_1965_mothers$Party == 4, subset_couple_1965_mothers$party_latent_independents, 
                                                               subset_couple_1965_mothers$Party)
subset_couple_1965_fathers$party_latent_independents <- ifelse(subset_couple_1965_fathers$Party == 4, subset_couple_1965_fathers$party_latent_independents, 
                                                               subset_couple_1965_fathers$Party)
# PID homogeneity (1,2,3)(5,6,7)
round((56 + 39 + 6 + 26 + 59 + 7 + 5 + 17 + 17 + 8 + 6 + 5 + 6 + 37 + 12 + 4 + 17 + 32) * 100/425, 2)
crosstab(subset_couple_1965_mothers$party_latent_independents, subset_couple_1965_fathers$party_latent_independents, weight = subset_couple_1965_fathers$V322, 
         plot = F)
round((56 + 26 + 5 + 39 + 59 + 17 + 9 + 10 + 29 + 15 + 8 + 5 + 9 + 37 + 12 + 8 + 17 + 32)/506, 2)
# homogeneity (1,2,3) (4,) and (5, 6, 7)) Jennings
(56 + 26 + 5 + 39 + 59 + 17 + 6 + 7 + 17 + 8 + 6 + 5 + 6 + 37 + 12 + 4 + 17 + 32)/507  #70.67%
# mine
(56 + 26 + 5 + 39 + 59 + 17 + 6 + 7 + 17 + 8 + 6 + 5 + 6 + 37 + 12 + 4 + 17 + 32)/507  #70.81%
# disagreement
round((0 + 1 + 1 + 4 + 13 + 3 + 9 + 5 + 3 + 1 + 4 + 0 + 1 + 13 + 0 + 5 + 2 + 1)/507 * 100, 2)  #13.02%
# mine
(56 + 39 + 6 + 26 + 59 + 7 + 5 + 17 + 17 + 8 + 6 + 4 + 6 + 37 + 17 + 5 + 12 + 32)/425  #84.5%
# latent
(56 + 26 + 8 + 39 + 59 + 8 + 11 + 33 + 21 + 15 + 7 + 5 + 8 + 37 + 12 + 6 + 17 + 32)/507  #78..90
############## religion###################### V266 church attendance, V558 church attendance
subset_couple_1965_fathers$religious_attendance <- recode(subset_couple_1965_fathers$V558, "1=4;2=3;3=2;4=1;9=NA;0=NA")
subset_couple_1965_fathers$religious_attendance_kids <- recode(subset_couple_1965_fathers$V266, "1=4;2=3;3=2;4=1;9=NA;0=NA")
subset_couple_1965_mothers$religious_attendance <- recode(subset_couple_1965_mothers$V558, "1=4;2=3;3=2;4=1;9=NA;0=NA")
subset_couple_1965_mothers$religious_attendance_kids <- recode(subset_couple_1965_mothers$V266, "1=4;2=3;3=2;4=1;9=NA;0=NA")
student_parent$religious_attendance_parent <- recode(student_parent$V558, "1=4;2=3;3=2;4=1;9=NA;0=NA")
student_parent$religious_attendance_kid <- recode(student_parent$V266, "1=4;2=3;3=2;4=1;9=NA;0=NA")
polychoric(cbind(subset_couple_1965_mothers$religious_attendance, subset_couple_1965_fathers$religious_attendance), weight = subset_couple_1965_fathers$V322)$rho
# now intergenerational
new_data <- rbind(as.matrix(subset_couple_1965_mothers[, c("religious_attendance", "religious_attendance_kids", "V321")]), as.matrix(subset_couple_1965_fathers[, 
                                                                                                                                                                c("religious_attendance", "religious_attendance_kids", "V321")]), as.matrix(student_parent[, c("religious_attendance_parent", "religious_attendance_kid", 
                                                                                                                                                                                                                                                               "V321")]))
polychoric(cbind(new_data[, 1], new_data[, 2]), weight = new_data[, 3])$rho
# bible for word V267 bible word for word kids, V559 bible word for word adults
subset_couple_1965_fathers$bible <- recode(subset_couple_1965_fathers$V559, "1=4;2=3;3=2;4=1;9=NA;8=NA")
subset_couple_1965_fathers$bible_kids <- recode(subset_couple_1965_fathers$V267, "1=4;2=3;3=2;4=1;9=NA;8=NA")
subset_couple_1965_mothers$bible <- recode(subset_couple_1965_mothers$V559, "1=4;2=3;3=2;4=1;9=NA;8=NA")
subset_couple_1965_mothers$bible_kids <- recode(subset_couple_1965_mothers$V267, "1=4;2=3;3=2;4=1;9=NA;8=NA")
student_parent$bible_parent <- recode(student_parent$V559, "1=4;2=3;3=2;4=1;9=NA;8=NA")
student_parent$bible_kid <- recode(student_parent$V267, "1=4;2=3;3=2;4=1;9=NA;8=NA")
polychoric(cbind(subset_couple_1965_mothers$bible, subset_couple_1965_fathers$bible), weight = subset_couple_1965_fathers$V322)$rho
# intergenerational
new_data <- rbind(as.matrix(subset_couple_1965_mothers[, c("bible", "bible_kids", "V321")]), as.matrix(subset_couple_1965_fathers[, c("bible", "bible_kids", 
                                                                                                                                      "V321")]), as.matrix(student_parent[, c("bible_parent", "bible_kid", "V321")]))
polychoric(cbind(new_data[, 1], new_data[, 2]), weight = new_data[, 3])$rho
# latent
subset_couple_1965_mothers$religious_attendance_latent <- fscores(mirt(with(subset_couple_1965_mothers, cbind(bible, religious_attendance, feeling_jews, 
                                                                                                              feeling_catholics)), 1, itemtype = "graded", survey.weights = subset_couple_1965_fathers$V322), full.scores = TRUE, scores.only = TRUE)
subset_couple_1965_fathers$religious_attendance_latent <- fscores(mirt(with(subset_couple_1965_fathers, cbind(bible, religious_attendance, feeling_jews, 
                                                                                                              feeling_catholics)), 1, itemtype = "graded", survey.weights = subset_couple_1965_fathers$V322), full.scores = TRUE, scores.only = TRUE)
cor.test(subset_couple_1965_fathers$religious_attendance_latent, subset_couple_1965_mothers$religious_attendance_latent)
# .42 latent intergenerational
latent_religious_parent <- fscores(mirt(rbind(as.matrix(student_parent[, c("bible_parent", "religious_attendance_parent", "feeling_jews_f", "feeling_catholics_f")]), 
                                              as.matrix(subset_couple_1965_fathers[, c("bible", "religious_attendance", "feeling_jews", "feeling_catholics")]), as.matrix(subset_couple_1965_mothers[, 
                                                                                                                                                                                                     c("bible", "religious_attendance", "feeling_jews", "feeling_catholics")])), 1, survey.weights = c(student_parent$V321, subset_couple_1965_fathers$V321, 
                                                                                                                                                                                                                                                                                                       subset_couple_1965_mothers$V321)), full.scores = T)
latent_religious_kid <- fscores(mirt(rbind(as.matrix(student_parent[, c("bible_kid", "religious_attendance_kid", "feeling_jews_k", "feeling_catholics_k")]), 
                                           as.matrix(subset_couple_1965_fathers[, c("bible_kids", "religious_attendance_kids", "feeling_jews_k", "feeling_catholics_k")]), as.matrix(subset_couple_1965_mothers[, 
                                                                                                                                                                                                                c("bible_kids", "religious_attendance_kids", "feeling_jews_k", "feeling_catholics_k")])), 1, survey.weights = c(student_parent$V321, subset_couple_1965_fathers$V321, 
                                                                                                                                                                                                                                                                                                                                subset_couple_1965_mothers$V321)), full.scores = T)
cor.test(latent_religious_parent, latent_religious_kid)
# 0.23 latent news interest, trust, discsussion non political - for parents only
subset_couple_1965_mothers$teenagers <- recode(as.numeric(subset_couple_1965_mothers$V330), "5=3;8=NA;9=NA")
subset_couple_1965_fathers$teenagers <- recode(as.numeric(subset_couple_1965_fathers$V330), "5=3;8=NA;9=NA")
polychoric(cbind(subset_couple_1965_mothers$teenagers, subset_couple_1965_fathers$teenagers), weight = subset_couple_1965_fathers$V322)$rho
subset_couple_1965_mothers$child_popular <- recode(as.numeric(subset_couple_1965_mothers$V332), "0=3;6=4;8=NA;9=NA")
subset_couple_1965_fathers$child_popular <- recode(as.numeric(subset_couple_1965_fathers$V332), "0=3;6=4;8=NA;9=NA")
polychoric(cbind(subset_couple_1965_mothers$child_popular, subset_couple_1965_fathers$child_popular), weight = subset_couple_1965_fathers$V322)$rho
subset_couple_1965_mothers$child_religious <- recode(as.numeric(subset_couple_1965_mothers$V333), "0=3;6=4;8=NA;9=NA")
subset_couple_1965_fathers$child_religious <- recode(as.numeric(subset_couple_1965_fathers$V333), "0=3;6=4;8=NA;9=NA")
polychoric(cbind(subset_couple_1965_mothers$child_religious, subset_couple_1965_fathers$child_religious), weight = subset_couple_1965_fathers$V322)$rho
subset_couple_1965_mothers$child_successfull <- recode(as.numeric(subset_couple_1965_mothers$V334), "0=3;6=4;8=NA;9=NA")
subset_couple_1965_fathers$child_successfull <- recode(as.numeric(subset_couple_1965_fathers$V334), "0=3;6=4;8=NA;9=NA")
polychoric(cbind(subset_couple_1965_mothers$child_successfull, subset_couple_1965_fathers$child_successfull), weight = subset_couple_1965_fathers$V322)$rho
subset_couple_1965_mothers$child_tolerant <- recode(as.numeric(subset_couple_1965_mothers$V335), "0=3;6=4;8=NA;9=NA")
subset_couple_1965_fathers$child_tolerant <- recode(as.numeric(subset_couple_1965_fathers$V335), "0=3;6=4;8=NA;9=NA")
polychoric(cbind(subset_couple_1965_mothers$child_tolerant, subset_couple_1965_fathers$child_tolerant), weight = subset_couple_1965_fathers$V322)$rho
subset_couple_1965_mothers$child_cultured <- recode(as.numeric(subset_couple_1965_mothers$V336), "0=3;6=4;8=NA;9=NA")
subset_couple_1965_fathers$child_cultured <- recode(as.numeric(subset_couple_1965_fathers$V336), "0=3;6=4;8=NA;9=NA")
polychoric(cbind(subset_couple_1965_mothers$child_cultured, subset_couple_1965_fathers$child_cultured), weight = subset_couple_1965_fathers$V322)$rho
subset_couple_1965_mothers$child_improve <- recode(as.numeric(subset_couple_1965_mothers$V337), "0=3;6=4;8=NA;9=NA")
subset_couple_1965_fathers$child_improve <- recode(as.numeric(subset_couple_1965_fathers$V337), "0=3;6=4;8=NA;9=NA")
polychoric(cbind(subset_couple_1965_mothers$child_improve, subset_couple_1965_fathers$child_improve), weight = subset_couple_1965_fathers$V322)$rho
subset_couple_1965_mothers$non_political <- append(fscores(mirt(subset_couple_1965_mothers[-253, c("child_popular", "child_religious", "child_successfull", 
                                                                                                   "child_tolerant", "child_cultured", "child_improve")], 1, survey.weights = subset_couple_1965_fathers$V322[-253]), full.scores = T), NA, 252)
subset_couple_1965_fathers$non_political <- append(append(append(append(append(append(fscores(mirt(subset_couple_1965_fathers[-c(65, 154, 232, 270, 
                                                                                                                                 338, 389), c("child_popular", "child_religious", "child_successfull", "child_tolerant", "child_cultured", "child_improve")], 1, survey.weights = subset_couple_1965_fathers$V322[-c(65, 
                                                                                                                                                                                                                                                                                                                     154, 232, 270, 338, 389)]), full.scores = T), NA, 64), NA, 153), NA, 231), NA, 269), NA, 337), NA, 388)
cor.test(subset_couple_1965_mothers$non_political, subset_couple_1965_fathers$non_political, use = "complete.obs")