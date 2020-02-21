# 1997 data setwd('C:/Users/tobia/Dropbox/Partisan Homogeneity Project/Jennings Data/Parent 1965-1982/')
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

# 7-way PID spouse_gen_complete<-read.spss('panel1997.sav',use.value.labels = F,to.data.frame = T)
spouse_gen_complete$Party_p1 <- NA
spouse_gen_complete$Party_p1[spouse_gen_complete$v9300 < 3] <- "Dem"
spouse_gen_complete$Party_p1[spouse_gen_complete$v9300 == 3 & spouse_gen_complete$v9301 == 2] <- "Dem"
spouse_gen_complete$Party_p1[spouse_gen_complete$v9300 == 3 & spouse_gen_complete$v9301 == 3] <- "Ind"
spouse_gen_complete$Party_p1[spouse_gen_complete$v9300 == 3 & spouse_gen_complete$v9301 == 4] <- "Rep"
spouse_gen_complete$Party_p1[spouse_gen_complete$v9300 > 3] <- "Rep"

spouse_gen_complete$Party_p2 <- NA
spouse_gen_complete$Party_p2[spouse_gen_complete$V5750 == 5 & spouse_gen_complete$V5752 == 1] <- "Dem"
spouse_gen_complete$Party_p2[spouse_gen_complete$V5750 == 5 & spouse_gen_complete$V5752 == 5] <- "Dem"
spouse_gen_complete$Party_p2[(spouse_gen_complete$V5750 == 2 | spouse_gen_complete$V5750 == 3 | spouse_gen_complete$V5750 == 7) & spouse_gen_complete$V5753 == 
                               5] <- "Dem"
spouse_gen_complete$Party_p2[(spouse_gen_complete$V5750 == 2 | spouse_gen_complete$V5750 == 3 | spouse_gen_complete$V5750 == 7) & spouse_gen_complete$V5753 == 
                               3] <- "Ind"
spouse_gen_complete$Party_p2[(spouse_gen_complete$V5750 == 2 | spouse_gen_complete$V5750 == 3 | spouse_gen_complete$V5750 == 7) & spouse_gen_complete$V5753 == 
                               1] <- "Rep"
spouse_gen_complete$Party_p2[spouse_gen_complete$V5750 == 1 & spouse_gen_complete$V5751 == 5] <- "Rep"
spouse_gen_complete$Party_p2[spouse_gen_complete$V5750 == 1 & spouse_gen_complete$V5751 == 1] <- "Rep"

spouse_gen_complete$homogeneity <- NA
spouse_gen_complete$homogeneity[spouse_gen_complete$Party_p1 == "Dem" & spouse_gen_complete$Party_p2 == "Dem"] <- 1
spouse_gen_complete$homogeneity[spouse_gen_complete$Party_p1 == "Dem" & spouse_gen_complete$Party_p2 == "Rep"] <- 0
spouse_gen_complete$homogeneity[spouse_gen_complete$Party_p1 == "Rep" & spouse_gen_complete$Party_p2 == "Dem"] <- 0
spouse_gen_complete$homogeneity[spouse_gen_complete$Party_p1 == "Rep" & spouse_gen_complete$Party_p2 == "Rep"] <- 1
spouse_gen_complete$homogeneity[spouse_gen_complete$Party_p1 == "Ind" & spouse_gen_complete$Party_p2 == "Ind"] <- 1
spouse_gen_complete$homogeneity[spouse_gen_complete$Party_p1 == "Rep" & spouse_gen_complete$Party_p2 == "Ind"] <- 0
spouse_gen_complete$homogeneity[spouse_gen_complete$Party_p1 == "Ind" & spouse_gen_complete$Party_p2 == "Rep"] <- 0
spouse_gen_complete$homogeneity[spouse_gen_complete$Party_p1 == "Dem" & spouse_gen_complete$Party_p2 == "Ind"] <- 0
spouse_gen_complete$homogeneity[spouse_gen_complete$Party_p1 == "Ind" & spouse_gen_complete$Party_p2 == "Dem"] <- 0

mean(spouse_gen_complete$homogeneity, na.rm = T)


# no weigts due to messy sampling frame 66.44444 se
length(which(!is.na(spouse_gen_complete$homogeneity)))

sqrt(66.44444 * (100 - 66.44444)/450)
# 2.225898

spouse_gen_complete$outparty_p1 <- ifelse(spouse_gen_complete$Party_p1 == "Dem", spouse_gen_complete$v9653 - spouse_gen_complete$v9657, ifelse(spouse_gen_complete$Party_p1 == 
                                                                                                                                                 "Rep", spouse_gen_complete$v9657 - spouse_gen_complete$v9653, NA))
spouse_gen_complete$outideology_p1 <- ifelse(spouse_gen_complete$Party_p1 == "Dem", spouse_gen_complete$v9658 - spouse_gen_complete$v9655, ifelse(spouse_gen_complete$Party_p1 == 
                                                                                                                                                    "Rep", spouse_gen_complete$v9655 - spouse_gen_complete$v9658, NA))
spouse_gen_complete$outcandidate_p1 <- ifelse(spouse_gen_complete$Party_p1 == "Dem", spouse_gen_complete$v9666 - spouse_gen_complete$v9664, ifelse(spouse_gen_complete$Party_p1 == 
                                                                                                                                                     "Rep", spouse_gen_complete$v9664 - spouse_gen_complete$v9666, NA))
spouse_gen_complete$outparty_p2 <- ifelse(spouse_gen_complete$Party_p2 == "Dem", spouse_gen_complete$V5604 - spouse_gen_complete$V5609, ifelse(spouse_gen_complete$Party_p2 == 
                                                                                                                                                 "Rep", spouse_gen_complete$V5609 - spouse_gen_complete$V5604, NA))
spouse_gen_complete$outideology_p2 <- ifelse(spouse_gen_complete$Party_p2 == "Dem", spouse_gen_complete$V5610 - spouse_gen_complete$V5606, ifelse(spouse_gen_complete$Party_p2 == 
                                                                                                                                                    "Rep", spouse_gen_complete$V5606 - spouse_gen_complete$V5610, NA))
spouse_gen_complete$outcandidate_p2 <- ifelse(spouse_gen_complete$Party_p2 == "Dem", spouse_gen_complete$V5624 - spouse_gen_complete$V5622, ifelse(spouse_gen_complete$Party_p2 == 
                                                                                                                                                     "Rep", spouse_gen_complete$V5622 - spouse_gen_complete$V5624, NA))
summary(lm(c(outparty_p1, outparty_p2) ~ rep(homogeneity, 2), data = spouse_gen_complete))
summary(lm(c(outideology_p1, outideology_p2) ~ rep(homogeneity, 2), data = spouse_gen_complete))
summary(lm(c(outcandidate_p1, outcandidate_p2) ~ rep(homogeneity, 2), data = spouse_gen_complete))


########################################################################################## merge parent in original data with kids, only parent-kids relationships of one parent-dyad kids<-read.csv('03926-0001-spss.csv')
########################################################################################## parents<-read.dta('04023-0001-Data.dta')
parents = parents[!parents$V5001 %in% spouse_gen_complete$V5001, ]
kids$V7002 = substr(kids$V7002, 1, nchar(kids$V7002) - 1)  #create matchable substring according to 03926 codebook

kids <- data.table(kids)
setkey(kids, V7002)
parents = data.table(parents)
parents$V5002 <- as.character(parents$V5002)
setkey(parents, V5002)

intergenerational_one_parent_dyad = parents[kids, nomatch = 0]
spouse_gen_complete$V5002 <- as.character(spouse_gen_complete$V5002)
spouse_gen_complete = data.table(spouse_gen_complete)
setkey(spouse_gen_complete, V5002)
setkey(kids, V7002)
intergenerational_two_parent_dyad = kids[spouse_gen_complete, nomatch = 0]
intergenerational_two_parent_dyad <- data.frame(intergenerational_two_parent_dyad)
intergenerational_one_parent_dyad <- data.frame(intergenerational_one_parent_dyad)
# 7-way PID, strong dem-strong rep
intergenerational_one_parent_dyad$pid_parent <- recode(as.numeric(intergenerational_one_parent_dyad$V5754), "1='Dem';2='Dem';3='Dem';4='Ind';5='Rep';6='Rep';7='Rep';9=NA")

intergenerational_one_parent_dyad$pid_kid <- recode(intergenerational_one_parent_dyad$PID3GEN, "0='Dem';1='Dem';2='Dem';3='Ind';4='Rep';5='Rep';6='Rep';9=NA")
intergenerational_two_parent_dyad$pid_kid <- recode(intergenerational_two_parent_dyad$PID3GEN, "0='Dem';1='Dem';2='Dem';3='Ind';4='Rep';5='Rep';6='Rep';9=NA")

new_data <- rbind(as.matrix(intergenerational_two_parent_dyad[, c("Party_p1", "pid_kid")]), as.matrix(intergenerational_two_parent_dyad[, c("Party_p2", 
                                                                                                                                            "pid_kid")]), as.matrix(intergenerational_one_parent_dyad[, c("pid_parent", "pid_kid")]))

new_data <- as.data.frame(new_data)

new_data$homogeneity <- NA
new_data$homogeneity[new_data$Party_p1 == "Dem" & new_data$pid_kid == "Dem"] <- 1
new_data$homogeneity[new_data$Party_p1 == "Dem" & new_data$pid_kid == "Rep"] <- 0
new_data$homogeneity[new_data$Party_p1 == "Rep" & new_data$pid_kid == "Dem"] <- 0
new_data$homogeneity[new_data$Party_p1 == "Rep" & new_data$pid_kid == "Rep"] <- 1
new_data$homogeneity[new_data$Party_p1 == "Ind" & new_data$pid_kid == "Ind"] <- 1
new_data$homogeneity[new_data$Party_p1 == "Rep" & new_data$pid_kid == "Ind"] <- 0
new_data$homogeneity[new_data$Party_p1 == "Ind" & new_data$pid_kid == "Rep"] <- 0
new_data$homogeneity[new_data$Party_p1 == "Dem" & new_data$pid_kid == "Ind"] <- 0
new_data$homogeneity[new_data$Party_p1 == "Ind" & new_data$pid_kid == "Dem"] <- 0

mean(new_data$homogeneity, na.rm = T)
length(which(!is.na(new_data$homogeneity)))

# 57.75792 se
sqrt(57.75792 * (100 - 57.75792)/1231)
# 1.407827