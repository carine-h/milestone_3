
library(data.table)
mround <- function(x, base) {
  base * round(x/base)
}
############################################################### Set up and LIbraries###################
library(car)
library(data.table)
library(foreach)
library(parallel)
library(doParallel)
# Problem of party in non-partisan registration states, such as MIssouri try California mark all states with partisan voter reg
library(descr)
library(xtable)
i = list.files("/share/data/voting-data/targetsmart/", pattern = "microsoft", full.names = T)



################### Set UP



aggregation <- function() {
  
  tab5rows = read.csv(i, nrows = 1, sep = "\t")
  classes <- rep("NULL", ncol(tab5rows))
  # use census ID from registration address 2012 instead of current address
  names_indic = c("vb.tsmart_zip", "vb.tsmart_full_address", "vb.vf_party", "vb.voterbase_gender", "vb.voterbase_age", "vb.tsmart_last_name", 
                  "vb.voterbase_race", "vb.tsmart_state", "vb.clarity_party_2013_v3", "vb.vf_pp2012_party", "vb.vf_pp2012", "vb.tsmart_census_id", "vb.education", 
                  "vb.vf_reg_zip", "vb.voterbase_dob", "vb.tsmart_first_name", "vb.tsmart_middle_name", "voterbase_id")
  indic <- NA
  for (p in 1:length(names_indic)) {
    indic[p] <- which(names(tab5rows) == names_indic[p])
  }
  
  classes[indic] <- c(rep("character", 2), rep(NA, 3), "character", rep(NA, 5), "character", rep(NA, 3), rep("character", 3))
  data <- read.csv(i, sep = "\t", colClasses = classes, na.strings = c("NA", "NaN", " ", ""))
  data = data.table(data)
  
  data
}
numCores <- 20
cl <- makeCluster(numCores, file = "test")
registerDoParallel(cl)



couples <- foreach::foreach(i = i, .packages = c("data.table"), .verbose = T) %dopar% aggregation()
stopCluster(cl)

data = do.call("rbind", couples)


# recode education: high school or less, some college, college or more
data[, `:=`(education, recode(vb.education, "0=NA;1=1;2=2;3=3;4=3;5=2"))]

states = read.csv("~/states.csv")
# party registration
party_registration_states <- c("AK", "AZ", "CA", "CO", "CT", "DC", "DE", "FL", "IA", "ID", "KS", "KY", "LA", "MA", "MD", "ME", "NC", "NE", "NH", 
                               "NJ", "NM", "NV", "NY", "OK", "OR", "PA", "RI", "SD", "UT", "WV", "WY")
party_registration_states_no_dc <- c("AK", "AZ", "CA", "CO", "CT", "DE", "FL", "IA", "ID", "KS", "KY", "LA", "MA", "MD", "ME", "NC", "NE", "NH", 
                                     "NJ", "NM", "NV", "NY", "OK", "OR", "PA", "RI", "SD", "UT", "WV", "WY")

# see difference between partisann registration states and non-partisan registration states
state_vote <- read.csv("~/State_Vote.csv")
state_vote <- data.table(state_vote)
t.test(state_vote[State %in% party_registration_states_no_dc]$National_dem, state_vote[!State %in% party_registration_states_no_dc]$National_dem)
# closed for both
closed_primary = c("DE", "FL", "KS", "KY", "ME", "NV", "NJ", "NM", "NY", "OR", "PA", "WY")

# semi-closed for both
semi_closed_primary <- c("MA", "RI", "NC", "WV")

# open for both

# adjust depending on whether we want all states
data = data[vb.tsmart_state %in% states$Abbreviation]


# construct census_id for block group
setnames(data, "vb.tsmart_census_id", "census_id")
# data[,census_id:=substr(ifelse(nchar(vb.tsmart_census_id)==14, paste0('0', vb.tsmart_census_id),vb.tsmart_census_id),1,12)]

# dim 252,292,062 save(data,file='~/Vfile/Marriage.RData') load('~/Vfile/Marriage.RData') complete data in full address, zip-code, age, gender,
# party
couples = data[complete.cases(data[, .(vb.tsmart_full_address, vb.tsmart_zip, vb.voterbase_age, vb.voterbase_gender, vb.vf_party)])]

# indicator for definition of spouses
couples[, `:=`(COUNT = .N), by = c("vb.tsmart_zip", "vb.tsmart_full_address")]


### Party: Unaffiliated: registered in non-partisan registration state, No Party as Independents impute party: could change to 79, 20
couples[, `:=`(Party, NA)]
couples$Party[couples$vb.vf_party != "Democrat" & couples$vb.vf_party != "Republican" & couples$vb.clarity_party_2013_v3 > 69] <- "Democrat"
couples$Party[couples$vb.vf_party != "Democrat" & couples$vb.vf_party != "Republican" & couples$vb.clarity_party_2013_v3 < 30] <- "Republican"
# adjust depending on whether we want all states
couples$Party[couples$vb.vf_party == "No Party" & couples$vb.clarity_party_2013_v3 > 29 & couples$vb.clarity_party_2013_v3 < 70] <- "No Party"
# couples$Party[(couples$vb.vf_party=='No Party'|couples$vb.vf_party=='Unaffiliated')& couples$vb.clarity_party_2013_v3>29 &
# couples$vb.clarity_party_2013_v3<70]<-'No Party'

couples$Party[is.na(couples$Party)] <- as.character(couples$vb.vf_party[is.na(couples$Party)])
couples[, `:=`(Party, factor(Party))]

# put out by county
couples[, `:=`(county_fips, substr(census_id, 1, 5))]
couples$Party_coded <- NA
couples$Party_coded[couples$Party == "Republican"] <- -1
couples$Party_coded[couples$Party == "Independent"] <- 0
couples$Party_coded[couples$Party == "Democrat"] <- 1

couples$Party_non_coded <- NA
couples$Party_non_coded[couples$vb.vf_party == "Republican"] <- -1
couples$Party_non_coded[couples$vb.vf_party == "No Party"] <- 0
couples$Party_non_coded[couples$vb.vf_party == "Democrat"] <- 1

corrs = couples[, list(imputed = mean(Party_coded, na.rm = T), non_imputed = mean(Party_non_coded, na.rm = T)), by = "county_fips"]
# save(corrs,file = '~/Vfile/Robustness.RData')
couples[, `:=`(Party_non_coded, NULL)]
couples[, `:=`(Party_coded, NULL)]
dem_par <- factor(as.character(couples[(vb.vf_party != "Democrat" & vb.vf_party != "Republican") & Party == "Democrat"]$vb.vf_pp2012_party))
# 

rep_par <- factor(as.character(couples[(vb.vf_party != "Democrat" & vb.vf_party != "Republican") & Party == "Republican"]$vb.vf_pp2012_party))

(254191 + 2218306)/(418886 + 2253043)  #consistency

# choose dyads with 2-4 members, at zip+full_adress
couples = couples[COUNT <= 4 & COUNT > 1]
couples[, `:=`(indicator = .I[1]), by = c("vb.tsmart_zip", "vb.tsmart_full_address")]


setkey(couples, indicator)
setorder(couples, indicator, -vb.voterbase_age)

first_out <- which(duplicated(couples, by = "indicator"))  #second oldest
family_dyad_1 <- couples[first_out]
couples_2 = couples[-first_out]
setkey(family_dyad_1, indicator)
setorder(family_dyad_1, indicator, -vb.voterbase_age)
setkey(couples_2, indicator)
setorder(couples_2, indicator, -vb.voterbase_age)

second_out <- which(duplicated(family_dyad_1, by = "indicator"))  #third oldest
family_dyad_2 <- family_dyad_1[second_out]
family_dyad_1 = family_dyad_1[-second_out]
setkey(family_dyad_2, indicator)
setorder(family_dyad_2, indicator, -vb.voterbase_age)

third_out <- which(duplicated(family_dyad_2, by = "indicator"))  #fourth_oldest
family_dyad_3 <- family_dyad_2[third_out]
family_dyad_2 = family_dyad_2[-third_out]
setkey(family_dyad_3, indicator)

length(which(duplicated(couples_2, by = "indicator")))
length(which(duplicated(family_dyad_1, by = "indicator")))
length(which(duplicated(family_dyad_2, by = "indicator")))
length(which(duplicated(family_dyad_3, by = "indicator")))

setnames(couples_2, c("vb.vf_party", "vb.voterbase_age", "vb.voterbase_gender", "Party", "vb.vf_pp2012_party"), c("Parental_Party_1", "Parental_Age_1", 
                                                                                                                  "Parental_Gender_1", "Parental_Party_imputed_1", "Parental_Primary_Vote_1"))
setnames(family_dyad_1, c("vb.vf_party", "vb.voterbase_age", "vb.voterbase_gender", "Party", "vb.vf_pp2012_party"), c("Parental_Party_2", "Parental_Age_2", 
                                                                                                                      "Parental_Gender_2", "Parental_Party_imputed_2", "Parental_Primary_Vote_2"))
setnames(family_dyad_2, c("vb.vf_party", "vb.voterbase_age", "vb.voterbase_gender", "Party", "vb.vf_pp2012_party"), c("Offspring_Party_1", "Offspring_Age_1", 
                                                                                                                      "Offspring_Gender_1", "Offspring_Party_imputed_1", "Offspring_Primary_Vote_1"))
setnames(family_dyad_3, c("vb.vf_party", "vb.voterbase_age", "vb.voterbase_gender", "Party", "vb.vf_pp2012_party"), c("Offspring_Party_2", "Offspring_Age_2", 
                                                                                                                      "Offspring_Gender_2", "Offspring_Party_imputed_2", "Offspring_Primary_Vote_2"))

setkey(couples_2, indicator)
setkey(family_dyad_1, indicator)
setkey(family_dyad_2, indicator)
setkey(family_dyad_3, indicator)

couples_2 = family_dyad_1[couples_2]
couples_2 = family_dyad_2[couples_2]
couples_2 = family_dyad_3[couples_2]

# 53,465,546

couples_2 = couples_2[Parental_Age_2 - Offspring_Age_1 > 17 | is.na(Offspring_Age_1)]
# 44,518,781 only men and wife
couples_2 = couples_2[(Parental_Gender_1 == "Male" & Parental_Gender_2 == "Female") | (Parental_Gender_2 == "Male" & Parental_Gender_1 == "Female")]
# 35,990,397

# only pid couples_2=couples_2[(Parental_Party_1=='Democrat' | Parental_Party_1=='Republican'|Parental_Party_1=='No
# Party')&(Parental_Party_2=='Democrat' | Parental_Party_2=='Republican'|Parental_Party_2=='No Party')]
couples_2 = couples_2[(Parental_Party_imputed_1 == "Democrat" | Parental_Party_imputed_1 == "Republican" | Parental_Party_imputed_1 == "No Party") & 
                        (Parental_Party_imputed_2 == "Democrat" | Parental_Party_imputed_2 == "Republican" | Parental_Party_imputed_2 == "No Party")]

# new: 29,026,930 29,026,930 mean in all states
mean(couples_2$Parental_Party_imputed_1 == couples_2$Parental_Party_imputed_2)


# save(couples_2, file='~/Vfile/triads.RData') validation check# primary vote
dem_par <- factor(c(as.character(couples_2[(Parental_Party_1 != "Democrat" & Parental_Party_1 != "Republican") & Parental_Party_imputed_1 == "Democrat"]$Parental_Primary_Vote_1), 
                    as.character(couples_2[(Parental_Party_2 != "Democrat" & Parental_Party_2 != "Republican") & Parental_Party_imputed_2 == "Democrat"]$Parental_Primary_Vote_2)))
# 

rep_par <- factor(c(as.character(couples_2[(Parental_Party_1 != "Democrat" & Parental_Party_1 != "Republican") & Parental_Party_imputed_1 == "Republican"]$Parental_Primary_Vote_1), 
                    as.character(couples_2[(Parental_Party_2 != "Democrat" & Parental_Party_2 != "Republican") & Parental_Party_imputed_2 == "Republican"]$Parental_Primary_Vote_2)))
# 
(105626 + 1258740)/(178511 + 1273498)  #consistency
dem_off <- factor(c(as.character(couples_2[(Offspring_Party_1 != "Democrat" & Offspring_Party_1 != "Republican") & Offspring_Party_imputed_1 == 
                                             "Democrat"]$Offspring_Primary_Vote_1), as.character(couples_2[(Offspring_Party_2 != "Democrat" & Offspring_Party_2 != "Republican") & Offspring_Party_imputed_2 == 
                                                                                                             "Democrat"]$Offspring_Primary_Vote_2)))
# 

rep_off <- factor(c(as.character(couples_2[(Offspring_Party_1 != "Democrat" & Offspring_Party_1 != "Republican") & Offspring_Party_imputed_1 == 
                                             "Republican"]$Offspring_Primary_Vote_1), as.character(couples_2[(Offspring_Party_2 != "Democrat" & Offspring_Party_2 != "Republican") & Offspring_Party_imputed_2 == 
                                                                                                               "Republican"]$Offspring_Primary_Vote_2)))
# 


# get women and men separately
fathers = couples_2[Parental_Gender_1 == "Male", .(i.vb.tsmart_state.1, i.COUNT.1, i.vb.tsmart_zip.1, indicator, Parental_Age_1, Parental_Party_1, 
                                                   Parental_Party_imputed_1, i.vb.education.1, i.vb.tsmart_last_name.1, i.vb.voterbase_race.1, i.census_id.1, i.vb.clarity_party_2013_v3.1, Parental_Primary_Vote_1, 
                                                   Parental_Gender_1, i.voterbase_id.1)]

setnames(fathers, c("indicator", "i.COUNT.1", "i.vb.tsmart_state.1", "i.vb.tsmart_zip.1", "i.vb.voterbase_race.1", "i.vb.tsmart_last_name.1", "i.census_id.1", 
                    "i.vb.clarity_party_2013_v3.1", "i.vb.education.1", "Parental_Age_1", "Parental_Party_1", "Parental_Party_imputed_1", "Parental_Primary_Vote_1", 
                    "Parental_Gender_1", "i.voterbase_id.1"), c("indicator_fathers", "COUNT", "state_fathers", "zip_fathers", "race_fathers", "last_name_fathers", 
                                                                "census_fathers", "clarity_fathers", "education_fathers", "age_fathers", "Parental_Party_fathers", "Parental_Party_imputed_fathers", "Parental_Primary_Vote_fathers", 
                                                                "Parental_Gender_fathers", "voterbase_indicator_fathers"))

fathers_2 = couples_2[Parental_Gender_2 == "Male", .(i.vb.tsmart_state.2, i.COUNT.2, i.vb.tsmart_zip.2, indicator, Parental_Age_2, Parental_Party_2, 
                                                     Parental_Party_imputed_2, i.vb.education.2, i.vb.tsmart_last_name.2, i.vb.voterbase_race.2, i.census_id.2, i.vb.clarity_party_2013_v3.2, Parental_Primary_Vote_2, 
                                                     Parental_Gender_2, i.voterbase_id.2)]
setnames(fathers_2, c("indicator", "i.COUNT.2", "i.vb.tsmart_state.2", "i.vb.tsmart_zip.2", "i.vb.voterbase_race.2", "i.vb.tsmart_last_name.2", 
                      "i.census_id.2", "i.vb.clarity_party_2013_v3.2", "i.vb.education.2", "Parental_Age_2", "Parental_Party_2", "Parental_Party_imputed_2", "Parental_Primary_Vote_2", 
                      "Parental_Gender_2", "i.voterbase_id.2"), c("indicator_fathers", "COUNT", "state_fathers", "zip_fathers", "race_fathers", "last_name_fathers", 
                                                                  "census_fathers", "clarity_fathers", "education_fathers", "age_fathers", "Parental_Party_fathers", "Parental_Party_imputed_fathers", "Parental_Primary_Vote_fathers", 
                                                                  "Parental_Gender_fathers", "voterbase_indicator_fathers"))

fathers = rbind(fathers, fathers_2)

# mothers
mothers = couples_2[Parental_Gender_1 == "Female", .(i.vb.tsmart_state.1, i.vb.tsmart_zip.1, indicator, Parental_Age_1, Parental_Party_1, Parental_Party_imputed_1, 
                                                     i.vb.education.1, i.vb.tsmart_last_name.1, i.vb.voterbase_race.1, i.census_id.1, i.vb.clarity_party_2013_v3.1, Parental_Primary_Vote_1, Parental_Gender_1, 
                                                     i.voterbase_id.1)]
setnames(mothers, c("indicator", "i.vb.tsmart_state.1", "i.vb.tsmart_zip.1", "i.vb.voterbase_race.1", "i.vb.tsmart_last_name.1", "i.census_id.1", 
                    "i.vb.clarity_party_2013_v3.1", "i.vb.education.1", "Parental_Age_1", "Parental_Party_1", "Parental_Party_imputed_1", "Parental_Primary_Vote_1", 
                    "Parental_Gender_1", "i.voterbase_id.1"), c("indicator_mothers", "state_mothers", "zip_mothers", "race_mothers", "last_name_mothers", "census_mothers", 
                                                                "clarity_mothers", "education_mothers", "age_mothers", "Parental_Party_mothers", "Parental_Party_imputed_mothers", "Parental_Primary_Vote_mothers", 
                                                                "Parental_Gender_mothers", "voterbase_indicator_mothers"))

mothers_2 = couples_2[Parental_Gender_2 == "Female", .(i.vb.tsmart_state.2, i.vb.tsmart_zip.2, indicator, Parental_Age_2, Parental_Party_2, Parental_Party_imputed_2, 
                                                       i.vb.education.2, i.vb.tsmart_last_name.2, i.vb.voterbase_race.2, i.census_id.2, i.vb.clarity_party_2013_v3.2, Parental_Primary_Vote_2, Parental_Gender_2, 
                                                       i.voterbase_id.2)]
setnames(mothers_2, c("indicator", "i.vb.tsmart_state.2", "i.vb.tsmart_zip.2", "i.vb.voterbase_race.2", "i.vb.tsmart_last_name.2", "i.census_id.2", 
                      "i.vb.clarity_party_2013_v3.2", "i.vb.education.2", "Parental_Age_2", "Parental_Party_2", "Parental_Party_imputed_2", "Parental_Primary_Vote_2", 
                      "Parental_Gender_2", "i.voterbase_id.2"), c("indicator_mothers", "state_mothers", "zip_mothers", "race_mothers", "last_name_mothers", "census_mothers", 
                                                                  "clarity_mothers", "education_mothers", "age_mothers", "Parental_Party_mothers", "Parental_Party_imputed_mothers", "Parental_Primary_Vote_mothers", 
                                                                  "Parental_Gender_mothers", "voterbase_indicator_mothers"))

mothers = rbind(mothers, mothers_2)
setkey(mothers, "indicator_mothers")
setkey(fathers, "indicator_fathers")
couples_2_married = fathers[mothers]
# 29,026,930 drop state inaccuracies
couples_2_married = couples_2_married[state_mothers == state_fathers]
## 29,026,891 depending on whether we want all spouses, or just spouses in partisan registration states
couples_2_married = couples_2_married[state_mothers %in% party_registration_states]
# 18,628,609 final pairs

# three-ways
xtable(CrossTable(couples_2_married[(Parental_Party_fathers == "Republican" | Parental_Party_fathers == "Democrat" | Parental_Party_fathers == "No Party") & 
                                      (Parental_Party_mothers == "Republican" | Parental_Party_mothers == "Democrat" | Parental_Party_mothers == "No Party")]$Parental_Party_fathers, 
                  couples_2_married[(Parental_Party_fathers == "Republican" | Parental_Party_fathers == "Democrat" | Parental_Party_fathers == "No Party") & (Parental_Party_mothers == 
                                                                                                                                                                "Republican" | Parental_Party_mothers == "Democrat" | Parental_Party_mothers == "No Party")]$Parental_Party_mothers, prop.r = F, prop.c = F, 
                  prop.chisq = F, percent = T, dnn = c("Democrat", "Independent")))
summary(factor(c(as.character(couples_2_married[(Parental_Party_fathers == "Republican" | Parental_Party_fathers == "Democrat" | Parental_Party_fathers == 
                                                   "No Party") & (Parental_Party_mothers == "Republican" | Parental_Party_mothers == "Democrat" | Parental_Party_mothers == "No Party")]$Parental_Party_fathers), 
                 as.character(couples_2_married[(Parental_Party_fathers == "Republican" | Parental_Party_fathers == "Democrat" | Parental_Party_fathers == "No Party") & 
                                                  (Parental_Party_mothers == "Republican" | Parental_Party_mothers == "Democrat" | Parental_Party_mothers == "No Party")]$Parental_Party_mothers))))

6352658/(14153186 + 6352658 + 13316088)
# 18.78% independents three-ways anything but closed
xtable(CrossTable(couples_2_married[(Parental_Party_fathers == "Republican" | Parental_Party_fathers == "Democrat" | Parental_Party_fathers == "No Party") & 
                                      (Parental_Party_mothers == "Republican" | Parental_Party_mothers == "Democrat" | Parental_Party_mothers == "No Party") & !state_fathers %in% 
                                      closed_primary]$Parental_Party_fathers, couples_2_married[(Parental_Party_fathers == "Republican" | Parental_Party_fathers == "Democrat" | Parental_Party_fathers == 
                                                                                                   "No Party") & (Parental_Party_mothers == "Republican" | Parental_Party_mothers == "Democrat" | Parental_Party_mothers == "No Party") & !state_mothers %in% 
                                                                                                  closed_primary]$Parental_Party_mothers, prop.r = F, prop.c = F, prop.chisq = F, dnn = c("Democrat", "Independent")))
# three-ways closed primary
xtable(CrossTable(couples_2_married[(Parental_Party_fathers == "Republican" | Parental_Party_fathers == "Democrat" | Parental_Party_fathers == "No Party") & 
                                      (Parental_Party_mothers == "Republican" | Parental_Party_mothers == "Democrat" | Parental_Party_mothers == "No Party") & state_fathers %in% 
                                      closed_primary]$Parental_Party_fathers, couples_2_married[(Parental_Party_fathers == "Republican" | Parental_Party_fathers == "Democrat" | Parental_Party_fathers == 
                                                                                                   "No Party") & (Parental_Party_mothers == "Republican" | Parental_Party_mothers == "Democrat" | Parental_Party_mothers == "No Party") & state_mothers %in% 
                                                                                                  closed_primary]$Parental_Party_mothers, prop.r = F, prop.c = F, prop.chisq = F, dnn = c("Democrat", "Independent")))

# two-ways
xtable(CrossTable(couples_2_married[(Parental_Party_fathers == "Republican" | Parental_Party_fathers == "Democrat") & (Parental_Party_mothers == 
                                                                                                                         "Republican" | Parental_Party_mothers == "Democrat")]$Parental_Party_fathers, couples_2_married[(Parental_Party_fathers == "Republican" | Parental_Party_fathers == 
                                                                                                                                                                                                                            "Democrat") & (Parental_Party_mothers == "Republican" | Parental_Party_mothers == "Democrat")]$Parental_Party_mothers, prop.r = F, prop.c = F, 
                  prop.chisq = F, dnn = c("Democrat", "Independent")))

# three ways with imputed partisanship
xtable(CrossTable(couples_2_married$Parental_Party_imputed_fathers, couples_2_married$Parental_Party_imputed_mothers, prop.r = F, prop.c = F, prop.chisq = F))

# two ways with imputed partisanship
xtable(CrossTable(couples_2_married[(Parental_Party_imputed_fathers == "Republican" | Parental_Party_imputed_fathers == "Democrat") & (Parental_Party_imputed_mothers == 
                                                                                                                                         "Republican" | Parental_Party_imputed_mothers == "Democrat")]$Parental_Party_imputed_fathers, couples_2_married[(Parental_Party_imputed_fathers == 
                                                                                                                                                                                                                                                            "Republican" | Parental_Party_imputed_fathers == "Democrat") & (Parental_Party_imputed_mothers == "Republican" | Parental_Party_imputed_mothers == 
                                                                                                                                                                                                                                                                                                                              "Democrat")]$Parental_Party_imputed_mothers, prop.r = F, prop.c = F, prop.chisq = F))

mean(couples_2_married[(Parental_Party_imputed_fathers == "Republican" | Parental_Party_imputed_fathers == "Democrat") & (Parental_Party_imputed_mothers == 
                                                                                                                            "Republican" | Parental_Party_imputed_mothers == "Democrat")]$Parental_Party_imputed_fathers == couples_2_married[(Parental_Party_imputed_fathers == 
                                                                                                                                                                                                                                                 "Republican" | Parental_Party_imputed_fathers == "Democrat") & (Parental_Party_imputed_mothers == "Republican" | Parental_Party_imputed_mothers == 
                                                                                                                                                                                                                                                                                                                   "Democrat")]$Parental_Party_imputed_mothers)

# different decision rules

# three ways with imputed partisanship for those married 5 years or less
crosstab(couples_2_married[ifelse(Parental_Age_fathers < Parental_Age_mothers, Parental_Age_fathers == 25 | Parental_Age_fathers == 26, Parental_Age_mothers == 
                                    25 | Parental_Age_mothers == 26)]$Parental_Party_imputed_fathers, couples_2_married[ifelse(Parental_Age_fathers < Parental_Age_mothers, Parental_Age_fathers == 
                                                                                                                                 25 | Parental_Age_fathers == 26, Parental_Age_mothers == 25 | Parental_Age_mothers == 26)]$Parental_Party_imputed_mothers, dnn = c("Fathers", 
                                                                                                                                                                                                                                                                    "Mothers"))


# homogeneity
couples_2_married$homogeneity <- NA
couples_2_married$homogeneity[couples_2_married$Parental_Party_imputed_fathers == "Democrat" & couples_2_married$Parental_Party_imputed_mothers == 
                                "Democrat"] <- 1
couples_2_married$homogeneity[couples_2_married$Parental_Party_imputed_fathers == "Democrat" & couples_2_married$Parental_Party_imputed_mothers == 
                                "Republican"] <- 0
couples_2_married$homogeneity[couples_2_married$Parental_Party_imputed_fathers == "Republican" & couples_2_married$Parental_Party_imputed_mothers == 
                                "Democrat"] <- 0
couples_2_married$homogeneity[couples_2_married$Parental_Party_imputed_fathers == "Republican" & couples_2_married$Parental_Party_imputed_mothers == 
                                "Republican"] <- 1
couples_2_married$homogeneity[couples_2_married$Parental_Party_imputed_fathers == "No Party" & couples_2_married$Parental_Party_imputed_mothers == 
                                "No Party"] <- 1
couples_2_married$homogeneity[couples_2_married$Parental_Party_imputed_fathers == "Republican" & couples_2_married$Parental_Party_imputed_mothers == 
                                "No Party"] <- 0
couples_2_married$homogeneity[couples_2_married$Parental_Party_imputed_fathers == "No Party" & couples_2_married$Parental_Party_imputed_mothers == 
                                "Republican"] <- 0
couples_2_married$homogeneity[couples_2_married$Parental_Party_imputed_fathers == "Democrat" & couples_2_married$Parental_Party_imputed_mothers == 
                                "No Party"] <- 0
couples_2_married$homogeneity[couples_2_married$Parental_Party_imputed_fathers == "No Party" & couples_2_married$Parental_Party_imputed_mothers == 
                                "Democrat"] <- 0
mean(couples_2_married$homogeneity, na.rm = T)
mean(couples_2_married$homogeneity, na.rm = T)
mean(couples_2_married[COUNT == 2]$homogeneity, na.rm = T)

# for 2-persons-dyads

couples_2_married[, `:=`(education_fathers, recode(education_fathers, "0=NA;1=1;2=2;3=3;4=3;5=2"))]
couples_2_married[, `:=`(education_mothers, recode(education_mothers, "0=NA;1=1;2=2;3=3;4=3;5=2"))]

with(couples_2_married, crosstab(homogeneity, round((education_fathers + education_mothers)/2), prop.c = T))

############## I am here!

# age for age plot
couples_2_married[, `:=`(mean_age, ifelse(age_fathers < age_mothers, age_fathers, age_mothers))]
couples_2_married$mean_age <- cut(couples_2_married$mean_age, breaks = c(-Inf, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, Inf), labels = c("<26", 
                                                                                                                                                "30", "35", "40", "45", "50", "55", "60", "65", "70", "75", "80", ">80"))


# different decision rules and ages
mean(couples_2_married[ifelse(age_fathers < age_mothers, age_fathers == 25 | age_fathers == 26, age_mothers == 25 | age_mothers == 26)]$homogeneity, 
     na.rm = T)
mean(couples_2_married[ifelse(age_fathers < age_mothers, age_fathers < 25, age_mothers < 25)]$homogeneity, na.rm = T)
mean(couples_2_married[ifelse(age_fathers < age_mothers, age_fathers > 45, age_mothers > 45)]$homogeneity, na.rm = T)
mean(couples_2_married[abs(age_fathers - age_mothers) <= 17]$homogeneity, na.rm = T)
mean(couples_2_married[abs(age_fathers - age_mothers) <= 5]$homogeneity)
mean(couples_2_married[last_name_mothers == last_name_fathers]$homogeneity)


# overall mean
mean(couples_2_married$homogeneity)  #80.54462
length(which(!is.na(couples_2_married$homogeneity)))
# se
sqrt(80.54462 * (100 - 80.54462)/18628609)
# 0.009171659



# 0.009106761 homogeneity by age######


Homogeneity = tapply(couples_2_married$homogeneity, couples_2_married$mean_age, function(x) {
  prop <- prop.table(table(x))
  SE <- sqrt(prop * (1 - prop)/length(x))
  list(prop = prop, SE = SE)
  
})
means <- NULL
ses <- NULL
for (i in 1:13) {
  means[i] <- Homogeneity[[i]][[1]][2]
  ses[i] <- Homogeneity[[i]][[2]][2]
  
}
Hom = data.frame(Age = names(Homogeneity)[1:13], Mean = means, SE = ses)
mean(couples_2_married[age_mothers < 25 & age_fathers < 25]$homogeneity)
mean(couples_2_married[age_mothers > 60 & age_fathers > 60]$homogeneity)

# by_age
#save(Hom, file = "~/Vfile/Homogeneity_by_Age_including_independents.RData")


############################# Zip Code Level Analyses####

#### zip-code-level folded spousal homogeneity


zipcode_couples_ind = couples_2_married[, list(homogeneity = mean(abs(homogeneity), na.rm = T), N = length(homogeneity)), by = zip_fathers]
#save(zipcode_couples_ind, file = "~/Vfile/zip-code-level_spousal_homogeneity_folded+indep.RData")



####### zip code level overall homogeneity just registered partisans

zip_code_just_partisan = data[, list(Homogeneity = (length(which(vb.vf_party == "Republican")) - length(which(vb.vf_party == "Democrat")))/(length(which(vb.vf_party == 
                                                                                                                                                           "Republican")) + length(which(vb.vf_party == "Democrat")))), by = "vb.tsmart_zip"]

setnames(zip_code_just_partisan, "vb.tsmart_zip", "zip")
#save(zip_code_just_partisan, file = "~/Vfile/homogeneity_by_zip_registration_partisan_denominator.RData")

############## now at blockgroup
couples_2_married[, `:=`(blgrp_mothers, substr(ifelse(nchar(as.character(census_mothers)) == 14, paste0("0", census_mothers), census_mothers), 1, 
                                               12))]
couples_2_married[, `:=`(blgrp_fathers, substr(ifelse(nchar(as.character(census_fathers)) == 14, paste0("0", census_fathers), census_fathers), 1, 
                                               12))]

blockgroup_couples_ind = couples_2_married[blgrp_mothers == blgrp_fathers, list(homogeneity = mean(abs(homogeneity), na.rm = T), N = length(homogeneity)), 
                                           by = "blgrp_mothers"]
setnames(blockgroup_couples_ind, "blgrp_mothers", "census")

#save(blockgroup_couples_ind, file = "~/Vfile/blockgroup-level_spousal_homogeneity_folded+indep.RData")


blockgroup_just_partisan = data[, list(Homogeneity = (length(which(vb.vf_party == "Republican")) - length(which(vb.vf_party == "Democrat")))/(length(which(vb.vf_party == 
                                                                                                                                                             "Republican")) + length(which(vb.vf_party == "Democrat")))), by = "census_id"]

setnames(blockgroup_just_partisan, "census_id", "census")

#save(blockgroup_just_partisan, file = "~/Vfile/homogeneity_by_blockgroup_registration_partisan_denominator.RData")



######## intergenerational, same last name

# also only party registrants minors
couples_2 = couples_2[(Offspring_Party_imputed_1 == "Democrat" | Offspring_Party_imputed_1 == "Republican" | Offspring_Party_imputed_1 == "No Party") & 
                        (Offspring_Party_imputed_2 == "Democrat" | Offspring_Party_imputed_2 == "Republican" | Offspring_Party_imputed_2 == "No Party")]


# overall
with(couples_2[i.vb.tsmart_last_name.1 == i.vb.tsmart_last_name.2 & vb.tsmart_state %in% party_registration_states], mean(rep(c(Parental_Party_imputed_1, 
                                                                                                                                Parental_Party_imputed_2), 2) == c(rep(Offspring_Party_imputed_1, 2), rep(Offspring_Party_imputed_2, 2)), na.rm = T))

with(couples_2[i.vb.tsmart_last_name.1 == i.vb.tsmart_last_name.2 & vb.tsmart_state %in% party_registration_states], length(na.omit(rep(c(Parental_Party_imputed_1, 
                                                                                                                                          Parental_Party_imputed_2), 2) == c(rep(Offspring_Party_imputed_1, 2), rep(Offspring_Party_imputed_2, 2)))))
# 78.60041

# error
sqrt(78.60041 * (100 - 78.60041)/4111860)
# 0.02022535

# 79.06719% for female parent offspring

with(couples_2[i.vb.tsmart_last_name.1 == i.vb.tsmart_last_name.2 & vb.tsmart_state %in% party_registration_states], mean(rep(c(Parental_Party_imputed_1[Parental_Gender_1 == 
                                                                                                                                                           "Female"], Parental_Party_imputed_2[Parental_Gender_2 == "Female"]), 2) == c(Offspring_Party_imputed_1[Parental_Gender_1 == "Female"], Offspring_Party_imputed_1[Parental_Gender_2 == 
                                                                                                                                                                                                                                                                                                                              "Female"], Offspring_Party_imputed_2[Parental_Gender_1 == "Female"], Offspring_Party_imputed_2[Parental_Gender_2 == "Female"]), na.rm = T))


with(couples_2[i.vb.tsmart_last_name.1 == i.vb.tsmart_last_name.2 & vb.tsmart_state %in% party_registration_states], mean(rep(c(Parental_Party_imputed_1[Parental_Gender_1 == 
                                                                                                                                                           "Male"], Parental_Party_imputed_2[Parental_Gender_2 == "Male"]), 2) == c(Offspring_Party_imputed_1[Parental_Gender_1 == "Male"], Offspring_Party_imputed_1[Parental_Gender_2 == 
                                                                                                                                                                                                                                                                                                                        "Male"], Offspring_Party_imputed_2[Parental_Gender_1 == "Male"], Offspring_Party_imputed_2[Parental_Gender_2 == "Male"]), na.rm = T))



# by homogeneity
couples_2$homogeneity <- NA
couples_2$homogeneity[couples_2$Parental_Party_imputed_1 == "Democrat" & couples_2$Parental_Party_imputed_2 == "Democrat"] <- 1
couples_2$homogeneity[couples_2$Parental_Party_imputed_1 == "Democrat" & couples_2$Parental_Party_imputed_2 == "Republican"] <- 0
couples_2$homogeneity[couples_2$Parental_Party_imputed_1 == "Republican" & couples_2$Parental_Party_imputed_2 == "Democrat"] <- 0
couples_2$homogeneity[couples_2$Parental_Party_imputed_1 == "Republican" & couples_2$Parental_Party_imputed_2 == "Republican"] <- 1
couples_2$homogeneity[couples_2$Parental_Party_imputed_1 == "No Party" & couples_2$Parental_Party_imputed_2 == "No Party"] <- 1
couples_2$homogeneity[couples_2$Parental_Party_imputed_1 == "Republican" & couples_2$Parental_Party_imputed_2 == "No Party"] <- 0
couples_2$homogeneity[couples_2$Parental_Party_imputed_1 == "No Party" & couples_2$Parental_Party_imputed_2 == "Republican"] <- 0
couples_2$homogeneity[couples_2$Parental_Party_imputed_1 == "Democrat" & couples_2$Parental_Party_imputed_2 == "No Party"] <- 0
couples_2$homogeneity[couples_2$Parental_Party_imputed_1 == "No Party" & couples_2$Parental_Party_imputed_2 == "Democrat"] <- 0
mean(couples_2$homogeneity, na.rm = T)



with(couples_2[i.vb.tsmart_last_name.1 == i.vb.tsmart_last_name.2 & vb.tsmart_state %in% party_registration_states & homogeneity == 1], mean(c(Offspring_Party_imputed_1 != 
                                                                                                                                                 Parental_Party_imputed_1 & Offspring_Party_imputed_1 != Parental_Party_imputed_2, Offspring_Party_imputed_2 != Parental_Party_imputed_1 & Offspring_Party_imputed_1 != 
                                                                                                                                                 Parental_Party_imputed_2), na.rm = T))
# 10.49266 % don't agree with either parent in homogeneous pairs
with(couples_2[i.vb.tsmart_last_name.1 == i.vb.tsmart_last_name.2 & vb.tsmart_state %in% party_registration_states & homogeneity == 0], mean(c(Offspring_Party_imputed_1 != 
                                                                                                                                                 Parental_Party_imputed_1 & Offspring_Party_imputed_1 != Parental_Party_imputed_2, Offspring_Party_imputed_2 != Parental_Party_imputed_1 & Offspring_Party_imputed_1 != 
                                                                                                                                                 Parental_Party_imputed_2), na.rm = T))
# 25.40446 % don't agree with either parent in heterogeneous pairs

# higher by education

with(couples_2[i.vb.tsmart_last_name.1 == i.vb.tsmart_last_name.2 & vb.tsmart_state %in% party_registration_states & round((i.education.1 + i.education.2)/2) == 
                 3], mean(rep(c(Parental_Party_imputed_1, Parental_Party_imputed_2), 2) == c(rep(Offspring_Party_imputed_1, 2), rep(Offspring_Party_imputed_2, 
                                                                                                                                    2)), na.rm = T))

with(couples_2[i.vb.tsmart_last_name.1 == i.vb.tsmart_last_name.2 & vb.tsmart_state %in% party_registration_states & round((i.education.1 + i.education.2)/2) == 
                 2], mean(rep(c(Parental_Party_imputed_1, Parental_Party_imputed_2), 2) == c(rep(Offspring_Party_imputed_1, 2), rep(Offspring_Party_imputed_2, 
                                                                                                                                    2)), na.rm = T))

with(couples_2[i.vb.tsmart_last_name.1 == i.vb.tsmart_last_name.2 & vb.tsmart_state %in% party_registration_states & round((i.education.1 + i.education.2)/2) == 
                 1], mean(rep(c(Parental_Party_imputed_1, Parental_Party_imputed_2), 2) == c(rep(Offspring_Party_imputed_1, 2), rep(Offspring_Party_imputed_2, 
                                                                                                                                    2)), na.rm = T))
# now by homogeneity
with(couples_2[i.vb.tsmart_last_name.1 == i.vb.tsmart_last_name.2 & vb.tsmart_state %in% party_registration_states & homogeneity == 1], mean(rep(c(Parental_Party_imputed_1, 
                                                                                                                                                   Parental_Party_imputed_2), 2) == c(rep(Offspring_Party_imputed_1, 2), rep(Offspring_Party_imputed_2, 2)), na.rm = T))
with(couples_2[i.vb.tsmart_last_name.1 == i.vb.tsmart_last_name.2 & vb.tsmart_state %in% party_registration_states & homogeneity == 0], mean(rep(c(Parental_Party_imputed_1, 
                                                                                                                                                   Parental_Party_imputed_2), 2) == c(rep(Offspring_Party_imputed_1, 2), rep(Offspring_Party_imputed_2, 2)), na.rm = T))


############ Different Decision Rules######



# not same last name
with(couples_2[vb.tsmart_state %in% party_registration_states], mean(rep(c(Parental_Party_imputed_1[Parental_Gender_1 == "Female"], Parental_Party_imputed_2[Parental_Gender_2 == 
                                                                                                                                                               "Female"]), 2) == c(Offspring_Party_imputed_1[Parental_Gender_1 == "Female"], Offspring_Party_imputed_1[Parental_Gender_2 == "Female"], Offspring_Party_imputed_2[Parental_Gender_1 == 
                                                                                                                                                                                                                                                                                                                                   "Female"], Offspring_Party_imputed_2[Parental_Gender_2 == "Female"]), na.rm = T))


# without the same last name 80.43



# all respondents from all states
with(couples_2[i.vb.tsmart_last_name.1 == i.vb.tsmart_last_name.2], mean(rep(c(Parental_Party_imputed_1[Parental_Gender_1 == "Female"], Parental_Party_imputed_2[Parental_Gender_2 == 
                                                                                                                                                                   "Female"]), 2) == c(Offspring_Party_imputed_1[Parental_Gender_1 == "Female"], Offspring_Party_imputed_1[Parental_Gender_2 == "Female"], Offspring_Party_imputed_2[Parental_Gender_1 == 
                                                                                                                                                                                                                                                                                                                                       "Female"], Offspring_Party_imputed_2[Parental_Gender_2 == "Female"]), na.rm = T))





# For our preferred metric, mother-daughter and father-son agreement
factor_mothers = factor(c(as.character(couples_2[i.vb.tsmart_last_name.1 == i.vb.tsmart_last_name.2 & vb.tsmart_state %in% party_registration_states & 
                                                   Parental_Gender_1 == "Female" & (Offspring_Gender_1 == "Female" | Offspring_Gender_2 == "Female") & Offspring_Gender_1 != Offspring_Gender_2]$Parental_Party_imputed_1), 
                          as.character(couples_2[i.vb.tsmart_last_name.1 == i.vb.tsmart_last_name.2 & vb.tsmart_state %in% party_registration_states & Parental_Gender_2 == 
                                                   "Female" & (Offspring_Gender_1 == "Female" | Offspring_Gender_2 == "Female") & Offspring_Gender_1 != Offspring_Gender_2]$Parental_Party_imputed_2)))

factor_daughters = factor(c(ifelse(couples_2[i.vb.tsmart_last_name.1 == i.vb.tsmart_last_name.2 & vb.tsmart_state %in% party_registration_states & 
                                               Parental_Gender_1 == "Female" & (Offspring_Gender_1 == "Female" | Offspring_Gender_2 == "Female") & Offspring_Gender_1 != Offspring_Gender_2]$Offspring_Gender_1 == 
                                     "Female", as.character(couples_2[i.vb.tsmart_last_name.1 == i.vb.tsmart_last_name.2 & vb.tsmart_state %in% party_registration_states & Parental_Gender_1 == 
                                                                        "Female" & (Offspring_Gender_1 == "Female" | Offspring_Gender_2 == "Female") & Offspring_Gender_1 != Offspring_Gender_2]$Offspring_Party_imputed_1), 
                                   as.character(couples_2[i.vb.tsmart_last_name.1 == i.vb.tsmart_last_name.2 & vb.tsmart_state %in% party_registration_states & Parental_Gender_1 == 
                                                            "Female" & (Offspring_Gender_1 == "Female" | Offspring_Gender_2 == "Female") & Offspring_Gender_1 != Offspring_Gender_2]$Offspring_Party_imputed_2)), 
                            ifelse(couples_2[i.vb.tsmart_last_name.1 == i.vb.tsmart_last_name.2 & vb.tsmart_state %in% party_registration_states & Parental_Gender_2 == 
                                               "Female" & (Offspring_Gender_1 == "Female" | Offspring_Gender_2 == "Female") & Offspring_Gender_1 != Offspring_Gender_2]$Offspring_Gender_1 == 
                                     "Female", as.character(couples_2[i.vb.tsmart_last_name.1 == i.vb.tsmart_last_name.2 & vb.tsmart_state %in% party_registration_states & Parental_Gender_2 == 
                                                                        "Female" & (Offspring_Gender_1 == "Female" | Offspring_Gender_2 == "Female") & Offspring_Gender_1 != Offspring_Gender_2]$Offspring_Party_imputed_1), 
                                   as.character(couples_2[i.vb.tsmart_last_name.1 == i.vb.tsmart_last_name.2 & vb.tsmart_state %in% party_registration_states & Parental_Gender_2 == 
                                                            "Female" & (Offspring_Gender_1 == "Female" | Offspring_Gender_2 == "Female") & Offspring_Gender_1 != Offspring_Gender_2]$Offspring_Party_imputed_2))))




factor_fathers = factor(c(as.character(couples_2[i.vb.tsmart_last_name.1 == i.vb.tsmart_last_name.2 & vb.tsmart_state %in% party_registration_states & 
                                                   Parental_Gender_1 == "Male" & (Offspring_Gender_1 == "Male" | Offspring_Gender_2 == "Male") & Offspring_Gender_1 != Offspring_Gender_2]$Parental_Party_imputed_1), 
                          as.character(couples_2[i.vb.tsmart_last_name.1 == i.vb.tsmart_last_name.2 & vb.tsmart_state %in% party_registration_states & Parental_Gender_2 == 
                                                   "Male" & (Offspring_Gender_1 == "Male" | Offspring_Gender_2 == "Male") & Offspring_Gender_1 != Offspring_Gender_2]$Parental_Party_imputed_2)))

factor_sons = factor(c(ifelse(couples_2[i.vb.tsmart_last_name.1 == i.vb.tsmart_last_name.2 & vb.tsmart_state %in% party_registration_states & Parental_Gender_1 == 
                                          "Male" & (Offspring_Gender_1 == "Male" | Offspring_Gender_2 == "Male") & Offspring_Gender_1 != Offspring_Gender_2]$Offspring_Gender_1 == "Male", 
                              as.character(couples_2[i.vb.tsmart_last_name.1 == i.vb.tsmart_last_name.2 & vb.tsmart_state %in% party_registration_states & Parental_Gender_1 == 
                                                       "Male" & (Offspring_Gender_1 == "Male" | Offspring_Gender_2 == "Male") & Offspring_Gender_1 != Offspring_Gender_2]$Offspring_Party_imputed_1), 
                              as.character(couples_2[i.vb.tsmart_last_name.1 == i.vb.tsmart_last_name.2 & vb.tsmart_state %in% party_registration_states & Parental_Gender_1 == 
                                                       "Male" & (Offspring_Gender_1 == "Male" | Offspring_Gender_2 == "Male") & Offspring_Gender_1 != Offspring_Gender_2]$Offspring_Party_imputed_2)), 
                       ifelse(couples_2[i.vb.tsmart_last_name.1 == i.vb.tsmart_last_name.2 & vb.tsmart_state %in% party_registration_states & Parental_Gender_2 == 
                                          "Male" & (Offspring_Gender_1 == "Male" | Offspring_Gender_2 == "Male") & Offspring_Gender_1 != Offspring_Gender_2]$Offspring_Gender_1 == 
                                "Male", as.character(couples_2[i.vb.tsmart_last_name.1 == i.vb.tsmart_last_name.2 & vb.tsmart_state %in% party_registration_states & Parental_Gender_2 == 
                                                                 "Male" & (Offspring_Gender_1 == "Male" | Offspring_Gender_2 == "Male") & Offspring_Gender_1 != Offspring_Gender_2]$Offspring_Party_imputed_1), 
                              as.character(couples_2[i.vb.tsmart_last_name.1 == i.vb.tsmart_last_name.2 & vb.tsmart_state %in% party_registration_states & Parental_Gender_2 == 
                                                       "Male" & (Offspring_Gender_1 == "Male" | Offspring_Gender_2 == "Male") & Offspring_Gender_1 != Offspring_Gender_2]$Offspring_Party_imputed_2))))



factor_mothers_sons_mothers = factor(c(as.character(couples_2[i.vb.tsmart_last_name.1 == i.vb.tsmart_last_name.2 & vb.tsmart_state %in% party_registration_states & 
                                                                Parental_Gender_1 == "Female" & (Offspring_Gender_1 == "Male" | Offspring_Gender_2 == "Male") & Offspring_Gender_1 != Offspring_Gender_2]$Parental_Party_imputed_1), 
                                       as.character(couples_2[i.vb.tsmart_last_name.1 == i.vb.tsmart_last_name.2 & vb.tsmart_state %in% party_registration_states & Parental_Gender_2 == 
                                                                "Female" & (Offspring_Gender_1 == "Male" | Offspring_Gender_2 == "Male") & Offspring_Gender_1 != Offspring_Gender_2]$Parental_Party_imputed_2)))


factor_mothers_sons_sons = factor(c(ifelse(couples_2[i.vb.tsmart_last_name.1 == i.vb.tsmart_last_name.2 & vb.tsmart_state %in% party_registration_states & 
                                                       Parental_Gender_1 == "Female" & (Offspring_Gender_1 == "Male" | Offspring_Gender_2 == "Male") & Offspring_Gender_1 != Offspring_Gender_2]$Offspring_Gender_1 == 
                                             "Male", as.character(couples_2[i.vb.tsmart_last_name.1 == i.vb.tsmart_last_name.2 & vb.tsmart_state %in% party_registration_states & Parental_Gender_1 == 
                                                                              "Female" & (Offspring_Gender_1 == "Male" | Offspring_Gender_2 == "Male") & Offspring_Gender_1 != Offspring_Gender_2]$Offspring_Party_imputed_1), 
                                           as.character(couples_2[i.vb.tsmart_last_name.1 == i.vb.tsmart_last_name.2 & vb.tsmart_state %in% party_registration_states & Parental_Gender_1 == 
                                                                    "Female" & (Offspring_Gender_1 == "Male" | Offspring_Gender_2 == "Male") & Offspring_Gender_1 != Offspring_Gender_2]$Offspring_Party_imputed_2)), 
                                    ifelse(couples_2[i.vb.tsmart_last_name.1 == i.vb.tsmart_last_name.2 & vb.tsmart_state %in% party_registration_states & Parental_Gender_2 == 
                                                       "Female" & (Offspring_Gender_1 == "Male" | Offspring_Gender_2 == "Male") & Offspring_Gender_1 != Offspring_Gender_2]$Offspring_Gender_1 == 
                                             "Male", as.character(couples_2[i.vb.tsmart_last_name.1 == i.vb.tsmart_last_name.2 & vb.tsmart_state %in% party_registration_states & Parental_Gender_2 == 
                                                                              "Female" & (Offspring_Gender_1 == "Male" | Offspring_Gender_2 == "Male") & Offspring_Gender_1 != Offspring_Gender_2]$Offspring_Party_imputed_1), 
                                           as.character(couples_2[i.vb.tsmart_last_name.1 == i.vb.tsmart_last_name.2 & vb.tsmart_state %in% party_registration_states & Parental_Gender_2 == 
                                                                    "Female" & (Offspring_Gender_1 == "Male" | Offspring_Gender_2 == "Male") & Offspring_Gender_1 != Offspring_Gender_2]$Offspring_Party_imputed_2))))



factor_fathers_daughters_fathers = factor(c(as.character(couples_2[i.vb.tsmart_last_name.1 == i.vb.tsmart_last_name.2 & vb.tsmart_state %in% party_registration_states & 
                                                                     Parental_Gender_1 == "Male" & (Offspring_Gender_1 == "Female" | Offspring_Gender_2 == "Female") & Offspring_Gender_1 != Offspring_Gender_2]$Parental_Party_imputed_1), 
                                            as.character(couples_2[i.vb.tsmart_last_name.1 == i.vb.tsmart_last_name.2 & vb.tsmart_state %in% party_registration_states & Parental_Gender_2 == 
                                                                     "Male" & (Offspring_Gender_1 == "Female" | Offspring_Gender_2 == "Female") & Offspring_Gender_1 != Offspring_Gender_2]$Parental_Party_imputed_2)))

factor_fathers_daughters_daugthers = factor(c(ifelse(couples_2[i.vb.tsmart_last_name.1 == i.vb.tsmart_last_name.2 & vb.tsmart_state %in% party_registration_states & 
                                                                 Parental_Gender_1 == "Male" & (Offspring_Gender_1 == "Female" | Offspring_Gender_2 == "Female") & Offspring_Gender_1 != Offspring_Gender_2]$Offspring_Gender_1 == 
                                                       "Female", as.character(couples_2[i.vb.tsmart_last_name.1 == i.vb.tsmart_last_name.2 & vb.tsmart_state %in% party_registration_states & Parental_Gender_1 == 
                                                                                          "Male" & (Offspring_Gender_1 == "Female" | Offspring_Gender_2 == "Female") & Offspring_Gender_1 != Offspring_Gender_2]$Offspring_Party_imputed_1), 
                                                     as.character(couples_2[i.vb.tsmart_last_name.1 == i.vb.tsmart_last_name.2 & vb.tsmart_state %in% party_registration_states & Parental_Gender_1 == 
                                                                              "Male" & (Offspring_Gender_1 == "Female" | Offspring_Gender_2 == "Female") & Offspring_Gender_1 != Offspring_Gender_2]$Offspring_Party_imputed_2)), 
                                              ifelse(couples_2[i.vb.tsmart_last_name.1 == i.vb.tsmart_last_name.2 & vb.tsmart_state %in% party_registration_states & Parental_Gender_2 == 
                                                                 "Male" & (Offspring_Gender_1 == "Female" | Offspring_Gender_2 == "Female") & Offspring_Gender_1 != Offspring_Gender_2]$Offspring_Gender_1 == 
                                                       "Female", as.character(couples_2[i.vb.tsmart_last_name.1 == i.vb.tsmart_last_name.2 & vb.tsmart_state %in% party_registration_states & Parental_Gender_2 == 
                                                                                          "Male" & (Offspring_Gender_1 == "Female" | Offspring_Gender_2 == "Female") & Offspring_Gender_1 != Offspring_Gender_2]$Offspring_Party_imputed_1), 
                                                     as.character(couples_2[i.vb.tsmart_last_name.1 == i.vb.tsmart_last_name.2 & vb.tsmart_state %in% party_registration_states & Parental_Gender_2 == 
                                                                              "Male" & (Offspring_Gender_1 == "Female" | Offspring_Gender_2 == "Female") & Offspring_Gender_1 != Offspring_Gender_2]$Offspring_Party_imputed_2))))





mean(factor_sons == factor_fathers)
mean(factor_mothers_sons_sons == factor_mothers_sons_mothers)
mean(factor_fathers_daughters_daugthers == factor_fathers_daughters_fathers)
mean(factor_daughters == factor_mothers)



xtable(CrossTable(factor_mothers, factor_daughters, prop.r = F, prop.c = F, prop.chisq = F, percent = T, dnn = c("Democrat", "Independent")))

xtable(CrossTable(factor_fathers, factor_sons, prop.r = F, prop.c = F, prop.chisq = F, percent = T, dnn = c("Democrat", "Independent")))


###### sampling exercises####################
data$Party <- NA
data$Party <- as.character(data$Party)
data$Party[data$vb.vf_party != "Democrat" & data$vb.vf_party != "Republican" & data$vb.clarity_party_2013_v3 > 69] <- "Democrat"
data$Party[data$vb.vf_party != "Democrat" & data$vb.vf_party != "Republican" & data$vb.clarity_party_2013_v3 < 30] <- "Republican"
data$Party[data$vb.vf_party == "No Party" & data$vb.clarity_party_2013_v3 > 29 & data$vb.clarity_party_2013_v3 < 70] <- "No Party"
data$Party[is.na(data$Party)] <- as.character(data$vb.vf_party[is.na(data$Party)])
data[, `:=`(Party, factor(Party))]



women <- data[vb.tsmart_state %in% party_registration_states & vb.voterbase_gender == "Female" & (Party == "Republican" | Party == "No Party" | 
                                                                                                    Party == "Democrat") & !is.na(vb.tsmart_zip) & nchar(census_id) == 15]
men <- data[vb.tsmart_state %in% party_registration_states & vb.voterbase_gender == "Male" & (Party == "Republican" | Party == "No Party" | Party == 
                                                                                                "Democrat") & !is.na(vb.tsmart_zip) & nchar(census_id) == 15]
# dim 41991822 8

# select men and women that have counterpart in census block group
women = women[which(census_id %in% men$census_id)]
men = men[which(census_id %in% women$census_id)]



########## Sampling 1: Completely Random
set.seed(21312)
women_sample = sample(nrow(women), 1000)
men_sample = sample(nrow(men), 1000)
setnames(women, colnames(women), paste0(colnames(women), "_female"))
setnames(men, colnames(men), paste0(colnames(men), "_male"))

couples = cbind(women[women_sample], men[men_sample])



couples$homogeneity <- NA
couples$homogeneity[couples$Party_male == "Democrat" & couples$Party_female == "Democrat"] <- 1
couples$homogeneity[couples$Party_male == "Democrat" & couples$Party_female == "Republican"] <- 0
couples$homogeneity[couples$Party_male == "Republican" & couples$Party_female == "Democrat"] <- 0
couples$homogeneity[couples$Party_male == "Republican" & couples$Party_female == "Republican"] <- 1
couples$homogeneity[couples$Party_male == "No Party" & couples$Party_female == "No Party"] <- 1
couples$homogeneity[couples$Party_male == "Republican" & couples$Party_female == "No Party"] <- 0
couples$homogeneity[couples$Party_male == "No Party" & couples$Party_female == "Republican"] <- 0
couples$homogeneity[couples$Party_male == "Democrat" & couples$Party_female == "No Party"] <- 0
couples$homogeneity[couples$Party_male == "No Party" & couples$Party_female == "Democrat"] <- 0
mean(couples$homogeneity, na.rm = T) * 100

# mean= 47.8 error
sqrt(47.8 * (100 - 47.8)/1000)
# 1.579608


########## Sampling 2: Zip Code


men[, `:=`(id, 1:nrow(men))]
men_sample <- rep(NA, 1000)
for (i in 1:1000) {
  men_sample[i] <- sample(men[vb.tsmart_zip_male %in% women[women_sample[i]]$vb.tsmart_zip_female]$id, 1)
}

couples = cbind(women[women_sample], men[id %in% men_sample])



couples$homogeneity <- NA
couples$homogeneity[couples$Party_male == "Democrat" & couples$Party_female == "Democrat"] <- 1
couples$homogeneity[couples$Party_male == "Democrat" & couples$Party_female == "Republican"] <- 0
couples$homogeneity[couples$Party_male == "Republican" & couples$Party_female == "Democrat"] <- 0
couples$homogeneity[couples$Party_male == "Republican" & couples$Party_female == "Republican"] <- 1
couples$homogeneity[couples$Party_male == "No Party" & couples$Party_female == "No Party"] <- 1
couples$homogeneity[couples$Party_male == "Republican" & couples$Party_female == "No Party"] <- 0
couples$homogeneity[couples$Party_male == "No Party" & couples$Party_female == "Republican"] <- 0
couples$homogeneity[couples$Party_male == "Democrat" & couples$Party_female == "No Party"] <- 0
couples$homogeneity[couples$Party_male == "No Party" & couples$Party_female == "Democrat"] <- 0
mean(couples$homogeneity, na.rm = T) * 100
# mean= 53.7 error
sqrt(53.7 * (100 - 53.7)/1000)
# 1.576804

########## Sampling 3: Block Group


men_sample <- rep(NA, 1000)
for (i in 1:1000) {
  men_sample[i] <- sample(men[census_id_male %in% women[women_sample[i]]$census_id_female]$id, 1)
}

couples = cbind(women[women_sample], men[id %in% men_sample])

couples$homogeneity <- NA
couples$homogeneity[couples$Party_male == "Democrat" & couples$Party_female == "Democrat"] <- 1
couples$homogeneity[couples$Party_male == "Democrat" & couples$Party_female == "Republican"] <- 0
couples$homogeneity[couples$Party_male == "Republican" & couples$Party_female == "Democrat"] <- 0
couples$homogeneity[couples$Party_male == "Republican" & couples$Party_female == "Republican"] <- 1
couples$homogeneity[couples$Party_male == "No Party" & couples$Party_female == "No Party"] <- 1
couples$homogeneity[couples$Party_male == "Republican" & couples$Party_female == "No Party"] <- 0
couples$homogeneity[couples$Party_male == "No Party" & couples$Party_female == "Republican"] <- 0
couples$homogeneity[couples$Party_male == "Democrat" & couples$Party_female == "No Party"] <- 0
couples$homogeneity[couples$Party_male == "No Party" & couples$Party_female == "Democrat"] <- 0
mean(couples$homogeneity, na.rm = T) * 100

# mean= 55.5 error
sqrt(55.5 * (100 - 55.5)/1000)
# 1.571544



### Race
data[, `:=`(race, recode(as.numeric(vb.voterbase_race), "1='B';2='W'; 3='A';4='A';5='H';6='O';7='O';8='O';9='O';10='O';11='A';12=NA;13='A'"))]

race = data[race == "B" | race == "A" | race == "W" | race == "H"]  #|race=='O']

zip_code_race = race[, list(B = mean(race == "B"), A = mean(race == "A"), W = mean(race == "W"), H = mean(race == "H")  #,O=mean(race=='O')
                            , 
                            COUNT = .N), by = "vb.tsmart_zip"]

zip_code_race[, `:=`(Homogeneity, max(B, W, A, H)), by = 1:nrow(zip_code_race)]

zip_code_race$Mode = colnames(zip_code_race[, 2:5, with = F])[max.col(zip_code_race[, 2:5, with = F], ties.method = "first")]

#save(zip_code_race, file = "~/Vfile/zip-code-level_racial_homogeneity.RData")

race <- data[complete.cases(data[, .(vb.tsmart_zip, vb.tsmart_full_address, vb.voterbase_gender, race)])]

race = race[, `:=`(COUNT = .N), by = c("vb.tsmart_zip", "vb.tsmart_full_address")]

race = race[COUNT == 2]

race[, `:=`(indicator, .I[1]), by = c("vb.tsmart_zip", "vb.tsmart_full_address")]

race[, `:=`(indicator, as.numeric(factor(indicator)))]

setkey(race, indicator)

race1 = unique(race)
duplicates = duplicated(race)
race2 = race[duplicates]

setnames(race1, names(race1), paste0(names(race1), "_1"))
setnames(race2, names(race2), paste0(names(race2), "_2"))

setkey(race1, "indicator_1")
setkey(race2, "indicator_2")

race = race1[race2]

race = race[(vb.voterbase_gender_1 == "Male" & vb.voterbase_gender_2 == "Female") | (vb.voterbase_gender_1 == "Female" & vb.voterbase_gender_2 == 
                                                                                       "Male")]

race$homogeneity <- NA
race$homogeneity[race$race_1 == "B" & race$race_2 == "B"] <- 1
race$homogeneity[race$race_1 == "A" & race$race_2 == "A"] <- 1
race$homogeneity[race$race_1 == "W" & race$race_2 == "W"] <- 1
race$homogeneity[race$race_1 == "H" & race$race_2 == "H"] <- 1

race$homogeneity[race$race_1 == "B" & race$race_2 == "A"] <- 0
race$homogeneity[race$race_1 == "A" & race$race_2 == "B"] <- 0

race$homogeneity[race$race_1 == "B" & race$race_2 == "H"] <- 0
race$homogeneity[race$race_1 == "H" & race$race_2 == "B"] <- 0

race$homogeneity[race$race_1 == "B" & race$race_2 == "W"] <- 0
race$homogeneity[race$race_1 == "W" & race$race_2 == "B"] <- 0

race$homogeneity[race$race_1 == "W" & race$race_2 == "A"] <- 0
race$homogeneity[race$race_1 == "A" & race$race_2 == "W"] <- 0

race$homogeneity[race$race_1 == "W" & race$race_2 == "H"] <- 0
race$homogeneity[race$race_1 == "H" & race$race_2 == "W"] <- 0

race$homogeneity[race$race_1 == "H" & race$race_2 == "A"] <- 0
race$homogeneity[race$race_1 == "A" & race$race_2 == "H"] <- 0

mean((race$race_1 == "W" & race$race_2 == "A") | (race$race_1 == "A" & race$race_2 == "W"), na.rm = T)  #0.82%
mean((race$race_1 == "H" & race$race_2 == "W") | (race$race_1 == "W" & race$race_2 == "H"), na.rm = T)  #2.60%
mean((race$race_1 == "B" & race$race_2 == "W") | (race$race_1 == "W" & race$race_2 == "B"), na.rm = T)  #2.51%

mean((race$race_1 == "A" & race$race_2 == "H") | (race$race_1 == "H" & race$race_2 == "A"), na.rm = T)  #0.13%
mean((race$race_1 == "A" & race$race_2 == "B") | (race$race_1 == "B" & race$race_2 == "A"), na.rm = T)  #0.07%


mean((race$race_1 == "B" & race$race_2 == "H") | (race$race_1 == "H" & race$race_2 == "B"), na.rm = T)  #0.27%%

mean(race$homogeneity, na.rm = T)  #93.35%

zipcode_couples_race = race[, list(homogeneity = mean(abs(homogeneity), na.rm = T), N = length(homogeneity)), by = vb.tsmart_zip_2]


#save(zipcode_couples_race, file = "~/Vfile/zip-code-level_spousal_racial_homogeneity_folded.RData")



############################### Match back to 2016, looking for new relationships#####

# match on first name, last name, date of birth, need: gender, party,age, full address, zip

singles = data[complete.cases(data[, .(vb.tsmart_last_name, vb.tsmart_first_name, vb.voterbase_dob, vb.tsmart_full_address, vb.tsmart_zip, vb.voterbase_age, 
                                       vb.voterbase_gender, vb.vf_party)])]

# indicator for definition of singles
singles[, `:=`(COUNT = .N), by = c("vb.tsmart_zip", "vb.tsmart_full_address")]

singles = singles[COUNT == 1]



### Party: Unaffiliated: registered in non-partisan registration state, No Party as Independents impute party: could change to 79, 20
singles[, `:=`(Party, NA)]
singles$Party[singles$vb.vf_party != "Democrat" & singles$vb.vf_party != "Republican" & singles$vb.clarity_party_2013_v3 > 69] <- "Democrat"
singles$Party[singles$vb.vf_party != "Democrat" & singles$vb.vf_party != "Republican" & singles$vb.clarity_party_2013_v3 < 30] <- "Republican"
# adjust depending on whether we want all states
singles$Party[singles$vb.vf_party == "No Party" & singles$vb.clarity_party_2013_v3 > 29 & singles$vb.clarity_party_2013_v3 < 70] <- "No Party"
# singles$Party[(singles$vb.vf_party=='No Party'|singles$vb.vf_party=='Unaffiliated')& singles$vb.clarity_party_2013_v3>29 &
# singles$vb.clarity_party_2013_v3<70]<-'No Party'

singles$Party[is.na(singles$Party)] <- as.character(singles$vb.vf_party[is.na(singles$Party)])
singles[, `:=`(Party, factor(Party))]
setkey(singles, voterbase_id)
singles <- unique(singles)
rm(list = setdiff(ls(), "singles"))


################### Set UP 2016
i = list.files("/share/data/voting-data/targetsmart_2016/", pattern = "tsmart_analytic", full.names = T)

aggregation <- function() {
  
  tab5rows = read.csv(i, nrows = 1, sep = "\t")
  classes <- rep("NULL", ncol(tab5rows))
  # use census ID from registration address 2012 instead of current address
  names_indic = c("vb.tsmart_zip", "vb.tsmart_full_address", "vb.vf_party", "vb.voterbase_gender", "vb.voterbase_age", "vb.tsmart_last_name", 
                  "vb.voterbase_race", "vb.tsmart_state", "ts.tsmart_partisan_score", "vb.vf_pp2012_party", "vb.vf_pp2012", "vb.tsmart_census_id", "vb.education", 
                  "vb.vf_reg_zip", "vb.voterbase_dob", "vb.tsmart_first_name", "vb.tsmart_middle_name", "voterbase_id")
  indic <- NA
  for (p in 1:length(names_indic)) {
    indic[p] <- which(names(tab5rows) == names_indic[p])
  }
  
  classes[indic] <- c(rep("character", 2), rep(NA, 3), "character", rep(NA, 5), "character", rep(NA, 3), rep("character", 3))
  data <- read.csv(i, sep = "\t", colClasses = classes, na.strings = c("NA", "NaN", " ", ""))
  data = data.table(data)
  
  data
}
numCores <- 20
cl <- makeCluster(numCores, file = "test")
registerDoParallel(cl)



couples_new <- foreach::foreach(i = i, .packages = c("dplyr", "car", "data.table"), .verbose = T) %dopar% aggregation()
stopCluster(cl)

data_new = do.call("rbind", couples_new)

data_new = data_new[, `:=`(COUNT = .N), by = c("vb.tsmart_zip", "vb.tsmart_full_address")]

couples_2016 = data_new[COUNT == 2]


couples_2016[, `:=`(indicator = .I[1]), by = c("vb.tsmart_zip", "vb.tsmart_full_address")]


setkey(couples_2016, indicator)
setorder(couples_2016, indicator, -vb.voterbase_age)


# impute party impute party: could change to 79, 20
couples_2016[, `:=`(Party, NA)]
couples_2016$Party[couples_2016$vb.vf_party != "Democrat" & couples_2016$vb.vf_party != "Republican" & couples_2016$ts.tsmart_partisan_score > 69] <- "Democrat"
couples_2016$Party[couples_2016$vb.vf_party != "Democrat" & couples_2016$vb.vf_party != "Republican" & couples_2016$ts.tsmart_partisan_score < 30] <- "Republican"
# adjust depending on whether we want all states
couples_2016$Party[couples_2016$vb.vf_party == "No Party" & couples_2016$ts.tsmart_partisan_score > 29 & couples_2016$ts.tsmart_partisan_score < 
                     70] <- "No Party"
# couples_2016$Party[(couples_2016$vb.vf_party=='No Party'|couples_2016$vb.vf_party=='Unaffiliated')& couples_2016$ts.tsmart_partisan_score>29 &
# couples_2016$ts.tsmart_partisan_score<70]<-'No Party'


couples_2016$Party[is.na(couples_2016$Party)] <- as.character(couples_2016$vb.vf_party[is.na(couples_2016$Party)])
couples_2016[, `:=`(Party, factor(Party))]

# recode education
couples_2016[, `:=`(education, car::recode(vb.education, "0=NA;1=1;2=2;3=3;4=3;6=1"))]

# split
first_out <- which(duplicated(couples_2016, by = "indicator"))  #second oldest
family_dyad_1 <- couples_2016[first_out]
couples_2016_2 = couples_2016[-first_out]
setkey(family_dyad_1, indicator)
setorder(family_dyad_1, indicator, -vb.voterbase_age)
setkey(couples_2016_2, indicator)
setorder(couples_2016_2, indicator, -vb.voterbase_age)


# second_out<-which(duplicated(couples_2016_2, by='indicator'))#works, only N=2 dyads

length(which(duplicated(couples_2016_2, by = "indicator")))
length(which(duplicated(family_dyad_1, by = "indicator")))
setnames(family_dyad_1, names(family_dyad_1), paste0(names(family_dyad_1), "_1"))
setnames(couples_2016_2, names(couples_2016_2), paste0(names(couples_2016_2), "_2"))

couples_2016_2 = family_dyad_1[couples_2016_2]

####################### 
fathers = couples_2016_2[vb.voterbase_gender_1 == "Male", grep("_1", colnames(couples_2016_2)), with = F]
setnames(fathers, colnames(fathers), gsub("_1", "_fathers", colnames(fathers)))


fathers_2 = cbind(couples_2016_2[vb.voterbase_gender_2 == "Male"]$indicator_1, couples_2016_2[vb.voterbase_gender_2 == "Male", grep("_2", colnames(couples_2016_2)), 
                                                                                              with = F])
setnames(fathers_2, "V1", "indicator_fathers")
setnames(fathers_2, grep("_2", colnames(fathers_2), value = T), gsub("_2", "_fathers", grep("_2", colnames(fathers_2), value = T)))

fathers = rbind(fathers, fathers_2)

# mothers
mothers = couples_2016_2[vb.voterbase_gender_1 == "Female", grep("_1", colnames(couples_2016_2)), with = F]
setnames(mothers, colnames(mothers), gsub("_1", "_mothers", colnames(mothers)))


mothers_2 = cbind(couples_2016_2[vb.voterbase_gender_2 == "Female"]$indicator_1, couples_2016_2[vb.voterbase_gender_2 == "Female", grep("_2", colnames(couples_2016_2)), 
                                                                                                with = F])
setnames(mothers_2, "V1", "indicator_mothers")
setnames(mothers_2, grep("_2", colnames(mothers_2), value = T), gsub("_2", "_mothers", grep("_2", colnames(mothers_2), value = T)))

mothers = rbind(mothers, mothers_2)

setkey(mothers, "indicator_mothers")
setkey(fathers, "indicator_fathers")
couples_2016_married = fathers[mothers]

#### match together new relationships, no same last name
couples_2016_married[, `:=`(indicator_fathers, voterbase_id_fathers)]
couples_2016_married[, `:=`(indicator_mothers, voterbase_id_mothers)]

couples_2016_married = couples_2016_married[indicator_fathers %in% singles$voterbase_id & indicator_mothers %in% singles$voterbase_id]
# HERE
setkey(couples_2016_married, indicator_fathers)
couples_2016_married <- unique(couples_2016_married)
setkey(couples_2016_married, indicator_mothers)
couples_2016_married <- unique(couples_2016_married)
couples_2016_married = couples_2016_married[indicator_fathers != indicator_mothers]
duplicate_pairs = c(couples_2016_married$indicator_fathers, couples_2016_married$indicator_mothers)[duplicated(c(couples_2016_married$indicator_fathers, 
                                                                                                                 couples_2016_married$indicator_mothers))]
couples_2016_married = couples_2016_married[!indicator_fathers %in% duplicate_pairs & !indicator_mothers %in% duplicate_pairs]
# test
summary(duplicated(c(couples_2016_married$indicator_fathers, couples_2016_married$indicator_mothers)))

singles = singles[, .(voterbase_id, Party, vb.tsmart_zip)]
setnames(singles, c("Party", "vb.tsmart_zip"), c("Party_old_fathers", "zip_old_fathers"))
setkey(singles, voterbase_id)
setkey(couples_2016_married, indicator_fathers)
couples_2016_married = singles[couples_2016_married]


setnames(singles, c("Party_old_fathers", "zip_old_fathers"), c("Party_old_mothers", "zip_old_mothers"))
setkey(singles, voterbase_id)
setkey(couples_2016_married, indicator_mothers)
couples_2016_married = singles[couples_2016_married]
setnames(couples_2016_married, grep("_fathers", colnames(couples_2016_married), value = T), gsub("_fathers", "_male", grep("_fathers", colnames(couples_2016_married), 
                                                                                                                           value = T)))
setnames(couples_2016_married, grep("_mothers", colnames(couples_2016_married), value = T), gsub("_mothers", "_female", grep("_mothers", colnames(couples_2016_married), 
                                                                                                                             value = T)))



# 434,778 new couples
#save(couples_2016_married, file = "~/Vfile/New_pairs_2014_2016_new.RData")

### I am here############

# load('~/Vfile/New_pairs_2014_2016.RData') 434,778 couples
couples_2016_married = couples_2016_married[vb.tsmart_state_male == vb.tsmart_state_female]
# 434,775


# only pid couples_2=couples_2[(Parental_Party_1=='Democrat' | Parental_Party_1=='Republican'|Parental_Party_1=='No
# Party')&(Parental_Party_2=='Democrat' | Parental_Party_2=='Republican'|Parental_Party_2=='No Party')]
couples_2016_married = couples_2016_married[(Party_male == "Democrat" | Party_male == "Republican" | Party_male == "No Party") & (Party_female == 
                                                                                                                                    "Democrat" | Party_female == "Republican" | Party_female == "No Party")]
# 286,483

# drop state inaccuracies depending on whether we want all spouses, or just spouses in partisan registration states
couples_2016_married = couples_2016_married[vb.tsmart_state_male %in% party_registration_states]

# 197,987of couples
couples_2016_married = couples_2016_married[abs(as.numeric(vb.voterbase_age_male) - as.numeric(vb.voterbase_age_female)) <= 5]
# 97,227
couples_2016_married$homogeneity <- NA
couples_2016_married$homogeneity[couples_2016_married$Party_male == "Democrat" & couples_2016_married$Party_female == "Democrat"] <- 1
couples_2016_married$homogeneity[couples_2016_married$Party_male == "Democrat" & couples_2016_married$Party_female == "Republican"] <- 0
couples_2016_married$homogeneity[couples_2016_married$Party_male == "Republican" & couples_2016_married$Party_female == "Democrat"] <- 0
couples_2016_married$homogeneity[couples_2016_married$Party_male == "Republican" & couples_2016_married$Party_female == "Republican"] <- 1
couples_2016_married$homogeneity[couples_2016_married$Party_male == "No Party" & couples_2016_married$Party_female == "No Party"] <- 1
couples_2016_married$homogeneity[couples_2016_married$Party_male == "Republican" & couples_2016_married$Party_female == "No Party"] <- 0
couples_2016_married$homogeneity[couples_2016_married$Party_male == "No Party" & couples_2016_married$Party_female == "Republican"] <- 0
couples_2016_married$homogeneity[couples_2016_married$Party_male == "Democrat" & couples_2016_married$Party_female == "No Party"] <- 0
couples_2016_married$homogeneity[couples_2016_married$Party_male == "No Party" & couples_2016_married$Party_female == "Democrat"] <- 0


mean(couples_2016_married$Party_male == couples_2016_married$Party_female)
# 0.677425 accounting for persuasion both parents at least college:
mean(couples_2016_married$homogeneity[couples_2016_married$education_male == 3 & couples_2016_married$education_female == 3], na.rm = T)
# 0.6985059

mean(couples_2016_married$homogeneity[couples_2016_married$education_male > couples_2016_married$education_female], na.rm = T)
# 0.6657989
mean(couples_2016_married$homogeneity[couples_2016_married$education_male < couples_2016_married$education_female], na.rm = T)
# 0.6577411

mean(couples_2016_married$Party_old_male == couples_2016_married$Party_old_female)
# 0.6472276
crosstab(couples_2016_married$Party_old_male, couples_2016_married$Party_male, prop.t = T)


xtable(crosstab(couples_2016_married$Party_male, couples_2016_married$Party_female, prop.t = T, dnn = c("Male", "Female")))

# education as comparison
mean(couples_2016_married$education_male == couples_2016_married$education_female, na.rm = T)
# 0.5969682

# same last name only
mean(couples_2016_married[vb.tsmart_last_name_male == vb.tsmart_last_name_female]$Party_male == couples_2016_married[vb.tsmart_last_name_male == 
                                                                                                                       vb.tsmart_last_name_female]$Party_female)

# 0.7789694 younger than 25
mean(couples_2016_married[vb.voterbase_age_male >= 25 & vb.voterbase_age_female >= 25]$Party_male == couples_2016_married[vb.voterbase_age_male >= 
                                                                                                                            25 & vb.voterbase_age_female >= 25]$Party_female)



### race######

couples_2016_married[, `:=`(race_male, car::recode(as.numeric(factor(vb.voterbase_race_male)), "1='B';2='A'; 3='W';4='A';5='A';6='H';7='O';8='O';9='O';10='O';11='O';12='A';13=NA"))]
couples_2016_married[, `:=`(race_female, car::recode(as.numeric(factor(vb.voterbase_race_female)), "1='B';2='A'; 3='W';4='A';5='A';6='H';7='O';8='O';9='O';10='O';11='O';12='A';13=NA"))]

couples_2016_married$homogeneity_race <- NA
couples_2016_married$homogeneity_race[couples_2016_married$race_male == "B" & couples_2016_married$race_female == "B"] <- 1
couples_2016_married$homogeneity_race[couples_2016_married$race_male == "A" & couples_2016_married$race_female == "A"] <- 1
couples_2016_married$homogeneity_race[couples_2016_married$race_male == "W" & couples_2016_married$race_female == "W"] <- 1
couples_2016_married$homogeneity_race[couples_2016_married$race_male == "H" & couples_2016_married$race_female == "H"] <- 1

couples_2016_married$homogeneity_race[couples_2016_married$race_male == "B" & couples_2016_married$race_female == "A"] <- 0
couples_2016_married$homogeneity_race[couples_2016_married$race_male == "A" & couples_2016_married$race_female == "B"] <- 0

couples_2016_married$homogeneity_race[couples_2016_married$race_male == "B" & couples_2016_married$race_female == "H"] <- 0
couples_2016_married$homogeneity_race[couples_2016_married$race_male == "H" & couples_2016_married$race_female == "B"] <- 0

couples_2016_married$homogeneity_race[couples_2016_married$race_male == "B" & couples_2016_married$race_female == "W"] <- 0
couples_2016_married$homogeneity_race[couples_2016_married$race_male == "W" & couples_2016_married$race_female == "B"] <- 0

couples_2016_married$homogeneity_race[couples_2016_married$race_male == "W" & couples_2016_married$race_female == "A"] <- 0
couples_2016_married$homogeneity_race[couples_2016_married$race_male == "A" & couples_2016_married$race_female == "W"] <- 0

couples_2016_married$homogeneity_race[couples_2016_married$race_male == "W" & couples_2016_married$race_female == "H"] <- 0
couples_2016_married$homogeneity_race[couples_2016_married$race_male == "H" & couples_2016_married$race_female == "W"] <- 0

couples_2016_married$homogeneity_race[couples_2016_married$race_male == "H" & couples_2016_married$race_female == "A"] <- 0
couples_2016_married$homogeneity_race[couples_2016_married$race_male == "A" & couples_2016_married$race_female == "H"] <- 0

mean(couples_2016_married[vb.voterbase_race_male != "O" & vb.voterbase_race_female != "O"]$homogeneity_race, na.rm = T)

# 0.9521388


########################################## 

# singles_2016 indicator for definition of singles

singles_2016 = data_new[COUNT == 1]



### Party: Unaffiliated: registered in non-partisan registration state, No Party as Independents impute party: could change to 79, 20
singles_2016[, `:=`(Party, NA)]
singles_2016$Party[singles_2016$vb.vf_party != "Democrat" & singles_2016$vb.vf_party != "Republican" & singles_2016$vb.clarity_party_2013_v3 > 69] <- "Democrat"
singles_2016$Party[singles_2016$vb.vf_party != "Democrat" & singles_2016$vb.vf_party != "Republican" & singles_2016$vb.clarity_party_2013_v3 < 30] <- "Republican"
# adjust depending on whether we want all states
singles_2016$Party[singles_2016$vb.vf_party == "No Party" & singles_2016$vb.clarity_party_2013_v3 > 29 & singles_2016$vb.clarity_party_2013_v3 < 
                     70] <- "No Party"
# singles_2016$Party[(singles_2016$vb.vf_party=='No Party'|singles_2016$vb.vf_party=='Unaffiliated')& singles_2016$vb.clarity_party_2013_v3>29 &
# singles_2016$vb.clarity_party_2013_v3<70]<-'No Party'

singles_2016$Party[is.na(singles_2016$Party)] <- as.character(singles_2016$vb.vf_party[is.na(singles_2016$Party)])
singles_2016[, `:=`(Party, factor(Party))]
setkey(singles_2016, voterbase_id)
singles_2016 <- unique(singles_2016)



couples_2016_divorced = couples_2_married[voterbase_indicator_mothers %in% singles_2016$voterbase_id & voterbase_indicator_fathers %in% singles_2016$voterbase_id]

singles_2016_female = subset(singles_2016[vb.voterbase_gender == "Female"], select = c("vb.tsmart_last_name", "voterbase_id", "Party"))
setnames(singles_2016_female, c("vb.tsmart_last_name", "Party"), c("last_name_mothers_new", "Party_mothers_new"))
setkey(singles_2016_female, "voterbase_id")
setkey(couples_2016_divorced, "voterbase_indicator_mothers")
couples_2016_divorced = singles_2016_female[couples_2016_divorced]
couples_2016_divorced = couples_2016_divorced[last_name_mothers != last_name_mothers_new]

singles_2016_male = subset(singles_2016[vb.voterbase_gender == "Male"], select = c("vb.tsmart_last_name", "voterbase_id", "Party"))
setnames(singles_2016_male, c("vb.tsmart_last_name", "Party"), c("last_name_fathers_new", "Party_fathers_new"))
setkey(singles_2016_male, "voterbase_id")
setkey(couples_2016_divorced, "voterbase_indicator_fathers")
couples_2016_divorced = singles_2016_male[couples_2016_divorced]


# here

# save(couples_2016_divorced,file='~/Vfile/Divorced_pairs_2014_2016.RData')

# drop state inaccuracies depending on whether we want all spouses, or just spouses in partisan registration states


# 197,987of couples 97,227
couples_2016_divorced$homogeneity <- NA
couples_2016_divorced$homogeneity[couples_2016_divorced$Parental_Party_imputed_fathers == "Democrat" & couples_2016_divorced$Parental_Party_imputed_mothers == 
                                    "Democrat"] <- 1
couples_2016_divorced$homogeneity[couples_2016_divorced$Parental_Party_imputed_fathers == "Democrat" & couples_2016_divorced$Parental_Party_imputed_mothers == 
                                    "Republican"] <- 0
couples_2016_divorced$homogeneity[couples_2016_divorced$Parental_Party_imputed_fathers == "Republican" & couples_2016_divorced$Parental_Party_imputed_mothers == 
                                    "Democrat"] <- 0
couples_2016_divorced$homogeneity[couples_2016_divorced$Parental_Party_imputed_fathers == "Republican" & couples_2016_divorced$Parental_Party_imputed_mothers == 
                                    "Republican"] <- 1
couples_2016_divorced$homogeneity[couples_2016_divorced$Parental_Party_imputed_fathers == "No Party" & couples_2016_divorced$Parental_Party_imputed_mothers == 
                                    "No Party"] <- 1
couples_2016_divorced$homogeneity[couples_2016_divorced$Parental_Party_imputed_fathers == "Republican" & couples_2016_divorced$Parental_Party_imputed_mothers == 
                                    "No Party"] <- 0
couples_2016_divorced$homogeneity[couples_2016_divorced$Parental_Party_imputed_fathers == "No Party" & couples_2016_divorced$Parental_Party_imputed_mothers == 
                                    "Republican"] <- 0
couples_2016_divorced$homogeneity[couples_2016_divorced$Parental_Party_imputed_fathers == "Democrat" & couples_2016_divorced$Parental_Party_imputed_mothers == 
                                    "No Party"] <- 0
couples_2016_divorced$homogeneity[couples_2016_divorced$Parental_Party_imputed_fathers == "No Party" & couples_2016_divorced$Parental_Party_imputed_mothers == 
                                    "Democrat"] <- 0


mean(couples_2016_divorced$Parental_Party_imputed_fathers == couples_2016_divorced$Parental_Party_imputed_mothers, na.rm = T)
mean(couples_2016_divorced$education_mothers == couples_2016_divorced$education_fathers, na.rm = T)
mean(couples_2016_divorced$Party_mothers_new == couples_2016_divorced$Party_fathers_new, na.rm = T)


# zip-code_level-spousal homogeneity against overall zip-code-level homogeneity household-level mean for zip-codes<(-.5)=-0.67 household-level
# mean for zip-codes>(.5)=0.73
#load("homogeneity_by_zip_registration_partisan_denominator.RData", verbose = T)  #this is folded
#load("zip-code-level_spousal_homogeneity_folded+indep.RData", verbose = T)
setkey(zip_code_just_partisan, zip)
setkey(zipcode_couples_ind, zip_fathers)
zipcode_couples <- zip_code_just_partisan[zipcode_couples_ind]
setnames(zipcode_couples, c("homogeneity", "Homogeneity"), c("spousal_homogeneity", "contextual_homogeneity"))
zipcode_couples <- zipcode_couples[zipcode_couples$N >= 100 & zip != 0]  #at least 34 respondents in zip-code
# 12824 zipcodes, with a median resident number of 808
histogram(zipcode_couples$contextual_homogeneity, freq = F, breaks = 20, type = "percent", xlab = "Contextual Homogeneity - Registration")
# drop zip codes 0 and zip codes with fewer than 10 people spousal_homogeneity=spousal_homogeneity[zip!=0 & N>10]
zipcode_couples$Context = "Neutral/Mixed"
zipcode_couples$Context[zipcode_couples$contextual_homogeneity < (-0.25)] <- "Democrat"
zipcode_couples$Context[zipcode_couples$contextual_homogeneity > 0.25] <- "Republican"
model_party_effect <- lm(spousal_homogeneity ~ abs(contextual_homogeneity), data = zipcode_couples)
predict(model_party_effect, with(zipcode_couples, data.frame(contextual_homogeneity = c(mean(abs(contextual_homogeneity)), mean(abs(contextual_homogeneity)) + 
                                                                                          sd(abs(contextual_homogeneity)), mean(abs(contextual_homogeneity)) - sd(abs(contextual_homogeneity))))))
# sposual similarity in most diverse zip-codes
summary(zipcode_couples[abs(contextual_homogeneity) == min(abs(contextual_homogeneity))])
# folded
p <- ggplot(zipcode_couples, aes(contextual_homogeneity, spousal_homogeneity)) + ylim(c(0.5, 1)) + theme_bw(base_size = 20) + xlab("Contextual Homogeneity") + 
  ylab("Spousal Homogeneity") + ggtitle("") + theme(axis.ticks.x = element_blank(), axis.title.x = element_text(face = "bold"), axis.title.y = element_text(face = "bold"), 
                                                    axis.text.x = element_text(face = "bold"), axis.text.y = element_text(face = "bold"), legend.text = element_text(face = "bold"), legend.title = element_text(face = "bold"))
p <- p + stat_bin_hex(aes(colour = Context, fill = Context, alpha = ..count..), bins = 40) + geom_smooth(aes(colour = Context))
p <- p + scale_color_manual("Context", values = c("blue", "black", "red")) + scale_fill_manual("Context and Choice", values = c("blue", "black", 
                                                                                                                                "red")) + guides(fill = F) + scale_alpha("No. Zip Codes in Bin")
#ggplot2::ggsave(filename = "C:/Users/tobia/Dropbox/Partisan Homogeneity Project/Graphs/Contextual_vs_selection.pdf", plot = p, width = 12, height = 8)
# repeat for race zip-code_level-spousal homogeneity against overall zip-code-level homogeneity household-level mean for zip-codes<(-.5)=-0.67
# household-level mean for zip-codes>(.5)=0.73
#load("zip-code-level_racial_homogeneity.RData", verbose = T)  #this is folded
#load("zip-code-level_spousal_racial_homogeneity_folded.RData", verbose = T)
setkey(zip_code_race, vb.tsmart_zip)
setkey(zipcode_couples_race, vb.tsmart_zip_2)
zipcode_couples_race <- zip_code_race[zipcode_couples_race]
setnames(zipcode_couples_race, c("homogeneity", "Homogeneity"), c("spousal_homogeneity", "contextual_homogeneity"))
zipcode_couples_race <- zipcode_couples_race[zipcode_couples_race$N >= 100 & zipcode_couples_race$vb.tsmart_zip != 0]  #at least 100 respondents in zip-code
# 23609 zipcodes, with a mean resident number of 652 racail homogeneity rate
summary(zipcode_couples_race[abs(contextual_homogeneity) == min(abs(contextual_homogeneity))])
# folded
p <- ggplot(zipcode_couples_race, aes(contextual_homogeneity, spousal_homogeneity)) + ylim(c(0.5, 1)) + theme_bw(base_size = 20) + xlab("Contextual Homogeneity") + 
  ylab("Spousal Homogeneity") + ggtitle("") + theme(axis.ticks.x = element_blank(), axis.title.x = element_text(face = "bold"), axis.title.y = element_text(face = "bold"), 
                                                    axis.text.x = element_text(face = "bold"), axis.text.y = element_text(face = "bold"), legend.text = element_text(face = "bold"), legend.title = element_text(face = "bold"))
p <- p + stat_bin_hex(aes(alpha = ..count..), fill = "black", bins = 100) + geom_smooth(colour = "black", fill = "grey26")
p <- p + scale_alpha("No. Zip Codes in Bin")
#ggplot2::ggsave(filename = "C:/Users/tobia/Dropbox/Partisan Homogeneity Project/Graphs/Contextual_vs_selection_race.pdf", plot = p, width = 12, 
                height = 8)
# regression estimates rescale race variable
zipcode_couples_race$contextual_homogeneity <- (zipcode_couples_race$contextual_homogeneity - 0.25)/(1 - 0.25)
model_race_effect <- lm(spousal_homogeneity ~ contextual_homogeneity, data = zipcode_couples_race)
predict(model_race_effect, with(zipcode_couples_race, data.frame(contextual_homogeneity = c(mean(abs(contextual_homogeneity)), mean(abs(contextual_homogeneity)) + 
                                                                                              sd(abs(contextual_homogeneity)), mean(abs(contextual_homogeneity)) - sd(abs(contextual_homogeneity))))))
model_race <- lm(spousal_homogeneity ~ abs(contextual_homogeneity), data = zipcode_couples_race)
model_partisan <- lm(spousal_homogeneity ~ abs(contextual_homogeneity), data = zipcode_couples)
model_partisan$se <- coeftest(model_partisan, vcov = vcovHC(model_partisan, type = "HC1", cluster = "group"))[, 2]
model_race$se <- coeftest(model_race, vcov = vcovHC(model_race, type = "HC1", cluster = "group"))[, 2]
apsrtable(model_race, model_partisan, se = "robust")
############################ at the blockgrouplevel###################################
#load("blockgroup-level_spousal_homogeneity_folded+indep.RData", verbose = T)  #this is folded
#load("homogeneity_by_blockgroup_registration_partisan_denominator.RData", verbose = T)
setnames(blockgroup_couples_ind, "blgrp_mothers", "census")
setkey(blockgroup_couples_ind, census)
setkey(blockgroup_just_partisan, census)
blockgroup_couples <- blockgroup_just_partisan[blockgroup_couples_ind]
setnames(blockgroup_couples, c("homogeneity", "Homogeneity"), c("spousal_homogeneity", "contextual_homogeneity"))
histogram(blockgroup_couples$contextual_homogeneity, freq = F, breaks = 20, type = "percent", xlab = "Contextual Homogeneity - Registration")
# drop zip codes 0 and zip codes with fewer than 10 people spousal_homogeneity=spousal_homogeneity[zip!=0 & N>10]
blockgroup_couples$Context = "Neutral/Mixed"
blockgroup_couples$Context[blockgroup_couples$contextual_homogeneity < (-0.25)] <- "Democrat"
blockgroup_couples$Context[blockgroup_couples$contextual_homogeneity > 0.25] <- "Republican"
# folded
p <- ggplot(blockgroup_couples, aes(contextual_homogeneity, spousal_homogeneity)) + ylim(c(0.5, 1)) + theme_bw(base_size = 20) + xlab("Contextual Homogeneity") + 
  ylab("Spousal Homogeneity") + ggtitle("") + theme(axis.ticks.x = element_blank(), axis.title.x = element_text(face = "bold"), axis.title.y = element_text(face = "bold"), 
                                                    axis.text.x = element_text(face = "bold"), axis.text.y = element_text(face = "bold"), legend.text = element_text(face = "bold"), legend.title = element_text(face = "bold"))
p <- p + stat_bin_hex(aes(colour = Context, fill = Context, alpha = ..count..), bins = 10) + geom_smooth(colour = "black", fill = "grey26")
p <- p + scale_color_manual("Context", values = c("blue", "black", "red")) + scale_fill_manual("Context and Choice", values = c("blue", "black", 
                                                                                                                                "red")) + guides(fill = F) + scale_alpha("No. Block Groups in Bin")
#ggplot2::ggsave(filename = "C:/Users/tobia/Dropbox/Partisan Homogeneity Project/Graphs/Contextual_vs_selection_block.pdf", plot = p, width = 15, 
                height = 10)
