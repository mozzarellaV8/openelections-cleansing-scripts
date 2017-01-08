# OpenElectionsData-NY
# https://github.com/openelections/openelections-data-ny

# Saratoga County, NY ---------------------------------------------------------

# Presidential Election
# US Senate
# US House of Representatives (20th & 21st Congressional District)
# NY State Senate (43rd & 49th Senatorial District)
# NY State Assembly (108th, 112th-114th Assembly District)

# laod libraries --------------------------------------------------------------

library(dplyr)
library(tidyr)

# Presidential results---------------------------------------------------------
pDir <- "~/GitHub/openelections-cleansing-scripts/data/Saratoga/President"
setwd(pDir)

pList <- list.files(pDir, pattern = ".csv", recursive = T, all.files = T)
pres <- data.frame()

for (i in 1:length(pList)) {
  
  # read in data; remove 'Whole Number' column
  temp01 <- read.csv(pList[i], header = F)
  temp01$V2 <- NULL
  
  # transpose data; remove vice-pres name
  temp01 <- as.data.frame(t(temp01))
  temp01$V2 <- gsub("/(.*)", "", temp01$V2)
  
  # convert to character and rename first two columns
  temp01 <- temp01 %>% mutate_if(is.factor, as.character)
  temp01[1, 1:2] <- c("party", "candidate")
  
  # set column names to precinct; remove superflous
  colnames(temp01) <- temp01[1, ]
  temp01 <- temp01[-1, ]
  
  # rename candidates
  temp01$candidate <- ifelse(temp01$candidate == "", "Blank votes", temp01$candidate)
  temp01$candidate <- ifelse(temp01$candidate == "Clinton", "Hillary Clinton",
                             ifelse(temp01$candidate == "Trump", "Donald J. Trump",
                                    ifelse(temp01$candidate == "Stein", "Jill Stein",
                                           ifelse(temp01$candidate == "Johnson", "Gary Johnson", temp01$candidate))))
  
  
  # wide-to-long conversion; stack precinct values as single column and rename
  temp01 <- temp01 %>% gather(key = "precinct", value = "votes", 3:ncol(temp01))
  temp01$precinct <- paste(pList[i], temp01$precinct)
  temp01$precinct <- gsub(".csv", "", temp01$precinct)
  temp01$precinct <- gsub("W:000\\sD:0", "", temp01$precinct)
  
  # rename blank votes in extra columns
  temp01$precinct[temp01$precinct == "Blank Votes"] <- ""
  temp01$party[temp01$party == "Blank Votes"] <- ""
  
  # add columns for county, office, and district
  temp01$county <- "Saratoga"
  temp01$office <- "President"
  temp01$district <- ""
  
  # reorder columns to match
  temp01 <-temp01[, c(5, 3, 6, 7, 1, 2, 4)]
  
  # bind data into one dataframe
  pres <- rbind(pres, temp01)
  
}

rm(temp01)
rm(pList)
rm(pDir)

# US Senate results------------------------------------------------------------
sDir <- "~/Documents/R2/00-openelections-data-ny/pd/data/Saratoga/US_Senate"
setwd(sDir)

sList <- list.files(sDir, pattern = ".csv", recursive = T, all.files = T)
US_Senate <- data.frame()

for (i in 1:length(sList)) {
  
  # read in data and remove "Whole Number" column
  temp02 <- read.csv(sList[i], header = F)
  temp02$V2 <- NULL
  
  # transpose data; unite long candidate names
  temp02 <- as.data.frame(t(temp02))
  temp02 <- temp02 %>% unite(temp02, c(V2, V3), sep = " ")
  
  temp02 <- temp02 %>% mutate_if(is.factor, as.character)
  temp02[1, 1:2] <- c("party", "candidate")
  
  # set precinct numbers as column names; remove superfluous
  colnames(temp02) <- temp02[1, ]
  temp02 <- temp02[-1, ]
  
  # remove trailing whitespace
  temp02$candidate <- gsub("\\s$", "", temp02$candidate)
  
  # assign "Blank Votes" to blank `candidate` fields
  temp02$candidate[temp02$candidate == ""] <- "Blank Votes"
  
  # wide-to-long conversion; stack precinct values as single column and rename
  temp02 <- temp02 %>% gather(key = "precinct", value = "votes", 3:ncol(temp02))
  temp02$precinct <- paste(sList[i], temp02$precinct)
  temp02$precinct <- gsub(".csv", "", temp02$precinct)
  temp02$precinct <- gsub("W:000\\sD:0", "", temp02$precinct)
  
  # remove 'Blank Votes' from extra columns
  temp02$precinct[temp02$precinct == "Blank Votes"] <- ""
  temp02$party[temp02$party == "Blank Votes"] <- ""
  
  # add columns for county, office, and district
  temp02$county <- "Saratoga"
  temp02$office <- "US Senate"
  temp02$district <- ""
  
  temp02 <-temp02[, c(5, 3, 6, 7, 1, 2, 4)]
  
  US_Senate <- rbind(US_Senate, temp02)
  
}

rm(temp02)
rm(sDir)
rm(sList)

# US House 20th results--------------------------------------------------------
h20dir <- "~/Documents/R2/00-openelections-data-ny/pd/data/Saratoga/US_House_20th"
setwd(h20dir)

h20List <- list.files(h20dir, pattern = ".csv", recursive = T, all.files = T)
US_House_20 <- data.frame()

for (i in 1:length(h20List)) {
  
  temp03 <- read.csv(h20List[i], header = F)
  temp03$V2 <- NULL
  
  temp03 <- as.data.frame(t(temp03))
  
  temp03 <- temp03 %>% mutate_if(is.factor, as.character)
  temp03[1, 1:2] <- c("party", "candidate")
  colnames(temp03) <- temp03[1, ]
  temp03 <- temp03[-1, ]
  
  temp03$candidate <- gsub("\\s$", "", temp03$candidate)
  temp03$candidate[temp03$candidate == ""] <- "Blank Votes"
  
  temp03 <- temp03 %>% gather(key = "precinct", value = "votes", 3:ncol(temp03))
  temp03$precinct <- paste(h20List[i], temp03$precinct)
  temp03$precinct <- gsub(".csv", "", temp03$precinct)
  temp03$precinct <- gsub("W:000\\sD:0", "", temp03$precinct)
  
  temp03$precinct[temp03$precinct == "Blank Votes"] <- ""
  temp03$party[temp03$party == "Blank Votes"] <- ""
  
  temp03$county <- "Saratoga"
  temp03$office <- "US House"
  temp03$district <- "20"
  
  temp03 <-temp03[, c(5, 3, 6, 7, 1, 2, 4)]
  
  US_House_20 <- rbind(US_House_20, temp03)
  
}

rm(temp03)
rm(h20dir)
rm(h20List)  
  
# US House 21st results--------------------------------------------------------
h21dir <- "~/Documents/R2/00-openelections-data-ny/pd/data/Saratoga/US_House_21st"
setwd(h21dir)

h21_List <- list.files(h21dir, pattern = ".csv", recursive = T, all.files = T)
US_House_21 <- data.frame()

for (i in 1:length(h21_List)) {
  
  temp04 <- read.csv(h21_List[i], header = F)
  temp04$V2 <- NULL
  
  temp04 <- as.data.frame(t(temp04))
  temp04 <- temp04 %>% unite(temp04, c(V2, V3), sep = " ")
  
  temp04 <- temp04 %>% mutate_if(is.factor, as.character)
  temp04[1, 1:2] <- c("party", "candidate")
  colnames(temp04) <- temp04[1, ]
  temp04 <- temp04[-1, ]
  
  temp04$candidate <- gsub("\\s$", "", temp04$candidate)
  temp04$candidate[temp04$candidate == ""] <- "Blank Votes"
  
  temp04 <- temp04 %>% gather(key = "precinct", value = "votes", 3:ncol(temp04))
  temp04$precinct <- paste(h21_List[i], temp04$precinct)
  temp04$precinct <- gsub(".csv", "", temp04$precinct)
  temp04$precinct <- gsub("W:000\\sD:0", "", temp04$precinct)
  
  temp04$precinct[temp04$precinct == "Blank Votes"] <- ""
  temp04$party[temp04$party == "Blank Votes"] <- ""
  
  temp04$county <- "Saratoga"
  temp04$office <- "US House"
  temp04$district <- "21"
  
  temp04 <-temp04[, c(5, 3, 6, 7, 1, 2, 4)]
  
  US_House_21 <- rbind(US_House_21, temp04)
  
}

rm(temp04)
rm(h21_List)
rm(h21dir)

# State Senate 43rd results----------------------------------------------------

ss43dir <- "~/Documents/R2/00-openelections-data-ny/pd/data/Saratoga/State_Senate_43rd"
setwd(ss43dir)

ss43_list <- list.files(ss43dir, pattern = ".csv", recursive = T, all.files = T)
ss43 <- data.frame()

# has long candidate names

for (i in 1:length(ss43_list)) {
  
  temp05 <- read.csv(ss43_list[i], header = F)
  temp05$V2 <- NULL
  
  temp05 <- as.data.frame(t(temp05))
  temp05 <- temp05 %>% unite(temp05, c(V2, V3), sep = " ")
  
  temp05 <- temp05 %>% mutate_if(is.factor, as.character)
  temp05[1, 1:2] <- c("party", "candidate")
  colnames(temp05) <- temp05[1, ]
  temp05 <- temp05[-1, ]
  
  temp05$candidate <- gsub("\\s$", "", temp05$candidate)
  temp05$candidate[temp05$candidate == ""] <- "Blank Votes"
  
  temp05 <- temp05 %>% gather(key = "precinct", value = "votes", 3:ncol(temp05))
  
  temp05$precinct <- paste(ss43_list[i], temp05$precinct)
  temp05$precinct <- gsub(".csv", "", temp05$precinct)
  temp05$precinct <- gsub("W:000\\sD:0", "", temp05$precinct)
  
  temp05$precinct[temp05$precinct == "Blank Votes"] <- ""
  temp05$party[temp05$party == "Blank Votes"] <- ""
  
  temp05$county <- "Saratoga"
  temp05$office <- "State Senate"
  temp05$district <- "43"
  
  temp05 <-temp05[, c(5, 3, 6, 7, 1, 2, 4)]
  
  ss43 <- rbind(ss43, temp05)
  
}

rm(temp05)
rm(ss43_list)
rm(ss43dir)

# State Senate 49th results----------------------------------------------------
ss49dir <- "~/Documents/R2/00-openelections-data-ny/pd/data/Saratoga/State_Senate_49th"
setwd(ss49dir)

ss49_list <- list.files(ss49dir, pattern = ".csv", recursive = T, all.files = T)
ss49 <- data.frame()

for (i in 1:length(ss49_list)) {
  
  temp06 <- read.csv(ss49_list[i], header = F)
  temp06$V2 <- NULL
  
  temp06 <- as.data.frame(t(temp06))
  
  temp06 <- temp06 %>% mutate_if(is.factor, as.character)
  temp06[1, 1:2] <- c("party", "candidate")
  colnames(temp06) <- temp06[1, ]
  temp06 <- temp06[-1, ]
  
  temp06$candidate[temp06$candidate == ""] <- "Blank Votes"
  
  temp06 <- temp06 %>% gather(key = "precinct", value = "votes", 3:ncol(temp06))
  
  temp06$precinct <- paste(ss49_list[i], temp06$precinct)
  temp06$precinct <- gsub(".csv", "", temp06$precinct)
  temp06$precinct <- gsub("W:000\\sD:0", "", temp06$precinct)
  
  temp06$precinct[temp06$precinct == "Blank Votes"] <- ""
  temp06$party[temp06$party == "Blank Votes"] <- ""
  
  temp06$county <- "Saratoga"
  temp06$office <- "State Senate"
  temp06$district <- "49"
  
  temp06 <-temp06[, c(5, 3, 6, 7, 1, 2, 4)]
  
  ss49 <- rbind(ss49, temp06)
  
}

rm(temp06)
rm(ss49_list)
rm(ss49dir)

# State House 108th results----------------------------------------------------
sh108dir <- "~/Documents/R2/00-openelections-data-ny/pd/data/Saratoga/State_House_108th"
setwd(sh108dir)

sh108_list <- list.files(sh108dir, pattern = ".csv", recursive = T, all.files = T)
sh108 <- data.frame()

for (i in 1:length(sh108_list)) {
  # read in data and remove "Whole Number" column
  temp07 <- read.csv(sh108_list[i], header = F)
  temp07$V2 <- NULL
  
  temp07 <- as.data.frame(t(temp07))
  temp07 <- temp07 %>% unite(temp07, c(V2, V3), sep = " ")
  
  temp07 <- temp07 %>% mutate_if(is.factor, as.character)
  temp07[1, 1:2] <- c("party", "candidate")
  colnames(temp07) <- temp07[1, ]
  temp07 <- temp07[-1, ]
  
  temp07$candidate <- gsub("\\s$", "", temp07$candidate)
  temp07$candidate[temp07$candidate == ""] <- "Blank Votes"
  
  temp07 <- temp07 %>% gather(key = "precinct", value = "votes", 3:ncol(temp07))
  
  temp07$precinct <- paste(sh108_list[i], temp07$precinct)
  temp07$precinct <- gsub(".csv", "", temp07$precinct)
  temp07$precinct <- gsub("W:000\\sD:0", "", temp07$precinct)
  
  temp07$precinct[temp07$precinct == "Blank Votes"] <- ""
  temp07$party[temp07$party == "Blank Votes"] <- ""
  
  temp07$county <- "Saratoga"
  temp07$office <- "State House"
  temp07$district <- "108"
  
  temp07 <-temp07[, c(5, 3, 6, 7, 1, 2, 4)]
  
  sh108 <- rbind(sh108, temp07)
  
}

rm(temp07)
rm(sh108_list)
rm(sh108dir)

# State House 112th results----------------------------------------------------
sh112dir <- "~/Documents/R2/00-openelections-data-ny/pd/data/Saratoga/State_House_112th"
setwd(sh112dir)

sh112_list <- list.files(sh112dir, pattern = ".csv", recursive = T, all.files = T)
sh112 <- data.frame()

for (i in 1:length(sh112_list)) {
  
  temp08 <- read.csv(sh112_list[i], header = F)
  temp08$V2 <- NULL
  
  temp08 <- as.data.frame(t(temp08))
  temp08 <- temp08 %>% unite(temp08, c(V2, V3), sep = " ")
  
  temp08 <- temp08 %>% mutate_if(is.factor, as.character)
  temp08[1, 1:2] <- c("party", "candidate")
  colnames(temp08) <- temp08[1, ]
  temp08 <- temp08[-1, ]
  
  temp08$candidate <- gsub("\\s$", "", temp08$candidate)
  temp08$candidate[temp08$candidate == ""] <- "Blank Votes"
  
  temp08 <- temp08 %>% gather(key = "precinct", value = "votes", 3:ncol(temp08))
  
  temp08$precinct <- paste(sh112_list[i], temp08$precinct)
  temp08$precinct <- gsub(".csv", "", temp08$precinct)
  temp08$precinct <- gsub("W:000\\sD:0", "", temp08$precinct)
  
  temp08$precinct[temp08$precinct == "Blank Votes"] <- ""
  temp08$party[temp08$party == "Blank Votes"] <- ""
  
  temp08$county <- "Saratoga"
  temp08$office <- "State House"
  temp08$district <- "112"
  
  temp08 <-temp08[, c(5, 3, 6, 7, 1, 2, 4)]
  sh112 <- rbind(sh112, temp08)
  
}

rm(temp08)
rm(sh112_list)
rm(sh112dir)

# State House 113th results----------------------------------------------------
sh113dir <- "~/Documents/R2/00-openelections-data-ny/pd/data/Saratoga/State_House_113th"
setwd(sh113dir)

sh113_list <- list.files(sh113dir, pattern = ".csv", recursive = T, all.files = T)
sh113 <- data.frame()

for (i in 1:length(sh113_list)) {
  
  temp09 <- read.csv(sh113_list[i], header = F)
  temp09$V2 <- NULL
  
  temp09 <- as.data.frame(t(temp09))
  temp09 <- temp09 %>% unite(temp09, c(V2, V3), sep = " ")
  
  temp09 <- temp09 %>% mutate_if(is.factor, as.character)
  temp09[1, 1:2] <- c("party", "candidate")
  colnames(temp09) <- temp09[1, ]
  temp09 <- temp09[-1, ]
  
  temp09$candidate <- gsub("\\s$", "", temp09$candidate)
  temp09$candidate[temp09$candidate == ""] <- "Blank Votes"
  
  temp09 <- temp09 %>% gather(key = "precinct", value = "votes", 3:ncol(temp09))
  
  temp09$precinct <- paste(sh113_list[i], temp09$precinct)
  temp09$precinct <- gsub(".csv", "", temp09$precinct)
  temp09$precinct <- gsub("W:000\\sD:0", "", temp09$precinct)
  
  temp09$precinct[temp09$precinct == "Blank Votes"] <- ""
  temp09$party[temp09$party == "Blank Votes"] <- ""
  
  temp09$county <- "Saratoga"
  temp09$office <- "State House"
  temp09$district <- "113"
  
  temp09 <-temp09[, c(5, 3, 6, 7, 1, 2, 4)]
  sh113 <- rbind(sh113, temp09)
  
}

rm(temp09)
rm(sh113_list)
rm(sh113dir)

# State House 114th results----------------------------------------------------
sh114dir <- "~/Documents/R2/00-openelections-data-ny/pd/data/Saratoga/State_House_114th"
setwd(sh114dir)

sh114_list <- list.files(sh114dir, pattern = ".csv", recursive = T, all.files = T)
sh114 <- data.frame()

for (i in 1:length(sh114_list)) {
  
  temp10 <- read.csv(sh114_list[i], header = F)
  temp10$V2 <- NULL
  
  temp10 <- as.data.frame(t(temp10))
  temp10 <- temp10 %>% unite(temp10, c(V2, V3), sep = " ")
  
  temp10 <- temp10 %>% mutate_if(is.factor, as.character)
  temp10[1, 1:2] <- c("party", "candidate")
  colnames(temp10) <- temp10[1, ]
  temp10 <- temp10[-1, ]
  
  temp10$candidate <- gsub("\\s$", "", temp10$candidate)
  temp10$candidate[temp10$candidate == ""] <- "Blank Votes"
  
  temp10 <- temp10 %>% gather(key = "precinct", value = "votes", 3:ncol(temp10))
  
  temp10$precinct <- paste(sh114_list[i], temp10$precinct)
  temp10$precinct <- gsub(".csv", "", temp10$precinct)
  temp10$precinct <- gsub("W:000\\sD:0", "", temp10$precinct)
  
  temp10$precinct[temp10$precinct == "Blank Votes"] <- ""
  temp10$party[temp10$party == "Blank Votes"] <- ""
  
  temp10$county <- "Saratoga"
  temp10$office <- "State House"
  temp10$district <- "114"
  
  temp10 <-temp10[, c(5, 3, 6, 7, 1, 2, 4)]
  sh114 <- rbind(sh114, temp10)
  
}

rm(temp10)
rm(sh114_list)
rm(sh114dir)

# Bind all into single dataframe ----------------------------------------------

setwd("~/GitHub/openelections-cleansing-scripts/")

Saratoga <- rbind(pres, US_Senate, US_House_20, US_House_21, ss43, ss49, sh108,
                  sh112, sh113, sh114)

Saratoga$precinct <- gsub("CliftonPark", "Clifton Park", Saratoga$precinct)
Saratoga$precinct <- gsub("SaratogaSpgs", "Saratoga Spgs", Saratoga$precinct)

write.csv(Saratoga, file = "20161108__ny__general__saratoga__precinct.csv", 
          row.names = F, na = "")

test <- read.csv("20161108__ny__general__saratoga__precinct.csv", na.strings = "",
                 fill = T)
