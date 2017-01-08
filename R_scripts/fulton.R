# OpenElectionsData-NY
# https://github.com/openelections/openelections-data-ny

# Fulton County, NY General Election Results for:
# Presidential Election
# US Senate
# US House of Representatives (21st Congressional District)
# NY State Senate (49th Senatorial District)
# NY State Assembly (118th Assembly District)

# load data -------------------------------------------------------------------

library(tidyr)
library(stats)

# example dataframe: Yates County
# yates <- read.csv("data/Yates.csv")

getwd()
setwd("~/GitHub/openelections-cleansing-scripts/")

# Fulton County: General Election, President ----------------------------------

# extracted from PDF with Tableau, then preprocessed in google sheets
rawPresidential <- read.csv("data/Fulton/data-V1/Fulton-Presidential.csv")
colnames(rawPresidential)[1] <- "precinct"

rList <- colnames(rawPresidential)[2:12]

# reshape dataframe from wide to long
# would have used `stack()` but the data came in with TWO rows needing to be spread.
r2 <- reshape(rawPresidential, varying = rList,
              v.names = "votes", 
              timevar = "party", 
              times = rList,
              direction = "long", 
              new.row.names = 1:1000)

# separate party and candidate
presidential <- r2 %>% separate(party, into = c("party", "candidate"))

# full names for candidates
presidential$candidate[presidential$candidate == "Clinton"] <- "Hillary Clinton"
presidential$candidate[presidential$candidate == "Trump"] <- "Donald Trump"
presidential$candidate[presidential$candidate == "Stein"] <- "Jill Stein"
presidential$candidate[presidential$candidate == "Johnson"] <- "Gary Johnson"

# preserve PDF spelling
presidential$candidate[presidential$candidate == "WriteIns"] <- "Write-Ins"
# convert to factor
presidential$candidate <- factor(presidential$candidate)

# remove R dummy headers
presidential$party[presidential$party == "x1" | presidential$party == "x2" | presidential$party == "x3"] <- ""
presidential$party <- factor(presidential$party)

# add county, office, district
presidential$county <- as.factor("Fulton")
presidential$office <- as.factor("President")
presidential$district <- NA

presidential <- presidential[, c("county", "precinct", "office", "district", 
                                 "party", "candidate", "votes")]

# write.table(presidential, file = "data-output/Fulton-Presidential-result.csv", 
#             sep = ",", row.names = F)


# Fulton County: US Senate Results --------------------------------------------

rawSenate <- read.csv("data/Fulton/data-V1/Fulton-US-Senate.csv")
colnames(rawSenate)[1] <- "precinct"

rawList <- colnames(rawSenate)[2:13]
rawList

r2s2 <- reshape(rawSenate, varying = rawList,
                v.names = "votes", 
                timevar = "party", 
                times = rawList,
                direction = "long",
                new.row.names = 1:1000)

senate <- r2s2 %>% separate(party, into = c("party", "candidate"))

senate$candidate[senate$candidate == "Schumer"] <- "Chuck Schumer"
senate$candidate[senate$candidate == "Long"] <- "Wendy Long"
senate$candidate[senate$candidate == "Merced"] <- "Alex Merced"
senate$candidate[senate$candidate == "Wilson"] <- "Robin Laverne Wilson"
senate$candidate[senate$candidate == "WriteIns"] <- "Write-Ins"

senate$party[senate$party == "x1" | senate$party == "x2" | senate$party == "x3"] <- ""
senate$party <- factor(senate$party)

senate$county <- as.factor("Fulton")
senate$office <- as.factor("U.S. Senate")
senate$district <- NA

senate <- senate[, c("county", "precinct", "office", "district", 
                     "party", "candidate", "votes")]

# write.table(senate, file = "data-output/Fulton-US-Senate-result.csv",
#             sep = ",", row.names = F)

# Fulton County: US House Results ---------------------------------------------

rawHouse <- read.csv("data/Fulton/data-V1/Fulton-US-House.csv")
colnames(rawHouse)[1] <- "precinct"

houseList <- colnames(rawHouse[2:11])

r2h2 <- reshape(rawHouse, varying = houseList,
                v.names = "votes",
                timevar = "party",
                times = houseList,
                direction = "long",
                new.row.names = 1:1000)

house <- r2h2 %>% separate(party, into = c("party", "candidate"))

house$candidate <- gsub("Derrick",  "Mike Derrick", house$candidate)
house$candidate <- gsub("Stefanik", "Elise Stefanik", house$candidate)
house$candidate <- gsub("Funiciello", "Matt Funiciello", house$candidate)
house$candidate <- gsub("WriteIns", "Write-Ins", house$candidate)
house$candidate <- factor(house$candidate)

house$party[house$party == "x1" | house$party == "x2" | house$party == "x3"] <- ""
house$party <- factor(house$party)

house$county <- as.factor("Fulton")
house$office <- as.factor("U.S. House")
house$district <- as.factor("21")

house <- house[, c("county", "precinct", "office", "district", 
                   "party", "candidate", "votes")]

# write.table(house, file = "data-output/Fulton-US-House-results.csv",
#             sep = ",", row.names = F)

# Fulton County: State Senate Results -----------------------------------------

rawStateSenate <- read.csv("data/Fulton/data-V1/Fulton-StateSenate.csv")
colnames(rawStateSenate)[1] <- "precinct"

stateSenateList <- colnames(rawStateSenate[2:11])

s2s2 <- reshape(rawStateSenate, varying = stateSenateList,
                v.names = "votes", 
                timevar = "party",
                times = stateSenateList,
                direction = "long",
                new.row.names = 1:1000)

state_senate <- s2s2 %>% separate(party, into = c("party", "candidate"))

state_senate$candidate[state_senate$candidate == "Putman"] <- "Chad Putman"
state_senate$candidate[state_senate$candidate == "Tedisco"] <- "Jim Tedisco"
state_senate$candidate[state_senate$candidate == "WriteIns"] <- "Write-Ins"
state_senate$candidate <- factor(state_senate$candidate)

state_senate$party[state_senate$party == "x1" | state_senate$party == "x2" | state_senate$party == "x3"] <- ""
state_senate$party <- factor(state_senate$party)

state_senate$county <- as.factor("Fulton")
state_senate$office <- as.factor("State Senate")
state_senate$district <- as.factor("49")

# reorder columns
state_senate <- state_senate[, c("county", "precinct", "office", "district",
                                 "party", "candidate", "votes")]

# write.table(state_senate, file = "data-output/Fulton-StateSenate-result.csv",
#             sep = ",", row.names = F)

# Fulton County: State Assembly Results ---------------------------------------

rawAssembly <- read.csv("data/Fulton/data-V1/Fulton-StateAssembly.csv")
colnames(rawAssembly)[1] <- "precinct"

SA_list <- colnames(rawAssembly)[2:8]

ra2 <- reshape(rawAssembly, varying = SA_list,
               v.names = "votes",
               timevar = "party",
               times = SA_list,
               direction = "long",
               new.row.names = 1:1000)

state_assembly <- ra2 %>% separate(party, into = c("party", "candidate"))
state_assembly$candidate[state_assembly$candidate == "WriteIns"] <- "Write-Ins"
state_assembly$candidate[state_assembly$candidate == "Butler"] <- "Marc Butler"
state_assembly$candidate <- factor(state_assembly$candidate)

state_assembly$party[state_assembly$party == "x1" | state_assembly$party == "x2" | state_assembly$party == "x3"] <- ""
state_assembly$party <- factor(state_assembly$party)

state_assembly$county <- as.factor("Fulton")
state_assembly$office <- as.factor("State House")
state_assembly$district <- as.factor("118")

state_assembly <- state_assembly[, c("county", "precinct", "office", "district",
                                     "party", "candidate", "votes")]

# write.table(state_assembly, file = "data-output/Fulton-StateAssembly-result.csv",
#             sep = ",", row.names = F)


#### Bind all Results into one dataframe --------------------------------------

fulton <- rbind(presidential, senate, house, state_senate, state_assembly)
write.table(fulton, file = "~/GitHub/openelections-cleansing-scripts/20161108__ny__general__fulton__precinct.csv", 
            sep = ",", row.names = F)

