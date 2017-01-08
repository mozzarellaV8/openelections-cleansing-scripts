# OpenElectionsData-NY
# https://github.com/openelections/openelections-data-ny

# Essex County, NY ------------------------------------------------------------

# Presidential Election
# US Senate
# US House of Representatives (21st Congressional District)
# NY State Senate (45th Senatorial District)
# NY State Assembly (114th Assembly District)

# GENERAL OUTLINE:
# 1. use `for`` loop to read in ~55 csvs and bind

# 2. create columns for:
# county, precinct, office (conditional), district

# 3. rename columns: 
# party, candidate, votes

# 4. convert all precincts with suffix '00' to just the precinct name
# e.g. 'Essex-00' becomes "Essex" to match the source PDF.
# If single zero e.g. "Elizabethtown-0', leave as is (matches source PDF)
# *****or remove dash****

# reorder columns to match, write out csv

# 4. reorder columns to match yates/other counties

# 5. check, fix, check

getwd()
eDir <- "~/GitHub/openelections-cleansing-scripts/data/Essex"
setwd(eDir)

# load data -------------------------------------------------------------------

# list of CSVs to read in and empty dataframe to store incoming
EssexList <- list.files(path = eDir, pattern = ".csv", all.files = T, recursive = T)
essex <- data.frame()

# `for` loop to read in CSVs and then: rename given columns, 
# create/assign values to new columns, and bind together for one dataframe.

system.time(
  for (i in 1:length(EssexList)) {
    
    temp <- read.csv(EssexList[i])
    
    # because of the PDF format
    colnames(temp) <- c("candidate", "votes", "party")
    
    # remove vice-presidential candidates and trailing whitespace
    temp$candidate <- gsub("and(.*)", "", temp$candidate)
    temp$candidate <- gsub("^\\s+|\\s+$", "", temp$candidate)
    
    # `precinct` is the CSV filename
    temp$precinct <- paste(EssexList[i])
    temp$precinct <- gsub(".csv", "", temp$precinct)
    temp$precinct <- gsub("-00", "", temp$precinct)
    temp$precinct <- gsub("-", " ", temp$precinct)
    
    # assign `office` values based on candidate
    temp$office <- ifelse(temp$candidate == "Hillary Clinton" | 
                            temp$candidate == "Donald J. Trump" |
                            temp$candidate == "Jill Stein" |
                            temp$candidate == "Gary Johnson", 
                          "President",
                          ifelse(temp$candidate == "Charles E. Schumer" |
                                   temp$candidate == "Wendy Long" |
                                   temp$candidate == "Robin Laverne Wilson" |
                                   temp$candidate == "Alex Merced", 
                                 "U.S. Senate",
                                 ifelse(temp$candidate == "Mike Derrick" |
                                          temp$candidate == "Elise M. Stefanik" |
                                          temp$candidate ==  "Matthew J. Funiciello", 
                                        "U.S. House", 
                                        ifelse(temp$candidate == "Elizabeth O'C Little" |
                                                 temp$candidate == "Stephen Matthew Ruzbacki", 
                                               "State Senate",
                                               ifelse(temp$candidate == "Daniel G Stec" |
                                                        temp$candidate == "Robin M. Barkenhagen", 
                                                      "State House", "")))))
    
    temp$district <- ifelse(temp$office == "U.S. House", "21", 
                            ifelse(temp$office == "State Senate", "45", 
                                   ifelse(temp$office == "State House", "114", "")))
    
    temp$county <- "Essex"
    essex <- rbind(essex, temp)
  }
)

#     user  system elapsed 
#    0.085   0.002   0.087

# reorder columns
essex <- essex[, c("county", "precinct", "office", "district", 
                   "party", "candidate", "votes")]

# convert strings to factors
library(dplyr)
essex <- essex %>% mutate_if(is.character, as.factor)

write.csv(essex, file = "~/GitHub/openelections-cleansing-scripts/20161108__ny__general__essex__precinct.csv", 
          row.names = F, na = "")

test <- read.csv("~/GitHub/openelections-cleansing-scripts/20161108__ny__general__essex__precinct.csv")
