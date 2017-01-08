# State House 114th results----------------------------------------------------
sh114dir <- "~/Documents/R2/00-openelections-data-ny/pd/data/Saratoga/State_House_114th"
setwd(sh114dir)

sh114_list <- list.files(sh114dir, pattern = ".csv", recursive = T, all.files = T)
sh114 <- data.frame()

for (i in 1:length(sh114_list)) {
  
  # read in data and remove "Whole Number" column
  temp10 <- read.csv(sh114_list[i], header = F)
  temp10$V2 <- NULL
  
  # transpose data; unite long candidate names
  temp10 <- as.data.frame(t(temp10))
  temp10 <- temp10 %>% unite(temp10, c(V2, V3), sep = " ")
  
  temp10 <- temp10 %>% mutate_if(is.factor, as.character)
  temp10[1, 1:2] <- c("party", "candidate")
  
  # set precinct numbers as column names; remove superfluous
  colnames(temp10) <- temp10[1, ]
  temp10 <- temp10[-1, ]
  
  # remove trailing whitespace
  temp10$candidate <- gsub("\\s$", "", temp10$candidate)
  
  # assign "Blank Votes" to blank `candidate` fields
  temp10$candidate[temp10$candidate == ""] <- "Blank Votes"
  
  # wide-to-long conversion; stack precinct values as single column and rename
  temp10 <- temp10 %>% gather(key = "precinct", value = "votes", 3:ncol(temp10))
  
  temp10$precinct <- paste(sh114_list[i], temp10$precinct)
  temp10$precinct <- gsub(".csv", "", temp10$precinct)
  temp10$precinct <- gsub("W:000\\sD:0", "", temp10$precinct)
  
  # remove 'Blank Votes' from extra columns
  temp10$precinct[temp10$precinct == "Blank Votes"] <- ""
  temp10$party[temp10$party == "Blank Votes"] <- ""
  
  # add columns for county, office, and district
  temp10$county <- "Saratoga"
  temp10$office <- "State House"
  temp10$district <- "114"
  
  temp10 <-temp10[, c(5, 3, 6, 7, 1, 2, 4)]
  
  sh114 <- rbind(sh114, temp10)
  
}
