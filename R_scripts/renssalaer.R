# OpenElectionsData-NY
# https://github.com/openelections/openelections-data-ny

# Rennssalaer County, NY ------------------------------------------------------

# Presidential Election
# US Senate 
# US House of Representatives (19th & 20th Congressional Districts)
# NY State Senate (43rd & 44th Senatorial Districts)
# NY State Assembly (107th & 108th Assembly Districts)

# laod libraries --------------------------------------------------------------

library(dplyr)
library(tidyr)

# Presidential results --------------------------------------------------------

pres <- read.csv("data/Rensselaer/Rensselaer-Pres.csv", stringsAsFactors = F)

# munge party, candidate 
pres[1, ] <- paste0(colnames(pres), ".", pres[1, ])
colnames(pres) <- pres[1, ]
pres <- pres[-1, ]

# set colnames to be spread 
colnames(pres) <- c("X", "DEM.Hillary Clinton", "REP.Donald J Trump", "CON.Donald J Trump",
                    "GRE.Jill Stein", "WOR.Hillary Clinton", "IND.Gary Johnson",
                    "WEP.Hillary Clinton", "LIB.Gary Johnson", 
                    "x.Write-In", "x.Blank", "x.Void")

# list of colnames to be spread
pList <- colnames(pres)[2:12]

# wide to long format
presidential <- reshape(pres, varying = pList,
              v.names = "votes", 
              timevar = "party", 
              times = pList,
              direction = "long", 
              new.row.names = 1:1428)


presidential$id <- NULL
colnames(presidential)[1] <- "precinct"

# separate colnames into fields
presidential <- presidential %>%
  separate(party, into = c("party", "candidate"), sep = "\\.")

# fix candidate name
presidential$candidate <- gsub("Donald J Trump", "Donald J. Trump", presidential$candidate)
levels(as.factor(presidential$candidate))

# remove 'x' party
presidential$party <- gsub("x", "", presidential$party)
levels(as.factor(presidential$party))
    
# add variables                       
presidential$county <- "Rensselaer"
presidential$office <- "President"
presidential$district <- ""

# reorder & rename columns
presidential <- presidential[, c(5, 1, 7, 6, 2, 3, 4)]
# colnames(presidential) <- c("county", "precinct", "office", "district", 
#                             "party", "candidate", "votes")

# bind to other results later
# write.csv(presidential, file = "data/Rensselaer/00-pres.csv", row.names = F)

# US Senate results -----------------------------------------------------------

sen <- read.csv("data/Rensselaer/Rensselaer-US_Senator.csv", stringsAsFactors = F)

# given two variables that need to be spread,
# combine them in the column name
# and then spread them after coverting data to long format.
sen[1, ] <- paste0(colnames(sen), "-", sen[1, ])
colnames(sen) <- sen[1, ]
sen <- sen[-1, ]

colnames(sen)[11:13] <- c("x-Write_In", "x-Blank", "x-Void") 
colnames(sen)[1] <- "X"

# list of names to spread
sList <- colnames(sen)[2:13]

# wide to long conversion
senate <- reshape(sen, varying = sList,
                  v.names = "votes", 
                  timevar = "party", 
                  times = sList,
                  direction = "long", 
                  new.row.names = 1:2000)

# remove added id variable
senate$id <- NULL
colnames(senate)[1] <- "precinct"

# change separator character
# senate$party <- sub("\\.", "\\-", senate$party)

senate <- senate %>%
  separate(party, into = c("party", "candidate"), sep = "\\-")

# remove carriage returns and extra blank space
senate$candidate <- gsub("[\r\n]", " ", senate$candidate)
senate$candidate <- gsub("\\s\\s", " ", senate$candidate)
senate$party <- gsub("x", "", senate$party)

levels(as.factor(senate$party))
levels(as.factor(senate$candidate))

# add variables                       
senate$county <- "Rensselaer"
senate$office <- "U.S. Senate"
senate$district <- ""

# reorder and rename columns
senate <- senate[, c(5, 1, 6, 7, 2, 3, 4)]
# colnames(senate) <- c("county", "precinct", "office", "district", 
#                             "party", "candidate", "votes")

write.csv(senate, file = "data/01-US-Senate.csv", row.names = F)
rm(sen)

# US House - 19th District ----------------------------------------------------

us19th <- read.csv("data/Rensselaer/Rensselaer-Congress19th.csv", stringsAsFactors = F)

us19th[1, ] <- paste0(colnames(us19th), ".", us19th[1, ])
colnames(us19th) <- us19th[1, ]
us19th <- us19th[-1, ]

colnames(us19th)[8:10] <- c("x.Write_In", "x.Blank", "x.Void") 
colnames(us19th)[1] <- "X"

# list of names to spread
cList <- colnames(us19th)[2:10]

# wide to long conversion
usHouse.19th <- reshape(us19th, varying = cList,
                  v.names = "votes", 
                  timevar = "party", 
                  times = cList,
                  direction = "long", 
                  new.row.names = 1:2000)

usHouse.19th$id <- NULL
colnames(usHouse.19th)[1] <- "precinct"


# change separator character
usHouse.19th$party <- sub("\\.", "\\-", usHouse.19th$party)

usHouse.19th <- usHouse.19th %>%
  separate(party, into = c("party", "candidate"), sep = "\\-")

# remove carriage returns and extra blank space
usHouse.19th$candidate <- gsub("[\r\n]", " ", usHouse.19th$candidate)
usHouse.19th$candidate <- gsub("\\s\\s", " ", usHouse.19th$candidate)
usHouse.19th$party <- gsub("x", "", usHouse.19th$party)

levels(as.factor(usHouse.19th$party))
levels(as.factor(usHouse.19th$candidate))

# add variables                       
usHouse.19th$county <- "Rensselaer"
usHouse.19th$office <- "U.S. House "
usHouse.19th$district <- "19"

# reorder and rename columns
usHouse.19th <- usHouse.19th[, c(5, 1, 6, 7, 2, 3, 4)]

# write.csv(usHouse.19th, file = "data/02-US-House-19th.csv", row.names = F)
rm(us19th)

# US House 20th ---------------------------------------------------------------

us20 <- read.csv("data/Rensselaer/Rensselaer-Congress20th.csv", stringsAsFactors = F)
us20[1, ] <- paste0(colnames(us20), ".", us20[1, ])
colnames(us20) <- us20[1, ]
us20 <- us20[-1, ]

colnames(us20)[9:11] <- c("x.Write_In", "x.Blank", "x.Void")
colnames(us20)[1] <- "X"

# list of names to spread
cList <- colnames(us20)[2:11]

# wide to long conversion
us20th <- reshape(us20, varying = cList,
                        v.names = "votes", 
                        timevar = "party", 
                        times = cList,
                        direction = "long", 
                        new.row.names = 1:2000)

us20th$id <- NULL
colnames(us20th)[1] <- "precinct"

# change separator character
us20th$party <- sub("\\.", "\\-", us20th$party)

us20th <- us20th %>%
  separate(party, into = c("party", "candidate"), sep = "\\-")

# remove carriage returns and extra blank space
us20th$candidate <- gsub("[\r\n]", " ", us20th$candidate)
# us20th$candidate <- gsub("\\s\\s", " ", us20th$candidate)
us20th$party <- gsub("x", "", us20th$party)

levels(as.factor(us20th$party))
levels(as.factor(us20th$candidate))

# add variables                       
us20th$county <- "Rensselaer"
us20th$office <- "U.S. House "
us20th$district <- "20"

# reorder and rename columns
us20th <- us20th[, c(5, 1, 6, 7, 2, 3, 4)]

write.csv(us20th, file = "data/03-US-House-20th.csv", row.names = F)

# State Senate 43rd -----------------------------------------------------------

ss43 <- read.csv("data/Rensselaer/Rensselaer-43rd.csv", stringsAsFactors = F)
ss43[1, ] <- paste0(colnames(ss43), "-", ss43[1, ])
colnames(ss43) <- ss43[1, ]
ss43 <- ss43[-1, ]

colnames(ss43)[9:11] <- c("x-Write_In", "x-Blank", "x-Void")
colnames(ss43)[1] <- "X"

# list of names to spread
ssList <- colnames(ss43)[2:11]

# wide to long conversion
ss43th <- reshape(ss43, varying = ssList,
                  v.names = "votes", 
                  timevar = "party", 
                  times = ssList,
                  direction = "long", 
                  new.row.names = 1:2000)

ss43th$id <- NULL
colnames(ss43th)[1] <- "precinct"

# change separator character
# ss43th$party <- sub("\\.", "\\-", ss43th$party)

ss43th <- ss43th %>%
  separate(party, into = c("party", "candidate"), sep = "\\-")

# remove carriage returns and extra blank space
ss43th$candidate <- gsub("[\r\n]", " ", ss43th$candidate)
# ss43th$candidate <- gsub("\\s\\s", " ", ss43th$candidate)
ss43th$party <- gsub("x", "", ss43th$party)

levels(as.factor(ss43th$party))
levels(as.factor(ss43th$candidate))

# add variables                       
ss43th$county <- "Rensselaer"
ss43th$office <- "State Senate"
ss43th$district <- "43"

# reorder and rename columns
ss43th <- ss43th[, c(5, 1, 6, 7, 2, 3, 4)]

write.csv(ss43th, file = "data/04-StateSenate-43rd.csv", row.names = F)

# State Senate 44th -----------------------------------------------------------

ss44 <- read.csv("data/Rensselaer/Rensselaer-44th.csv", stringsAsFactors = F)
ss44[1, ] <- paste0(colnames(ss44), "-", ss44[1, ])
colnames(ss44) <- ss44[1, ]
ss44 <- ss44[-1, ]

colnames(ss44)[9:11] <- c("x-Write_In", "x-Blank", "x-Void")
colnames(ss44)[1] <- "X"

# list of names to spread
ssList <- colnames(ss44)[2:11]

# wide to long conversion
ss44th <- reshape(ss44, varying = ssList,
                  v.names = "votes", 
                  timevar = "party", 
                  times = ssList,
                  direction = "long", 
                  new.row.names = 1:2000)

ss44th$id <- NULL
colnames(ss44th)[1] <- "precinct"

# change separator character
# ss44th$party <- sub("\\.", "\\-", ss44th$party)

ss44th <- ss44th %>%
  separate(party, into = c("party", "candidate"), sep = "\\-")

# remove carriage returns and extra blank space
ss44th$candidate <- gsub("[\r\n]", " ", ss44th$candidate)
ss44th$candidate <- gsub("\\s\\s", " ", ss44th$candidate)
ss44th$party <- gsub("x", "", ss44th$party)

levels(as.factor(ss44th$party))
levels(as.factor(ss44th$candidate))

# add variables                       
ss44th$county <- "Rensselaer"
ss44th$office <- "State Senate"
ss44th$district <- "44"

# reorder and rename columns
ss44th <- ss44th[, c(5, 1, 6, 7, 2, 3, 4)]

write.csv(ss44th, file = "data/05-StateSenate-44th.csv", row.names = F)

# State House 107th -----------------------------------------------------------

sh107 <- read.csv("data/Rensselaer/Rensselaer-107th.csv", stringsAsFactors = F)
sh107[1, ] <- paste0(colnames(sh107), "-", sh107[1, ])
colnames(sh107) <- sh107[1, ]
sh107 <- sh107[-1, ]

colnames(sh107)[6:8] <- c("x-Write_In", "x-Blank", "x-Void")
colnames(sh107)[1] <- "X"

# list of names to spread
shList <- colnames(sh107)[2:8]

# wide to long conversion
sh107th <- reshape(sh107, varying = shList,
                  v.names = "votes", 
                  timevar = "party", 
                  times = shList,
                  direction = "long", 
                  new.row.names = 1:2000)

sh107th$id <- NULL
colnames(sh107th)[1] <- "precinct"

# change separator character
# sh107th$party <- sub("\\.", "\\-", sh107th$party)

sh107th <- sh107th %>%
  separate(party, into = c("party", "candidate"), sep = "\\-")

# remove carriage returns and extra blank space
sh107th$candidate <- gsub("[\r\n]", " ", sh107th$candidate)
sh107th$candidate <- gsub("\\s\\s", " ", sh107th$candidate)
sh107th$party <- gsub("x", "", sh107th$party)

levels(as.factor(sh107th$party))
levels(as.factor(sh107th$candidate))

# add variables                       
sh107th$county <- "Rensselaer"
sh107th$office <- "State House"
sh107th$district <- "107"

# reorder and rename columns
sh107th <- sh107th[, c(5, 1, 6, 7, 2, 3, 4)]

# write.csv(sh107th, file = "data/06-StateHouse-107th.csv", row.names = F)

# State House 108th -----------------------------------------------------------

sh108 <- read.csv("data/Rensselaer/Rensselaer-108th.csv", stringsAsFactors = F)
sh108[1, ] <- paste0(colnames(sh108), "-", sh108[1, ])
colnames(sh108) <- sh108[1, ]
sh108 <- sh108[-1, ]

colnames(sh108)[4:6] <- c("x-Write_In", "x-Blank", "x-Void")
colnames(sh108)[1] <- "X"

# list of names to spread
shList <- colnames(sh108)[2:6]

# wide to long conversion
sh108th <- reshape(sh108, varying = shList,
                   v.names = "votes", 
                   timevar = "party", 
                   times = shList,
                   direction = "long", 
                   new.row.names = 1:2000)

sh108th$id <- NULL
colnames(sh108th)[1] <- "precinct"

# change separator character
# sh108th$party <- sub("\\.", "\\-", sh108th$party)

sh108th <- sh108th %>%
  separate(party, into = c("party", "candidate"), sep = "\\-")

# remove carriage returns and extra blank space
sh108th$candidate <- gsub("[\r\n]", " ", sh108th$candidate)
sh108th$candidate <- gsub("\\s\\s", " ", sh108th$candidate)
sh108th$party <- gsub("x", "", sh108th$party)

levels(as.factor(sh108th$party))
levels(as.factor(sh108th$candidate))

# add variables                       
sh108th$county <- "Rensselaer"
sh108th$office <- "State House"
sh108th$district <- "108"

# reorder and rename columns
sh108th <- sh108th[, c(5, 1, 6, 7, 2, 3, 4)]

# write.csv(sh108th, file = "data/07-StateHouse-108th.csv", row.names = F)


# Bind all data ---------------------------------------------------------------

pres <- read.csv("data/Rensselaer/00-pres.csv", stringsAsFactors = F)
us19th <- usHouse.19th

rensselaer <- rbind(presidential, senate, us19th, us20th, ss43th, ss44th, sh107th, sh108th)
levels(as.factor(rensselaer$candidate))

rensselaer$candidate <- gsub("Write_In", "Write-In", rensselaer$candidate)
rensselaer$candidate <- gsub("\\s\\s", " ", rensselaer$candidate)
rensselaer$office <- gsub("U.S. House ", "U.S. House", rensselaer$office)
  
levels(as.factor(rensselaer$candidate))
levels(as.factor(rensselaer$precinct))
levels(as.factor(rensselaer$office))
levels(as.factor(rensselaer$district))
levels(as.factor(rensselaer$party))

write.csv(rensselaer, file = "data/rensselaer.csv", row.names = F)
