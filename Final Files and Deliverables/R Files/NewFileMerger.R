library(tidyverse)
library(lubridate)
library(janitor)

# Read in county revenue file for 2015, remove total row, add year column
rev13 <- read_csv("2013/cmreb2013_villages.csv")
rev13 <- rev13[-nrow(rev13),]
rev13$Year <- as.Date("2013-01-01")
rev13$`County Code` <- as.numeric(as.character(rev13$`County Code`))
# rev13$`Muni Code` <- as.numeric(as.character(rev13$`Muni Code`))

rev14 <- read_csv("2014/cmreb2014_villages.csv", skip = 2)
rev14 <- rev14[-nrow(rev14),]
rev14$Year <- as.Date("2014-01-01")
rev14$`County Code` <- as.numeric(as.character(rev14$`County Code`))
# rev14$`Muni Code` <- as.numeric(as.character(rev14$`Muni Code`))

revComp <- read_csv("Combined Files/Combined_Villages.csv")

rev21 <- read_csv("2021/cmreb2021_villages.csv")
common <- names(rev21)[names(rev21) %in% names(revComp)]
rev21 <- apply(rev21, 2, function(x) gsub("\\$", "", x))
rev21 <- apply(rev21, 2, function(x) gsub(",", "", x))
rev21 <- as.data.frame(rev21)
rev21$Year <- as.Date("2021-01-01")
rev21[common] <- lapply(common, function(x) {
  match.fun(paste0("as.", class(revComp[[x]])))(rev21[[x]])
})
rev21$`County Code` <- as.numeric(as.character(rev21$`County Code`))

revMerge <- bind_rows(rev13,rev14,revComp,rev21)

write.csv(revMerge, "E:\\Capstone\\Combined Files\\Combined_Villages.csv", row.names=FALSE)
