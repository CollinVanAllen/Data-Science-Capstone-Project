library(janitor)
library(reshape)
library(reshape2)
library(lubridate)
library(tidyverse)

municip <- read_csv("Combined Files/Combined_Municipalities.csv")
PSW_Aid <- read_csv("Other Sheets/PSW Aid.csv")
# Only use for year 2021, value is halved for aid split between 21/22
PSW_Aid <- na.omit(PSW_Aid)
PSW_Aid$Year <- "2021-01-01"
PSW_Aid$Year <- ymd(PSW_Aid$Year)
municip$Year <- dmy(municip$Year)

municip <- left_join(x = municip, y = PSW_Aid)
municip$`PSW Aid`[is.na(municip$`PSW Aid`)] <- 0
municip$`PSW Halved`[is.na(municip$`PSW Halved`)] <- 0

municip <- municip %>% mutate(`Total Expenses Updated` = 
                                `Total Expenditures & Other Financing Uses` - (`Total Debt Service` + `Highway Construction`))
municip <- municip %>% mutate(`Total Revenue Updated` = `Total Revenue & Other Financing Sources` - 
                                `Other Financing Sources` - `PSW Halved` - `Federal Aids`)

municip <- municip %>% mutate(Fail = case_when(`Total Expenses Updated` > `Total Revenue Updated` ~ "Fail", TRUE ~ "No Fail"))
municipSub <- municip %>% filter(municip$`County Name` %in% c("GRANT", "IOWA", "LAFAYETTE", "RICHLAND", "GREEN"))
municipRevenue <- municipSub[,c(1:45,53:55,57:ncol(municipSub))]

# municipSub <- municipSub %>% mutate(Label = paste(Municipality, "-", `Muni Type Code`))
