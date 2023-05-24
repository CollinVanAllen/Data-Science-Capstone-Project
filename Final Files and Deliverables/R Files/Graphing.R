library(tidyverse)
library(lubridate)

dataset <- read_csv("Combined Files/municipFinal.csv")
wisconsinData <- read_csv("Combined Files/municip.csv")
dataset

municipality <- 'Fennimore'
municipalityType <- "c"
county <- 'grant'

municipality <- toupper(municipality)
municipalityType <- toupper(municipalityType)
county <- toupper(county)

municipalityTypeFull <- case_when(
  municipalityType == "C" ~ "City",
  municipalityType == "V" ~ "Village",
  .default = "Township")

Label = paste(municipality, "-", municipalityType)

##
##
##

municipSub %>%
  filter(`Muni Type Code` == munitype & Municipality == muni) %>%
  ggplot(aes(x = Year)) +
  geom_line(aes(y = `Total Revenue Updated`, color = "Total Revenue Updated")) +
  geom_point(aes(y = `Total Revenue Updated`, color = "Total Revenue Updated", size = 1.5)) +
  geom_line(aes(y = `Total Expenses Updated`, color = "Total Expenses Updated")) +
  geom_point(aes(y = `Total Expenses Updated`, color = "Total Expenses Updated", size = 1.5)) +
  scale_y_continuous(labels=scales::dollar_format()) +
  scale_color_manual(values = c("blue", "red"), 
                     labels = c("Revenue", "Expenses")) +
  labs(y = "Dollars ($)", title = paste("Revenue and Expenditures -", muni, munitype)) + 
  guides(size = "none") +
  theme_bw()


municipSub %>%
  filter(`County Name` == county & Municipality == muni & `Muni Type Code` == munitype) %>% 
  ggplot(aes(x = Year)) + 
  # geom_line(aes(y = `Total Revenue Updated`), color = "blue") +
  # geom_point(aes(y = `Total Revenue Updated`), color = "blue") +
  geom_line(aes(y = Population)) + 
  geom_point(aes(y = Population)) +
  # scale_y_continuous(labels=scales::dollar_format()) +
  labs(y = "Population", title = paste("Population -", muni, "-", munitype)) + theme_bw()

# levels(municipSub$`Muni Type Code`) <- c("T", "V", "C")
dataset %>% 
  ggplot(aes(x = as.factor(year(Year)), group = Fail, fill = Fail)) +
  geom_bar(position = "dodge", color = "Black") +
  theme_bw() + coord_flip() +
  stat_count(geom = "text", colour = "black", size = 3.5,
             aes(label = ..count..), position = position_dodge(width = 1.0),
             hjust = 1.15) +
  labs(title = "Fail Counts", y = "Count", x = "Year")


# municipSub %>% 
#   ggplot(aes(x = Fail)) +
#   geom_bar(position = "dodge", color = "Black") +
#   facet_wrap(~Year) + theme_bw()
# 
# 
# municipSub %>% 
#   ggplot(aes(x = `Total Revenue Updated`, y = `Total Expenses Updated`)) + 
#   geom_point() +
#   geom_smooth(method = "lm") +
#   facet_wrap(~`Year`) +
#   scale_y_continuous(labels=scales::dollar_format()) +
#   scale_x_continuous(labels=scales::dollar_format()) +
#   labs(y = "Expenses", x = "Revenues", title = "Revenues vs. Expenses") + theme_bw()

#---------------------------------------------------------------------------
# Revenue and Expenses graphs per municip, just change var for muni and county name
# --------------------------------------------------------------------------

muni <- 'YUBA'
munitype <- "V"
county <- 'RICHLAND'

municipFilter <- municipSub %>% filter(`County Name` == county)

# Revenues ------------------------------ 

municipRevenue <- municipFilter[,c(3:5,7,10, 12, 14:15, 21, 23, 24 , 55, 59, 61)]
municipLong <- municipRevenue %>% 
  pivot_longer(cols = -c("Muni Type Code", "County Name", "Municipality", "Year", "vulnerability"))
municipLong <- as_tibble(municipLong)
municipLong$Year <- as.Date(municipLong$Year)

municipLong %>% filter(Municipality == muni & `Muni Type Code` == munitype) %>% 
  ggplot(aes(x = Year)) +
  geom_point(aes(y = value, color = vulnerability, size = 1.5)) + 
  geom_line(aes(y = value)) +
  facet_wrap(~name) +
  scale_y_continuous(labels=scales::dollar_format()) +
  labs(title = paste("Revenue --",muni,munitype)) +
  guides(size = "none") +
  theme_bw()

# Expenses ------------------------

municipExpenses <- municipFilter[,c(3:5, 29, 34, 38:39, 41, 55, 58, 61)]
municipLongE <- municipExpenses %>% 
  pivot_longer(cols = -c("Muni Type Code", "County Name", "Municipality", "Year", "vulnerability"))
municipLongE <- as_tibble(municipLongE)
municipLongE$Year <- as.Date(municipLongE$Year)

municipLongE %>% filter(Municipality == muni & `Muni Type Code` == munitype) %>%  
  ggplot(aes(x = Year)) +
  geom_point(aes(y = value, color = vulnerability, size = 1.5)) + 
  geom_line(aes(y = value)) +
  facet_wrap(~name) +
  scale_y_continuous(labels=scales::dollar_format()) +
  labs(title = paste("Expenses --",muni,munitype)) +
  guides(size = "none") +
  theme_bw()


#--------------------------------------------------------------------



  

# municipSub %>% 
#   filter(`County Name` == "GRANT" & Municipality == "CASSVILLE" & `Muni Type Code` == "V") %>% 
#   ggplot(aes(x = Year)) + 
#   geom_line(aes(y = `Total Revenue & Other Financing Sources`), color = "blue") +
#   geom_point(aes(y = `Total Revenue & Other Financing Sources`), color = "blue") +
#   geom_line(aes(y = `Total Expenditures & Other Financing Uses`), color = "red") + 
#   geom_point(aes(y = `Total Expenditures & Other Financing Uses`), color = "red") + 
#   facet_wrap(~Municipality, scales = "free_y") +
#   scale_y_continuous(labels=scales::dollar_format()) +
#   labs(y = "Dollars ($)", title = "Revenue and Expenditures") + theme_bw()

#-------------------------------------------------------------------------------
# Fail Graphs
#-------------------------------------------------------------------------------

vulnCounts <- dataset %>%
  group_by(Municipality, `Muni Type Code`) %>% 
  summarise(counts = sum(vulnerability == "Fail", na.rm = TRUE))

top3_each <- vulnCounts %>%
  group_by(`Muni Type Code`) %>%
  top_n(3, counts) %>%
  arrange(`Muni Type Code`, desc(counts)) %>%
  slice(1:3)

top3_each

#count
vulnCountsYear %>% 
  ggplot(aes(x = as.factor(Years_Vuln), fill = `Muni Type Code`)) + 
  geom_bar(color = "black", position = position_dodge()) +
  stat_count(geom = "text", colour = "black", size = 3.5,
             aes(label = ..count..),position=position_dodge(width = 1,preserve = "total"),
             vjust = -0.25) +
  labs(title = "Count of Years Vulnerable", x = "Number of Years of Vulnerability", y = "Count") +
  theme_bw()


#percent
# dataset %>%
#   ggplot(aes(x = as.factor(Years_Vuln))) +
#   geom_bar(fill = "turquoise", color = "black") +
#   stat_count(geom = "text", colour = "black", size = 3.5,
#              aes(label = scales::percent(after_stat(count/sum(count)))),
#              position=position_dodge(width = 1,preserve = "total"),
#              vjust = -0.25) +
#   labs(title = "Count of Critical Fails", x = "Number of Fails", y = "Percentage") +
#   theme_bw()


vulnCountsYear <- dataset %>% filter(Year != "2013-01-01")
vulnCountsYear <- dataset %>% filter(vulnerability == "Fail")

#count
dataset %>%
  summarise(counts = sum(vulnerability == "Fail")) %>% 
  ggplot(aes(x = counts)) + 
  geom_bar(color = "black", fill = "turquoise", position = position_dodge()) +
  stat_count(geom = "text", colour = "black", size = 3.5,
             aes(label = ..count..),position=position_dodge(width = 1,preserve = "total"),
             vjust = -0.25) +
  labs(title = "Vulnerability Counts per Year", x = "Year", y = "Count") +
  theme_bw()

dataset %>%
  group_by(Municipality, `Muni Type Code`) %>% 
  summarise(counts = sum(vulnerability == "Fail", na.rm = TRUE)) %>% 
  ggplot(aes(x = counts)) + 
  geom_bar(color = "black", fill = "turquoise", position = position_dodge()) +
  stat_count(geom = "text", colour = "black", size = 3.5,
             aes(label = ..count..),position=position_dodge(width = 1,preserve = "total"),
             vjust = -0.25) +
  labs(title = "Sum of Vulnerability Counts Over All Years", x = "Years Vulnerable", y = "Municipality Count") +
  theme_bw()






# #percent split
# failCountsYear %>%
#   group_by(year = as.factor(year(Year)), `Type Code`) %>%
#   summarize(count = n()) %>%
#   mutate(percent = count / sum(count) * 100) %>%
#   ggplot(aes(x = year, y = percent, fill = `Type Code`)) + 
#   geom_bar(stat = "identity", color = "black") +
#   geom_text(aes(label = paste0(round(percent, 0), "%")), 
#             position = position_stack(vjust = 0.5), 
#             size = 3.5) +
#   labs(title = "Percentage of Critical Fails", x = "Year", y = "Percentage") +
#   theme_bw() +
#   facet_grid(~ `Type Code`)

#number split
vulnCountsYear %>%
  group_by(year = as.factor(year(Year)), `Muni Type Code`) %>%
  summarize(count = n()) %>%
  ggplot(aes(x = year, y = count, fill = `Muni Type Code`)) + 
  geom_bar(stat = "identity", color = "black") +
  geom_text(aes(label = format(count, big.mark = ",")), 
            position = position_stack(vjust = 0.5), 
            size = 3.5) +
  labs(title = "Count of Vulnerable Municipalities by Year", x = "Year", y = "Count") +
  theme_bw() +
  facet_grid(~ `Muni Type Code`)



# #percent
# vulnCountsYear %>%
#   group_by(year = as.factor(year(Year))) %>%
#   summarize(count = n()) %>%
#   mutate(percent = count/sum(count)*100) %>%
#   ggplot(aes(x = year, y = percent)) + 
#   geom_bar(fill = Years_Vuln, color = "black", stat = "identity") +
#   geom_text(aes(label = paste0(round(percent, 0), "%")), 
#             position = position_stack(vjust = 0.5), 
#             size = 3.5) +
#   labs(title = "Percentage of Critical Fails", x = "Year", y = "Percentage") +
#   theme_bw()
# 
# failCounts %>% 
#   ggplot(aes(x = as.factor(Fail_Count), fill = `Type Code`)) + 
#   geom_bar(position = "dodge", color = "black") + # add position parameter to set bars side-by-side
#   stat_count(geom = "text", colour = "black", size = 3.5,
#              aes(label = ..count..),position=position_dodge(width = 1,preserve = "total"),
#              vjust = -0.5) + 
#   labs(title = "Count of Critical Fails", x = "Number of Fails", y = "Count") +
#   theme_bw()


