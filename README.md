# sha
#Splitting data in dates (mdy)
setwd("C:/Users/admin/Desktop/MACHINE LEARNING/PROJECT - 602")
getwd()
graffiti <- read.csv("GraffitiRemoval.csv")
View(graffiti)
install.packages("lubridate")
library(lubridate)
install.packages("dplyr")
library(dplyr)
(library(readr))
library(ggplot2)
graffiti$DueDate <- parse_date_time(graffiti$DueDate, orders = c("mdy", "dmy"))
graffiti$DueDate <- as.Date(graffiti$DueDate, format = "%m/%d/%y")

#split month , day , year

graffiti$day <- factor(day(graffiti$DueDate))
graffiti$month <- factor(month(graffiti$DueDate))
graffiti$year <- factor(year(graffiti$DueDate))
graffiti$dayofweek <- factor(wday(graffiti$DueDate))
month_day <- graffiti[graffiti$year == "2015", c("month", "day")] %>% group_by(month, day) %>% summarise(Count = n()) 
month_day <- as.data.frame(month_day)
View(graffiti)
data.frame(graffiti)
srtype = unique(graffiti$SRType)
length(srtype)
srtype
filter(graffiti, SRType$)
graffiti_removal <- graffiti %>% group_by(year, SRType)


request_by_year <- graffiti %>%
  group_by(year, SRType) %>%
  dplyr::summarize(Total = n()) 

ggplot(request_by_year, aes(year, Total, fill = SRType)) + 
  geom_bar(stat = "identity") + 
  labs(title="Graffiti Removal", fill="Level") + theme_bw() +  scale_fill_manual(values = c("#66C2A5" ,"#3288BD", "#5E4FA2")) + scale_fill_viridis(discrete = TRUE)
glimpse(month_day)







