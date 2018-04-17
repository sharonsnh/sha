# By Sharon Singh


# ----Set working directory and load dataset
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

# split month , day , year

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



# has error-- Error: Insufficient values in manual scale. 382 needed but only 3 provided.

ggplot(request_by_year, aes(year, Total, fill = SRType)) + 
  geom_bar(stat = "identity") + 
  labs(title="Graffiti Removal", fill="Level") + theme_bw() +  scale_fill_manual(values = c("#66C2A5" ,"#3288BD", "#5E4FA2")) + scale_fill_viridis(discrete = TRUE)
glimpse(month_day)

# count of SRType
 request_by_year <- graffiti %>%
   group_by(year, SRType) %>%
   dplyr::summarize(Total = n())
 request_by_year

# remove Rat Rubout and bulk schedule
graffiti1<- request_by_year[ grep("SW-Rat Rubout", request_by_year$SRType, invert = TRUE) , ]
graffiti1 <- request_by_year[ grep("SSW-Bulk-Scheduled", request_by_year$SRType, invert = TRUE) , ]
graffiti1 <-request_by_year[ grep("SW-Bulk-Scheduled", request_by_year$SRType, invert = TRUE) , ]
View(graffiti1)

# Remove rat rubout 2nd try-- ERROR
graffiti1 <- request_by_year[request_by_year$SRType != "SW-Rat Rubout-Proactive", ]
View(graffiti1)


# filter data with selected SRType
graffiti2<- graffiti1%>%select(year, SRType, Total) %>% filter(SRType %in% c("HCD-Sanitation Property","TRS-Parking Complaints","SW-Dirty Alley",
                                                                             "SW-HGW","TRM-Snow/Icy Conditions","ECC-Vehicle Look Up",
                                                                             "SW-Dirty Street - Proactive","SW-Dirty Alley","TRS-Parking Complaints",
                                                                             "SW-Dirty Street","SW-Proactive Mowing Schedule","SW-HGW","TRS-Abandoned Vehicle","BGE-StLight(s) Out",
                                                                             "HCD-Vacant Building","TRM-Illegal Sign Removal","HCD-Illegal Dumping"))
View(graffiti2)
## OR ##
listsrtype <-c("HCD-Sanitation Property","TRS-Parking Complaints","SW-Dirty Alley",
               "SW-HGW","TRM-Snow/Icy Conditions","ECC-Vehicle Look Up",
               "SW-Dirty Street - Proactive","SW-Dirty Alley","TRS-Parking Complaints",
               "SW-Dirty Street","SW-Proactive Mowing Schedule","SW-HGW","TRS-Abandoned Vehicle","BGE-StLight(s) Out",
               "HCD-Vacant Building","TRM-Illegal Sign Removal","HCD-Illegal Dumping")

graffiti2<- graffiti1%>%select(year, SRType, Total) %>% filter(SRType %in% listsrtype)
View(graffiti2)

