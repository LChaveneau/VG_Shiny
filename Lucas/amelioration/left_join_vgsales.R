library(rjson)
library(jsonlite)

## Import de vgsales.csv manuellement


data_3<-read_json("vgsales2.json",simplifyVector = TRUE)

library(dplyr)

data_3 <- data_3 %>% mutate(Rank = as.numeric(Rank),
                  NA_Sales = as.numeric(NA_Sales),
                  EU_Sales = as.numeric(EU_Sales),
                  JP_Sales = as.numeric(JP_Sales),
                  Other_Sales = as.numeric(Other_Sales),
                  Global_Sales = as.numeric(Global_Sales))

data_3 <- data_3 %>% arrange(Rank)


data <- vgsales %>% left_join(data_3, by=c("Rank"="Rank", 
                                           "Name" = "Name", 
                                           "Platform" = "Platform", 
                                           "Year" = "Year", 
                                           "Genre" = "Genre",
                                           "Publisher" = "Publisher",
                                           "NA_Sales" = "NA_Sales",
                                           "EU_Sales" = "EU_Sales",
                                           "JP_Sales" = "JP_Sales",
                                           "Other_Sales" = "Other_Sales",
                                           "Global_Sales" = "Global_Sales"
                                           ))
sum(data$Name == vgsales$Name)
sum(data$Year == vgsales$Year)
sum(data$Platform == vgsales$Platform)
sum(data$Genre == vgsales$Genre)
sum(data$Publisher == vgsales$Publisher)
sum(data$NA_Sales == vgsales$NA_Sales)
sum(data$EU_Sales == vgsales$EU_Sales)
sum(data$JP_Sales == vgsales$JP_Sales)
sum(data$Other_Sales == vgsales$Other_Sales)
sum(data$Global_Sales == vgsales$Global_Sales)


write.csv(data,"C:\\Users\\Lucas\\Documents\\M2\\S2\\Big data\\Lucas\\data\\vgsales2.csv", row.names = FALSE)
