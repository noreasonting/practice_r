library(dplyr)
library(ggplot2)
library(lubridate)

library(rstudioapi) 
setwd(dirname(getActiveDocumentContext()$path))
source("config.R")

setRawDataPath()

epl= read.csv("epl.csv")
epl %>%
  filter(year(Match_Date)>=2001 & year(Match_Date)<=2010 ) %>% 
  filter((HomeTeam=="Liverpool" & Home_Win==1) | (AwayTeam=="Liverpool" & Away_Win ==1)) %>%
  summarise(count = n())

epl %>%
  filter(Season =="2017-18") %>% 
  group_by(year = year(Match_Date), 
           month = month(Match_Date, label = T), 
           wday = wday(Match_Date, label = T)) %>%
  summarise(total_matches = n()) %>% 
  ggplot(aes(reorder(month, year), wday, fill = total_matches)) +
  geom_tile() +
  theme_bw() +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(title = "Matches played in 2017-18 season by Month and Day of Week",
       x = "Month of Season 2017-18",
       y = "Day of Week",
       fill = "Count")
