library(dplyr)
library(ggplot2)
library(lubridate)
library(scales)

library(rstudioapi) 
setwd(dirname(getActiveDocumentContext()$path))
source("config.R")

#####R Base#####
dateBase = as.POSIXct("02-20-2018", format = "%m-%d-%Y", tz = "UTC")

#####Example1: Lakers#####
#library(lubridate)

lakers %>%
  mutate(date = ymd(date)) %>%
  ggplot(aes(date, 0, color = game_type)) +
    geom_point() +
    ggtitle("Home games and Away Games")

#####Manipulating Dates and Times#####
date <- now()
update(date, year = 2010, month = 1, day = 1,tz="America/Chicago")

#####Example2: Thanksgiving date in 2018 & 2019#####
getThanksgivingDate = 
  function(year) {
    year_start = mdy(paste("01-01-", year, sep = ""))
    november_start = year_start + months(10)
    wday_nov_start = wday(november_start)
    nov_start_thu = november_start
    if(wday_nov_start < 5) {
      nov_start_thu = november_start + days(5 - wday_nov_start)
    } else if (wday_nov_start > 5) {
      nov_start_thu = november_start + days(5 + (7 - wday_nov_start))
    }
    return(nov_start_thu + weeks(3))
  }
getThanksgivingDate(2018)
getThanksgivingDate(2019)

#####Example3: Visualizing Los Angeles Collision Data#####
setDataObjPath()
load("collisiondata.rda")

collision_pedestrian = 
  collision %>% mutate(timestamp = dmy_hms(paste(Collision.date, Collision.Time, sep = " "))) %>%
  filter(Involved.With == "Pedestrian")

ggplot(collision_pedestrian, aes(wday(timestamp, label = TRUE))) +
  geom_bar() +
  labs(
    x = "",
    title = "Number of Collisions that Involved Pedestrians"
  )

ggplot(collision_pedestrian, aes(month(timestamp, label = TRUE))) +
  geom_bar() +
  labs(
    x = "",
    title = "Number of Collisions that Involved Pedestrians"
  )

collision_pedestrian %>%
  group_by(wday = wday(timestamp, label = TRUE, abbr = FALSE), 
           hour = hour(timestamp)) %>%
  summarise(count = n()) %>%
  ggplot(aes(wday, as.factor(hour), fill = count)) +
    geom_tile() +
    scale_fill_gradient(low = "white", high = "darkred") +
    labs(
      x = "Day of week",
      y = "Hour of day",
      title = "Heatmap of Collisions in Los Angeles"
    )

#####Example4: economics in ggplot2 (scales)#####
ggplot(economics, aes(date, unemploy)) + 
  geom_line() +
  labs(
    x = "Date",
    y = "Number of Unemployed (Thousands)"
  )

economics %>%
  filter(date %within% interval(ymd("1992-05-01"), ymd("1993-06-01"))) %>%
  ggplot(aes(date, unemploy)) + 
    geom_line() +
    labs(
      x = "Date",
      y = "Number of Unemployed (Thousands)"
    )

#library(scales)
economics %>%
  ggplot(aes(date, unemploy)) + 
  geom_line() +
  scale_x_date(date_breaks = "10 years", labels = date_format("%Y %b")) +
  labs(
    x = "Date",
    y = "Number of Unemployed (Thousands)"
  ) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

#####Example5: Visulazing Stocks#####
setRawDataPath()
apple <- read.csv("AAPL.csv")
google <- read.csv("GOOG.csv")
amazon <- read.csv("AMZN.csv")

apple$Date <- ymd(apple$Date) 
google$Date <- ymd(google$Date) 
amazon$Date <- ymd(amazon$Date)
apple$Adj.Close = as.numeric(apple$Adj.Close)

apple = apple %>% filter(Date >= dmy("1-1-2005")) 
google = google %>% filter(Date >= dmy("1-1-2005")) 
amazon = amazon %>% filter(Date >= dmy("1-1-2005"))

ggplot(apple, aes(Date, Adj.Close)) +
  geom_line(color =  "red") +
  geom_line(data = google, aes(Date, Adj.Close)) +
  geom_line(data = amazon, aes(Date, Adj.Close), color = "blue")
