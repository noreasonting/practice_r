library(ggplot2)
library(rstudioapi)
setwd(dirname(getActiveDocumentContext()$path))
source("config.R")
setRawDataPath()
heavenlyChocolates = read.csv("HeavenlyChocolates.csv")

ggplot(heavenlyChocolates, aes(Browser)) +
  geom_bar() +
  labs(
    y = "Count",
    title = "Browsers Used to Visit Heavenly Chocolate Website"
  )

ggplot(heavenlyChocolates, aes(Time, Amount_Spent, color = Browser)) +
  geom_point() +
  labs(
    x = "Time Spent",
    y = "Amount Spent",
    title = "Relationship between Time Spent Browsing and Amount Spent"
  )

ggplot(heavenlyChocolates, aes(Day, fill = Browser)) +
  geom_bar() +
  labs(
    x = "",
    y = "Count",
    title = "Number of Website Visitors per Day per Browser"
  )

heavenlyChocolates$AmountCat = ifelse(heavenlyChocolates$Amount_Spent <= 62, "Low", "High")
ggplot(heavenlyChocolates, aes(Browser, fill = AmountCat)) +
  geom_bar() +
  facet_wrap(~Day) +
  labs(
    y = "Count",
    title = "Distribution of ‘AmountCat‘ across Different Days of the Week"
  )

amountColName = "Amount_Spent"
heavenlyChocolates$IsWeekend = ifelse(heavenlyChocolates$Day == "Sat" | heavenlyChocolates$Day == "Sun", T, F)
weekdayAvg = mean(heavenlyChocolates[heavenlyChocolates$IsWeekend == F, amountColName])
weekendAvg = mean(heavenlyChocolates[heavenlyChocolates$IsWeekend == T, amountColName])
avgAmtDf = data.frame(c("Weekday", "Weekend"), c(weekdayAvg, weekendAvg))
colnames(avgAmtDf) = c('Week', 'AverageAmount')
ggplot(avgAmtDf, aes(Week, AverageAmount)) + 
  geom_col() +
  labs(
    x = "",
    y = "Average Amount Spent",
    title = "Average Amount Spent by Visitors: Weekday vs. Weekend"
  )
