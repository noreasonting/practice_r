library(rstudioapi)
setwd(dirname(getActiveDocumentContext()$path))
source("config.R")

library(ggplot2)
library(gcookbook)

#####Scatterplots#####
ggplot(mpg, aes(x = displ, y = hwy, color = class, size = class)) + 
  geom_point(alpha = 0.4)

ggplot(mpg, aes(x = displ, y = hwy, color = class)) + 
  geom_point() +
  facet_wrap(~class)

#####Bar Graphs#####
ggplot(BOD, aes(factor(Time), demand)) + 
  geom_col(fill ="lightblue") +
  geom_text(aes(label = demand, vjust = -0.5))

ggplot(diamonds, aes(cut, fill = clarity)) +
  geom_bar(position = position_dodge(1))

subclimate <- climate[climate$Source == "Berkeley" & climate$Year >=1900,]
subclimate$Pos = ifelse(subclimate$Anomaly10y >=0, T, F)
ggplot(subclimate, aes(Year, Anomaly10y, fill = Pos)) + 
  geom_col() +
  theme(legend.position = "none")

#####Cleveland Dot Plot or Dot Plots#####
top10 <- tophitters2001[1:10, c('name', 'avg')] 
ggplot(top10, aes(avg, reorder(name, avg))) +
  geom_point(size = 3)

syrianRefugees = read.csv("syria_refugees.csv")
sortedRefugees = syrianRefugees[order(-syrianRefugees$refugees), ]
top20 = sortedRefugees[1:20, ]
ggplot(top20, aes(refugees, reorder(Country, refugees))) + 
  geom_point() +
  labs(
    x = "Number of Syrian Refugees",
    y = "",
    title = "Syrian Refugee Crisis"
  )
