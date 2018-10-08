library(dplyr)
library(ggplot2)

library(rstudioapi) 
setwd(dirname(getActiveDocumentContext()$path))
source("config.R")
setRawDataPath()

#####Case1: Nathan’s Hot Dog Eating Contest#####
hotdog = read.csv("hotdog.csv")
hotdog %>%
  ggplot(aes(Year, Dogs.eaten, fill = New.record)) +
    geom_col() +
    scale_fill_gradient(low = "lightblue", high = "red") +
    labs(
      y = "",
      title = "Number of Hot Dogs Eaten") +
    theme(
      legend.position = "none"
    )

#####Case2: General Social Survery (GSS)#####
gss = read.csv("gss.csv")

# a.
party_count = 
  gss %>%
    group_by(partyid) %>%
    summarise(party_count = n()) %>%
    filter(party_count >= 2000)

# b.
total_counts = sum(party_count$party_count)
party_count %>%
  mutate(percent = party_count/total_counts) %>%
  ggplot(aes(reorder(partyid, -percent), percent, fill = partyid)) +
    geom_col() +
    scale_fill_manual(values = c(
      "Ind,near dem" = "purple", 
      "Independent" = "purple", 
      "Not str democrat" = "lightblue", 
      "Not str republican" = "red",
      "Strong democrat" = "lightblue", 
      "Strong republican" = "red"
    )) +
    labs(
      x = "",
      y = "Percent(%)"
    ) +
    theme(
      legend.position = "none"
    )

# c.
#geom_density
gss %>%
  filter(race %in% c("Black", "White") & !is.na(age)) %>%
  group_by(race, age) %>%
  ggplot(aes(age, ..density.., fill = race)) +
  geom_density(color = "white", alpha = 0.4) +
  geom_line(stat = "density")

#geom_area
gss %>%
  filter(race %in% c("Black", "White") & !is.na(age)) %>%
  group_by(race, age) %>%
  ggplot(aes(age, ..density.., fill = race)) +
  geom_area(stat = "density", position = "dodge", color = "white", alpha = 0.4) +
  geom_line(stat = "density")

# d.
age_level = c("younger", "older")
gss %>%
  mutate(age_group = factor(ifelse(gss$age < 50, "younger", "older"), levels = age_level)) %>%
  filter(age_group %in% age_level) %>%
  group_by(partyid, age_group) %>%
  ggplot(aes(partyid, fill = age_group)) +
    geom_bar(position = "dodge") +
    scale_fill_manual(values = c(
      "younger" = "lightblue", 
      "older" = "pink"
    )) +
    ggtitle("Number of People: Party Aff. vs. Age (<50: Blue, >=50: Pink)") +
    theme(
      legend.position = "none"
    ) +
    coord_flip()

#e.
gss %>%
  group_by(relig) %>%
  summarise(avg_hour = mean(tvhours, na.rm = TRUE)) %>%
  ggplot(aes(avg_hour, reorder(relig, avg_hour))) +
    geom_point() +
    theme_bw() +
    labs(
      x = "Avg. TV Hours",
      y = "Religion"
    )
