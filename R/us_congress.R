library(dplyr)
library(ggplot2)
library(rstudioapi)
setwd(dirname(getActiveDocumentContext()$path))
source("config.R")
setRawDataPath()
congress = read.csv("congress.csv", header = T)

#Trend of Senate Representatives
congress %>%
  filter(party %in% c("D", "R"), chamber == "senate") %>% group_by(congress, party) %>%
  summarise(num_rep = n()) %>%
  ggplot(aes(congress, num_rep, color = party)) + geom_point() +
  geom_smooth(se = F) +
  labs(title = "Trend of Senate Representatives",
       x = "Congress",
       y = "Number of Representatives")

#Age Distribution for Congress 90
roundedAge = as.integer(congress$age)
congress$age_group = 
  ifelse(roundedAge <= 30, "Below 30 years",
         ifelse(roundedAge <= 50, "(30, 50] years", 
                ifelse(roundedAge <= 65, "(50, 65] years", "Above 65 years")))
state_list = c("CA", "NY", "TX", "PA", "IL")
congress %>%
  filter(congress == 90, state %in% state_list) %>% 
  group_by(state, age_group) %>%
  summarise(count = n()) %>%
  mutate(factor(state, levels = state_list)) %>%
  ggplot(aes(reorder(state, count), count, fill = age_group)) +
  geom_col(position = 'dodge') +
  labs(title = "Age Distribution for Congress 90",
       x = "",
       y = "Number of Representatives",
       fill = "Age Group")

#Democrat and Republican Distribution
congress %>% 
  filter(party %in% c("D", "R")) %>%
  mutate(party = ifelse(party == "D", "Democrats", "Republican")) %>%
  ggplot(aes(age, ..density..)) +
    geom_histogram() +
    facet_wrap(~party, nrow = 2) +
    geom_line(stat = "density", color = "red") + 
    xlab("Age (Years)") + 
    scale_x_continuous(breaks = seq(0, 100, 10)) + 
    scale_y_continuous(breaks = NULL) +
    theme(axis.text.y=element_blank(),
          axis.ticks.y=element_blank())
