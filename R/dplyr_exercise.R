library(dplyr)
library(ggplot2)
library(MASS)
library(gcookbook)

library(rstudioapi)
setwd(dirname(getActiveDocumentContext()$path))
source("config.R")

#####Example1: hflight#####
library(hflights)

hflights %>%
  filter(Cancelled != 0) %>%
  select(one_of("Origin", "Dest"))

# Relationship between distance travelled and arrival delays
hflights %>%
  group_by(Dest) %>% 
  summarize(count = n(), dist = mean(Distance, na.rm = T), delay = mean(ArrDelay, na.rm = T)) %>%
  filter(dist < 2000) %>% #get rid of outliers
  ggplot(aes(dist, delay)) + 
    geom_point() + 
    geom_smooth()

# Compare the individual carriers
hflights %>% 
  group_by(UniqueCarrier) %>% 
  summarise(n_flights = n(),
            n_canc = sum(Cancelled == 1),
            avg_delay = mean(ArrDelay, na.rm = TRUE)) %>% 
  arrange(avg_delay, n_canc)

#####Example2: diamonds (geom_col)#####
diamonds %>%
  group_by(cut) %>%
  summarise(avg_price = mean(price)) %>%
  ggplot(aes(reorder(cut, -avg_price), avg_price)) + 
  geom_col() +
  labs(
    x = "Quality of Cut",
    y = "Average Price in USD",
    title = "Average Diamond Price by Cut"
  )

#####Example3: faithful (histogram & line)#####
ggplot(faithful, aes(waiting)) +
  geom_histogram(binwidth = 10, fill = "white", color = "black")

ggplot(faithful, aes(waiting, ..density..)) + #use density as y values, instead of count (too big)
  geom_histogram(fill = "lightblue", color = "white", size = 0.2) + #size -> border size
  geom_line(stat = "density") +
  geom_density(fill = "blue", adjust = 2, color = NA, alpha = 0.2) +
  geom_line(stat = "density", adjust = 0.25, color = "red") +
  geom_line(stat = "density", adjust = 2, color = "blue") +
  xlim(35, 105) +
  theme_bw()

#####Example4: Birth Weight (histogram & boxplot)#####
#library(MASS)

# histogram facet - by smokers or not
birth_weight = 
  birthwt %>%
  mutate(smoke = factor(ifelse(smoke == 0, "No Smoke", "Smoke")))

birth_weight %>%
  ggplot(aes(bwt)) +
    geom_histogram(fill = "white", colour = "black") + 
    facet_wrap(~smoke)

# histogram facet - by race
birth_weight$race <- factor(birth_weight$race) 
levels(birth_weight$race) <- c("White", "Black", "Other")

ggplot(birth_weight, aes(bwt)) +
  geom_histogram(fill = "white", colour = "black") + 
  facet_wrap(~race)

# histogram overlay - by smoker or not
ggplot(birth_weight, aes(bwt, fill = smoke)) + 
  geom_histogram(position = "identity", alpha = 0.6)

# boxplot by race
ggplot(birth_weight, aes(race, bwt)) +
  geom_boxplot() +
  stat_summary(fun.y = "mean", geom = "point", color = "red") +
  scale_y_continuous(breaks = NULL) #remove y axis breaks

#####Example5: biochemical oxygen demand (Line Graphs)#####
ggplot(BOD, aes(x = factor(Time), y = demand, group = 1)) +  
  #If you don't add 'group = 1', then you will get an error message stating that 
  #`Each group consists of only one observation`
  geom_line() +
  geom_point() +
  scale_x_discrete(name= "", breaks = 1:7, labels = paste(1:7, "min")) + 
  scale_y_continuous(name = "Demand", breaks = 8:20) + 
  geom_vline(xintercept = 3, color = "red", linetype = "dashed") + 
  theme_bw()

#####Example6: ToothGrowth (Line Graphs)#####
tg = ToothGrowth %>%
  group_by(supp, dose) %>% 
  summarise(length = mean(len))

ggplot(tg, aes(factor(dose), length, group = supp, colour = supp, linetype = supp)) + 
  geom_line() +
  labs(
    x = "Dose (milligrams/day)",
    y = "Average Tooth Length",
    color = "Supplement type",
    linetype = "Supplement type"
  )

#####Example7: US population (Stacked Area Graphs)#####
#library(gcookbook)
ggplot(uspopage, aes(Year, Thousands, fill = AgeGroup)) + 
  geom_area(color = "white", size = 0.2)

#proportional stacked area graph
uspopage %>%
  group_by(Year) %>%
  mutate(percentage = Thousands*100/sum(Thousands)) %>%
  ggplot(aes(Year, percentage, fill = AgeGroup)) +
    geom_area(color = "black", size = 0.1)
