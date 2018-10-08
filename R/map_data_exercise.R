library(dplyr)
library(ggplot2)

library(rstudioapi) 
setwd(dirname(getActiveDocumentContext()$path))
source("config.R")
setRawDataPath()

#####data_map in ggplot2#####
getMap = function(scope, region = "") {
  map_data(scope, region) %>%
    ggplot(aes(long, lat, group = group)) +
      geom_polygon(fill = "white", color = "black")
}

getUsaMap = function(region_scope) {
  getMap(region_scope)
}

getMapOutsideUsa = function(country) {
  getMap("world", country)
}

getMap("world")
getUsaMap("usa")
getUsaMap("state")
getUsaMap("county")
getMapOutsideUsa("taiwan")
getMapOutsideUsa(c("Japan", "China", "North Korea", "South Korea"))

#####Choropleth Map: USA Assault#####
plotMap = function(map_df) {
  ggplot(map_df, aes(long, lat, group = group))
}

usa_map = map_data("state")
usa_crimes = 
  USArrests %>%
  mutate(region = tolower(rownames(USArrests)))
usa_crime_map = 
  left_join(usa_map, usa_crimes, by = "region")
plotMap(usa_crime_map) +
  geom_polygon(aes(fill = Assault), color = "black") +
  scale_fill_gradient(low = "white", high = "darkred") +
  coord_map("polyconic") +
  theme_void()

#####Choropleth Map: Starbucks Franchise#####
usa_starbucks_map = 
  read.csv("starbucks_US.csv", stringsAsFactors = F) %>%
  group_by(State) %>%
  summarise(state_stores = n()) %>%
  mutate(State = tolower(State)) %>%
  full_join(usa_map, by = c("region" = "State"))
plotMap(usa_starbucks_map) +
  geom_polygon(aes(fill = state_stores), color = "black") +
  scale_fill_gradient(low = "white", high = "darkred") +
  coord_map("polyconic") +
  theme_void() +
  labs(
    title = "Starbucks Franchise across United States", 
    fill = "Number of Stores"
  ) +
  theme(legend.position = "bottom")
