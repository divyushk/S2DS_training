# setwd("C:/Users/k1599153/OneDrive - King's College London/exoplanets")

#####
library(tidyverse)
library(janitor)
exoplanets <- read_csv("exoplanets.csv") %>% 
  janitor::clean_names()
view(exoplanets)

####Question 1####

number_of_records <- nrow(exoplanets)

#There are 4521 records in the dataframe

####Question 2####

column_names <- colnames(exoplanets)
print(column_names)

####Question 3####

mean_orbital_period <- mean(exoplanets$planet_orbital_period_days, na.rm = TRUE)
print(mean_orbital_period)
median_orbital_period <- median(exoplanets$planet_orbital_period_days, na.rm = TRUE)
print(median_orbital_period)

####Question 4####

top_discovery_method <- exoplanets %>% 
  count(discovery_method) %>%  
  arrange(desc(n))

popular_discovery_method <- top_n(top_discovery_method, 1)
print(popular_discovery_method)[1]

pop_discovery_method <- top_discovery_method %>% 
  top_n(1) %>% 
  select(discovery_method) %>% 
  unlist() %>% 
  unname()

#Answer: Transit

####Question 5####

planet_mass <- exoplanets %>%
  filter(discovery_method == pop_discovery_method) %>%
  arrange(desc(planet_mass_earths))
planet_mass

largest_mass <- top_n(planet_mass, 1, wt = planet_mass$planet_mass_earths)

planet_largest_mass <- largest_mass %>% 
  top_n(1) %>% 
  select(planet_mass_earths) %>% 
  unlist() %>% 
  unname()
planet_largest_mass

####Answer: K2-52 b 457000 earths

####Question 6####

exoplanets2 <- exoplanets %>% 
  filter(planet_mass_earths != planet_largest_mass)

exoplanets2 %>% 
  filter(discovery_method == "Transit") %>% 
  arrange(desc(planet_mass_earths))

exoplanets2 %>% 
  select(planet_mass_earths) %>% 
  arrange(desc(planet_mass_earths))
largest_mass2 <- top_n(exoplanets2, 1, wt = exoplanets2$planet_mass_earths)
print(largest_mass2)

exoplanets %>% 
  filter(planet_mass_earths == planet_largest_mass)

#Answer: Kepler-840 b; mass = 43000 earths

####Question 7####

exoplanets2 %>% 
  ggplot(aes(x = system_distance_from_earth_pc, 
             y = star_mass_suns, 
             color = discovery_method)) +
  geom_jitter() +
  labs(x = "Distance from Earth", y = "The mass of the star in suns")

####Question 8####

scatterplot2 <- function(var1 = system_distance_from_earth_pc, 
                         var2 = star_mass_suns) {
  exoplanets2 %>% 
    ggplot(aes(x = .data[[var1]], 
               y = .data[[var2]],
               color = discovery_method)) +
    geom_jitter()
}

scatterplot <- function(var1 = exoplanets2$system_distance_from_earth_pc, var2 = exoplanets2$star_mass_suns) {
  exoplanets2 %>% 
    ggplot(aes(x = var1, 
               y = var2,
               color = discovery_method)) +
    geom_jitter()
}

scatterplot2("star_luminosity_abs", "star_temp_k")


