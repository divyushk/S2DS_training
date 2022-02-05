setwd("C:/Users/k1599153/OneDrive - King's College London/exoplanets")

#####
library(tidyverse)
exoplanets <- read_csv("exoplanets.csv")
view(exoplanets)

####Question 1####

number_of_records <- nrow(exoplanets)

#There are 4521 records in the dataframe

####Question 2####

column_names <- colnames(exoplanets)
print(column_names)

####Question 3####

mean_orbital_period <- mean(exoplanets$`planet_orbital_period_(days)`, na.rm = TRUE)
print(mean_orbital_period)
median_orbital_period <- median(exoplanets$`planet_orbital_period_(days)`, na.rm = TRUE)
print(median_orbital_period)

####Question 4####

top_discovery_method <- exoplanets %>% 
  count(discovery_method) %>%  
  arrange(desc(n))

popular_discovery_method <- top_n(top_discovery_method, 1)
print(popular_discovery_method)[1]

#Answer: Transit

####Question 5####

planet_mass <- exoplanets %>%
  filter(discovery_method == "Transit") %>%
  arrange(desc(`planet_mass_(earths)`))
view(planet_mass)

largest_mass <- top_n(planet_mass, 1, wt = planet_mass$`planet_mass_(earths)`)
print(largest_mass)

####Answer: K2-52 b 457000 earths

####Question 6####

exoplanets2 <- exoplanets %>% 
  filter(`planet_mass_(earths)` < 450000)

largest_mass2 <- top_n(exoplanets2, 1, wt = exoplanets2$`planet_mass_(earths)`)
print(largest_mass2)

#Answer: Kepler-840 b; mass = 43000 earths

####Question 7####

exoplanets2 %>% 
  ggplot(aes(x = `system_distance_from_earth_(pc)`, 
             y = `star_mass_(suns)`, 
             color = discovery_method)) +
  geom_jitter() +
  labs(x = "Distance from Earth", y = "The mass of the star in suns")




