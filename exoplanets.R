setwd("C:/Users/k1599153/OneDrive - King's College London/exoplanets")
#####
library(tidyverse)
exoplanets <- read_csv("exoplanets.csv")
view(exoplanets)
####Question 1####

str(exoplanets)

####There are 4521 records in the dataframe

####Question 2####

colnames(exoplanets)

####Question 3####
exoplanets %>%
  mutate(round(exoplanets$planet_orbital_period_(days), digits = 0))


exoplanets %>%
  round(`planet_orbital_period_(days)`, digits = 0) %>%
  summarize(mean = mean(`planet_orbital_period_(days)`))

exoplanets$orbital <- rename(exoplanets, orbital, planet_orbital_period_(days))
  

####Question 4####
ggplot(exoplanets, aes(x = discovery_method)) +
  geom_bar()

####Answer: Transit

####Question 5####

exoplanets %>%
  filter(discovery_method == "Transit") %>%
  group_by(planet_name) %>%
  summarize(mean(`planet_mass_(earths)`))
####Answer: CoRoT-14 b 2415 earths

####Question 7
exoplanets %>%
  ggplot(exoplanets, aes(x = `system_distance_from_earth_(pc)`, y = ))
