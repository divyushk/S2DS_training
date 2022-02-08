library(tidyverse)
library(nycflights13)

flights <- flights

nov_dec <- flights %>% 
  filter(month %in% c(11,12))

delayed_more_than_2hrs <- flights %>% 
  filter(arr_delay >= 2)

houston_flights <- flights %>% 
  filter(dest %in% c("IAH", "HOU"))

UA_AA_DL_flights <- flights %>% 
  filter(carrier %in% c("UA", "DL", "AA"))

summer_flights <- flights %>% 
  filter(between(month,7,9))

late_arr2_punctual_departure <- flights %>% 
  filter(arr_delay > 120 & dep_delay <= 0)

hour_late_departure_30min_recovery <- flights %>% 
  filter(dep_delay >= 60, dep_delay - arr_delay > 30)

early_morning_flights <- flights %>% 
  filter(dep_time == 2400 | dep_time <= 600)

missing_dep_time <- flights %>% 
  filter(is.na(dep_time))

#####

arrange(flights, year, month, day)
flights

arrange(flights, desc(dep_delay))

arrange(flights, desc(is.na(dep_time)), dep_time)

flights %>% 
  arrange(desc(dep_delay))

flights %>% 
  arrange(dep_delay)

flights %>% 
  arrange(air_time)

flights %>% 
  arrange(desc(distance))

flights %>% 
  arrange(distance)

head(arrange(flights, desc(distance / air_time)))

vars <- c("year", "month", "day", "dep_delay", "arr_delay")

select(flights, all_of(vars))

select(flights, starts_with("dep"), starts_with("arr"))

flights_sml <- select(flights, year:day, ends_with("delay"), distance, air_time)

mutate(flights_sml,
       gain = dep_delay - arr_delay,
       speed = distance / air_time * 60)

flights_times <- mutate(flights,
                        dep_time_mins = (dep_time %/% 100 * 60 + dep_time %% 100) %% 1440,
                        sched_dep_time_mins = (sched_dep_time %/% 100 * 60 +
                                                 sched_dep_time %% 100) %% 1440
)
# view only relevant columns
select(
  flights_times, dep_time, dep_time_mins, sched_dep_time,
  sched_dep_time_mins
)

flights_delays <- flights %>% 
  mutate(dep_time_min = (dep_time %/% 100 * 60 + dep_time %% 100) %% 1440,
         sched_dep_time_min = (arr_time %/% 100 * 60 + arr_time %% 100) %% 1440,
         dep_delay_diff = dep_delay - dep_time_min + sched_dep_time_min
  )

filter(flights_delays, dep_delay_diff != 0)
  
flights_top10_delays <- flights %>% 
  mutate(dep_delay_min_rank = min_rank(dep_delay),
         dep_delay_row_number = row_number(dep_delay),
         dep_delay_dense_rank = dense_rank(dep_delay))

cancelled_per_day <- flights %>% 
  mutate(cancelled = (is.na(arr_delay) | is.na(dep_delay))) %>% 
  group_by(year, month, day) %>% 
  summarise(cancelled_num = sum(cancelled),
            flights_num = n())

ggplot(cancelled_per_day) +
  geom_point(aes(x = flights_num, y = cancelled_num))

cancelled_and_delays <- 
  flights %>%
  mutate(cancelled = (is.na(arr_delay) | is.na(dep_delay))) %>%
  group_by(year, month, day) %>%
  summarise(
    cancelled_prop = mean(cancelled),
    avg_dep_delay = mean(dep_delay, na.rm = TRUE),
    avg_arr_delay = mean(arr_delay, na.rm = TRUE)
  ) %>%
  ungroup()

ggplot(cancelled_and_delays) +
  geom_point(aes(x = avg_dep_delay, y = cancelled_prop))

flights %>% 
  group_by(carrier) %>% 
  summarise(arr_delay = mean(arr_delay, na.rm = TRUE)) %>% 
  arrange(desc(arr_delay))

filter(airlines, carrier == "F9")

flights %>% 
  filter(is.na(arr_delay)) %>% 
  # Total delay by carrier within each origin, dest
  group_by(origin, dest, carrier) %>% 
  summarise(arr_delay = sum(arr_delay),
            flights = n())

flightsx <- flights %>% 
  filter(arr_delay > 0) %>% 
  group_by(dest) %>% 
  mutate(arr_delay_total = sum(arr_delay),
         arr_delay_prop = arr_delay / arr_delay_total) %>% 
  select(dest, month, day, dep_time, carrier, flight,
         arr_delay, arr_delay_prop) %>%
  arrange(dest, desc(arr_delay_prop))

