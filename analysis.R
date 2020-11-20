library("tidyverse")

incarceration <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv", stringsAsFactors=FALSE)

# Summary information 5 values that I am looking at

# Total pop by most recent year on the state level
state_total_pop_recent <- incarceration %>% 
  group_by(state) %>%
  filter(year == max(year)) %>%
  summarise(current_year = year, total_jail_population = sum(total_pop)) %>% 
  unique()

# Total pop by most early year on the state level
state_total_pop_past <- incarceration %>% 
  group_by(state) %>%
  filter(year == min(year)) %>%
  summarise(earliest_year = year, total_jail_population = sum(total_pop)) %>% 
  unique()

# Incarceration growth rate for each state
merge_tables <- merge(x = state_total_pop_recent, y = state_total_pop_past, by = "state", all.x = TRUE)

rate_vec <- (merge_tables$total_jail_population.x - merge_tables$total_jail_population.y) / (merge_tables$current_year - merge_tables$earliest_year)

state_incarceration_rate <- merge_tables %>% 
  cbind(incarceration_rate = rate_vec)

# State incarceration by race in most recent year
state_race_pop <- incarceration %>%
  group_by(state) %>%
  filter(year == max(year)) %>% 
  summarise(aapi_jail_pop = sum(aapi_jail_pop, na.rm = T),
            black_jail_pop = sum(black_jail_pop, na.rm = T), 
            latinx_jail_pop = sum(latinx_jail_pop, na.rm = T), 
            native_jail_pop = sum(native_jail_pop, na.rm = T), 
            white_jail_pop = sum(white_jail_pop, na.rm = T), 
            other_race_jail_pop = sum(other_race_jail_pop, na.rm = T))

state_race_pop_std_dev <- state_race_pop %>% 
  rowwise() %>% 
  mutate(std_dev = sd(c(aapi_jail_pop, black_jail_pop, latinx_jail_pop, native_jail_pop, white_jail_pop, other_race_jail_pop)))

# My 5 values:
  
# 1. Highest jail population by state by most recent year
state_highest_pop_recent <- state_total_pop_recent[which.max(state_total_pop_recent$total_jail_population),]

# 2. Lowest jail population by state by most recent year
state_lowest_pop_recent <- state_total_pop_recent[which.min(state_total_pop_recent$total_jail_population),]
  
# 3. Highest incarceration rate by state
state_max_incareceration_rate <- state_incarceration_rate[which.max(state_incarceration_rate$incarceration_rate),]

# 4. Lowest jail incarceration rate by state
state_min_incareceration_rate <- state_incarceration_rate[which.min(state_incarceration_rate$incarceration_rate),]

# 5. State with the highest discrepancy in race population in the most recent year
state_max_std_dev_recent <- state_race_pop_std_dev[which.max(state_race_pop_std_dev$std_dev),]

# Variables to analyze

# Trends over time chart

# Variable comparsion chart

# Map
