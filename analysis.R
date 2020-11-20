library("tidyverse")
library("ggplot2")
library("maps")
library("mapproj")

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
# Trends over time chart: How does national jail population grow over time from 1970 to 2018 in the US?
national_jail_pop_by_year <- incarceration %>%
  group_by(year) %>%
  summarise(national_jail_pop = sum(total_pop)) %>%
  filter(year %in% c(1990 : 1999))

national_jail_pop_by_year_graph <- ggplot(data = national_jail_pop_by_year) +
  geom_point(mapping = aes(x = year, y = national_jail_pop)) +
  geom_smooth(mapping = aes(x = year, y = national_jail_pop)) +
  labs(
    title = "National Jail Population against time from 1990 to 1999",
    x = "Year",
    y = "National jail population"
  )

# Variable comparison chart
# Jail admission count vs Jail discharge count on the national level in 2018 by county.
state_adm_and_dis <- incarceration %>%
  group_by(county_name) %>%
  filter(year == max(year)) %>%
  summarise(total_admissions = total_jail_adm, total_discharge = total_jail_dis)

county_adm_and_dis <- ggplot(data = state_adm_and_dis) +
  geom_point(mapping = aes(x = total_admissions, y = total_discharge)) +
  labs(
    title = "Admission vs Discharge counts of each county in 2018",
    x = "Admissions Count",
    y = "Discharges Count"
  )

# Map
# Jail population for each county heat map in 2018
counties_shape <- map_data("county")

blank_theme <- theme_bw() +
  theme(
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank()
  )

jail_pop_by_county_2018 <- incarceration %>%
  group_by(fips) %>%
  filter(year == max(year)) %>%
  summarise(fips = fips, jail_pop = total_pop)

filter(maps::county.fips, grepl(":", polyname))

fipstab <-
  transmute(maps::county.fips, fips, county = sub(":.*", "", polyname)) %>%
  unique() %>%
  separate(county, c("region", "subregion"), sep = ",")

counties_shape <- left_join(counties_shape, fipstab, c("region", "subregion"))

anti_join(counties_shape, jail_pop_by_county_2018, "fips") %>%
  select(region, subregion) %>%
  unique()

county_jail_pop <- left_join(counties_shape, jail_pop_by_county_2018, "fips")
  filter(county_jail_pop, is.na(jail_pop)) %>%
  select(region, subregion, jail_pop) %>%
  unique()

counties_map <- ggplot(county_jail_pop) +
  geom_polygon (
    mapping = aes(x = long, y = lat, group = group, fill = jail_pop),
    color = "white",
    size = .1
  ) +
  coord_map() +
  scale_fill_continuous(low = "#132B43", high = "Red") +
  labs(
    title = "Jail Population By county in 2018",
    fill = "Jail population") +
  blank_theme








