library(tidyverse)

# google mobility data

gmob_global <- read.csv(url("https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv"))
gmob <- filter(gmob_global, country_region_code == "US") 
gmob$date <- as.Date(gmob$date)
gmob$sub_region_1 <- state.abb[match(gmob$sub_region_1, state.name)]   # change full state name to abbreviation
  
gmob <-
  gmob %>%
  pivot_longer(cols = 6:11,
               names_to = "location_type",
               values_to = "percent_change") %>%
  mutate(location_type = str_remove_all(location_type, "_percent_change_from_baseline"))

# plot percent mobility change for all states
gmob %>%
  filter(sub_region_1 != ""  # remove national average data
    ,sub_region_2 == "") %>% # state-level data (group county data together)
ggplot(aes(x = date, y = percent_change, col = location_type)) +
  geom_line() +
  facet_wrap(~sub_region_1) +
  theme_minimal() +
  theme(legend.position = "bottom")

# plot specific state
whichstate <- "TX"
gmob %>%
filter(sub_region_1 == whichstate, sub_region_2 == "") %>%
ggplot(aes(x = date, y = percent_change, group = location_type, col = location_type)) +
  geom_line() +
  facet_wrap(~sub_region_1) +
  theme_minimal() +
  theme(legend.position = "bottom")
  

# Youyang Gu COVID-19 projections and estimates of R_t
yyg <- read.csv(url("https://raw.githubusercontent.com/youyanggu/covid19_projections/master/projections/combined/latest_us.csv"))
yyg$date <- as.Date(yyg$date)


names(yyg)

yyg %>%
  filter(region == "WA") %>%
  select()

yyg %>%
  filter(date <= Sys.Date()) %>%
  filter(region != "") %>% # remove blank region (is this national level?)
  ggplot(aes(x = date, y = `r_values_mean`)) +
#  coord_flip() +
  geom_line() +
  geom_hline(aes(yintercept = 1), linetype = "dotted", col = "red") +
  facet_wrap(~region) +
  theme_minimal()

str(yyg)

unique(yyg$r_values_mean)

yyg %>% filter(!is.na('r_values_mean')) %>% select(date, `r_values_mean`, region)
