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
  filter(!is.na(r_values_mean)) %>%
  #filter(date <= Sys.Date()) %>%
  filter(region != "") %>% # remove blank region (is this national level?)
  ggplot(aes(x = date, y = `r_values_mean`)) +
#  coord_flip() +
  geom_line(size = 2) +
  geom_hline(aes(yintercept = 1), linetype = "dashed", col = "red", size = 1) +
  facet_wrap(~region) +
  theme_minimal() +
  ylim(0, 3.75) +
labs(title = "Estimates of R(t) for US states and territories", 
       #bquote("Estimates"~"of"~R[t]~"for"~"US"~"states"~"and"~"territories"),
       caption = "source: Youyang Gu's COVID19 projections \nhttps://github.com/youyanggu/covid19_projections/",
     y = "R(t)") +
  theme(text = element_text(size = 16),
        axis.text=element_text(size=12, colour="black"),
        axis.title=element_text(size=16)
        )

# state policy actions (complied by Easton White)
# need to add reopening dates

actions <- read.csv(url("https://raw.githubusercontent.com/eastonwhite/COVID19_US_States/master/US_states_correlates/States_by_Actions.csv"))

names(actions)[1] <- "sub_region_1"
actions$sub_region_1 <- state.abb[match(actions$sub_region_1, state.name)]

actions <-
  actions %>%
  pivot_longer(cols = c("StateOfEmergency", "LimitGatherings", "ClosePublicSchools", "RestrictBusinesses", "StayAtHome"),
               names_to = "action",
               values_to = "date") %>%
  mutate(date = as.Date(date))

# plot google mobility and dates of state actions
ggplot() +
  geom_line(data = filter(gmob,sub_region_1 != ""  # remove national average data
                                ,sub_region_2 == ""), 
            aes(x = date, y = percent_change, col = location_type)) +
  geom_vline(data = actions,
             aes(xintercept = date, col = action), alpha = 0.5) +
  # geom_point(data = actions,
  #            aes(x = date, y = jitter(0, 5), col = action, shape = action)) +
  facet_wrap(~sub_region_1)

# same as above but without parks usage
ggplot() +
  geom_line(data = filter(gmob,
                          sub_region_1 != ""  # remove national average data
                          ,!is.na(sub_region_1)
                          ,sub_region_2 == ""
                          ,location_type != "parks"), 
            aes(x = date, y = percent_change, col = location_type)) +
  geom_vline(data = filter(actions, !is.na(sub_region_1)) ,
             aes(xintercept = date, col = action), alpha = 0.5) +
  geom_hline(data = filter(actions, !is.na(sub_region_1)), aes(yintercept = 0), col = "red") +
  facet_wrap(~sub_region_1) +
  theme_minimal()


# R(t) with policy dates
actions$region <- actions$sub_region_1


ggplot() +
  geom_line(data = filter(yyg, !is.na(r_values_mean) & region != "")
            ,aes(x = date, y = `r_values_mean`)) +
  geom_hline(data = filter(actions, !is.na(sub_region_1)),
             aes(yintercept = 1), linetype = "dashed", col = "red", size = 1) +
  geom_vline(data = filter(actions, !is.na(sub_region_1)) ,
             aes(xintercept = date, col = action), alpha = 0.5) +
  facet_wrap(~region) +
  theme_minimal() +
  ylim(0, 3.75) +
  labs(title = "Estimates of R(t) for US states and territories", 
       #bquote("Estimates"~"of"~R[t]~"for"~"US"~"states"~"and"~"territories"),
       caption = "source: Youyang Gu's COVID19 projections \nhttps://github.com/youyanggu/covid19_projections/",
       y = "R(t)") +
  theme(text = element_text(size = 16),
        axis.text=element_text(size=12, colour="black"),
        axis.title=element_text(size=16))