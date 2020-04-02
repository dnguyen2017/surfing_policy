# based on code from Moragan Kain: https://github.com/morgankain/COVID_interventions
# Simulation parameters

covid_params <- c(
  beta0            = 0.5
  , Ca               = 1
  , Cp               = 1
  , Cs               = 1
  , Cm               = 1
  , alpha            = 1/3
  , gamma            = 1/5.2
  , lambda_a         = 1/7
  , lambda_s         = 1/4
  , lambda_m         = 1/7
  , lambda_p         = 1/0.5
  , rho              = 1/10.7
  , delta            = 0.2
  , mu               = 19/20
  , N                = 1937570 # (Santa Clara County) 59.02e6 (Wuhan)
  , E0               = 20
  # , intervention     = 2
  # , thresh_H_start   = 3
  # , thresh_H_end     = 2
  # , thresh_int_level = 0.1
)

c_iso_severe <- 0
c_iso_mild   <- 0.2
c_social_dist <- 0.2
c_thresh_int <- 0.2

start_thresh_int <- 20 # number of hospitalized cases
stop_thresh_int <- 2   # number of hostpitalized cases

source("mk_covid-19_setup.R")

# simulate with a set of parameters
sim <- do.call(
  pomp::simulate
  , list(
    object       = covid
    , params       = covid_params
    , nsim         = 50
    , format       = "d"
    , include.data = F
    , seed         = 1001)
) #%>% # no clue what this is doing
# {rbind(.,
#            group_by(., day) %>%
#              select(-.id) %>%
#              summarise_all(median) %>%
#              mutate(.id = "median"))} 

sim %>%
  ggplot() +
  geom_line(aes(x = day, y = Ia + Ip + Is, group = .id), col = "red") +
  geom_line(aes(x = day, y = H, group = .id), col = "darkgreen") +
  ylim(0, 1000)#+
geom_line(aes(x = day, y = S, group = .id), col = "blue")


sim %>% filter(.id == 1) %>% View()


# calculate total person-days locked down and total dead
summary_df <-
  sim %>%
  group_by(.id) %>%
  summarize(person_days_locked = sum(thresh_crossed*(S + E + Ia + Ip + R)),
            total_dead = last(D))
