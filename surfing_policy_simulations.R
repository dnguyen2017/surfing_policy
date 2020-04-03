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

# time line
years <- 5
sim_length <- round(365.25*years)
# t_nothing <- 60
# t_social_dist <- c(30, 60, 90, sim_length - t_nothing)
# control_policy <- 2 # 0 = do nothing, 1 = social distancing, 2 = surfing

# effect of interventions (proportion reduction of beta)
c_iso_severe <- 0
c_iso_mild   <- 0.2
c_social_dist <- 0.2
c_thresh_int <- 0.2

# surfing rules
start_thresh_int <- 20 # number of hospitalized cases
stop_thresh_int <- 2   # number of hostpitalized cases

# make matrix of policy choices
control_pars <- 
  expand.grid(t_nothing = 60,             # days at beginning of outbreak w/o control
              t_social_dist = c(30, 60, 90), # days of social distancing
              control_policy = c(0,1,2))  # policy choice for remaining days: do nothing, social dist, surfing

sim_list <- vector("list", length = nrow(control_pars))
n_sim <- 2
  
for (i in seq_along(sim_list)) {
  t_nothing <- control_pars[i, "t_nothing"]
  t_social_dist <- control_pars[i, "t_social_dist"]
  control_policy <- control_pars[i, "control_policy"]
  
  source("mk_covid-19_setup.R")
  
  # create df of sim pars
  sim_pars <- replicate(n_sim * sim_length, control_pars[i,], simplify = FALSE) %>% bind_rows()
  
  # simulate with a set of parameters
  sim_list[[i]] <- do.call(
    pomp::simulate
    , list(
      object       = covid
      , params       = covid_params
      , nsim         = n_sim
      , format       = "d"
      , include.data = F
      , seed         = 1001)
  ) %>% cbind(sim_pars)
  
}

sim_df <- bind_rows(sim_list)
head(sim_df)

sim_list[[4]] %>% filter(.id == 1) %>% View()

sim_df %>%
  ggplot(aes(x = day, y = policy)) +
  geom_point() +
  facet_grid(t_social_dist ~ control_policy)

# rename control policies
sim_df <-
  sim_df %>%
  mutate(control_policy = case_when(control_policy == 0 ~ "end social distancing",
                                    control_policy == 1 ~ "continue social distancing",
                                    control_policy == 2 ~ "use surfing policy"))

# plot total infected and hospitalized
sample_ids <- base::sample(seq_along(1:n_sim), 10)
sim_df  %>%
  #filter(.id %in% sample_ids) %>%
  ggplot() +
  #geom_point(aes(x = day, y = 0, col = as.factor(policy)), size = 1) +
  geom_point(aes(x = day, y = 0, col = as.factor(thresh_crossed)), shape = 22, size = 1, alpha = 0.5) + # show surf policy at bottom
  scale_color_manual(values = c(NA, "black")) +
  # geom_point(aes(x = day, y = max(Ia + Ip + Is) + 100, col = as.factor(thresh_crossed))) + # show surf policy at top
  geom_line(aes(x = day, y = Ia + Ip + Is, group = .id), col = "purple") +
  geom_line(aes(x = day, y = H, group = .id), col = "darkgreen") +
  #ylim(0, 1000) +
  scale_y_log10() +
  facet_grid(t_social_dist ~ control_policy) +
  labs(title = "Epidemic scenarios under alternative management strategies",
       subtitle = "purple is total infected and non-hospitalized, green is hospitalized cases",
       y = "(log10) number of people",
       x = "days",
       caption = "All simulations begin with no controls for first 60 days. 
       Rows are the duration of the initial social distancing policy. 
       Columns are the policy chosen after the inital social distancing ends.") +
  theme(legend.position = "n")


# try to shade background
# code is still shitty. Vline is very slow. maybe use annotate (but would need all xmin, xmax pairs)?
sim_df  %>%
  mutate(policy = ifelse(policy == 2, NA, policy)) %>%
  # filter(.id %in% sample_ids) %>%
  filter(day <= 1000) %>%
  ggplot() +
  geom_vline(aes(xintercept = day, col = as.factor(policy)), alpha = 0.1) + # for what policy is in effect
  geom_vline(aes(xintercept = day, col = as.factor(thresh_crossed)), alpha = 0.1) + # when surfing is on or off
  geom_line(aes(x = day, y = Ia + Ip + Is, group = .id), col = "purple") +
  geom_line(aes(x = day,  y = H, group = .id), col = "darkgreen") +
  #geom_rect(aes(fill = as.factor(policy), xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf), alpha = 0.5) +
  ylim(0, 1000) +
  scale_y_log10() +
  facet_grid(t_social_dist ~ control_policy) +
  labs(title = "Epidemic scenarios under alternative management strategies",
       subtitle = "purple is total infected and non-hospitalized, green is hospitalized cases",
       y = "(log10) number of people",
       x = "days",
       caption = "All simulations begin with no controls for first 60 days. 
       Rows are the duration of the initial social distancing policy. 
       Columns are the policy chosen after the inital social distancing ends.")


# calculate total person-days locked down and total dead

# surfing policy
summary_surf <-
  sim_df %>%
  filter(control_policy == "use surfing policy") %>%
  group_by(.id, t_social_dist, control_policy) %>%
  summarize(person_days_locked = sum(thresh_crossed*(S + E + Ia + Ip + R)), # during surfing policy
            total_dead = last(D)) 

summary_surf_social_dist <-
  sim_df %>%
  filter(control_policy == "use surfing policy") %>%
  group_by(.id, t_social_dist, control_policy) %>%
  filter(day > 60, day < 60 + t_social_dist) %>%
  summarize(person_days_social_dist = sum((S + E + Ia + Ip + R))) # during init social dist

summary_surf$person_days_locked <- summary_surf$person_days_locked + summary_surf_social_dist$person_days_social_dist 

# continue social dist policy
summary_social_dist <-
  sim_df %>%
  filter(control_policy == "continue social distancing") %>%
  group_by(.id, t_social_dist, control_policy) %>%
  # hmm, think this is wrong. Should continue adding even after outbreak is over,
  # since herd immunity has not been reached, the only thing stopping a "second wave" is the social distancing (R_e < 1)
  filter(day > 60, 
         (E + Ia + Ip + Is + Im + H) > 0) %>% # timeframe: past do nothing until outbreak is over
  summarize(person_days_locked = sum((S + E + Ia + Ip + R)),
            total_dead = last(D))

# end all interventions policy
summary_nothing <-
  sim_df %>%
  filter(control_policy == "end social distancing") %>%
  group_by(.id, t_social_dist, control_policy) %>%
  filter(day > 60, day <= 60 + t_social_dist) %>%
  summarize(person_days_locked = sum((S + E + Ia + Ip + R)))

summary_nothing <-
  sim_df %>%
  filter(control_policy == "end social distancing") %>%
  group_by(.id, t_social_dist, control_policy) %>%
  summarize(total_dead = last(D)) %>%
  select(total_dead) %>%
  cbind(summary_nothing) %>%
  select(.id, t_social_dist, control_policy, person_days_locked, total_dead)
  

# plot efficient frontiers
rbind(summary_surf, summary_social_dist, summary_nothing) %>%
  ggplot(aes(x = person_days_locked, y = total_dead, 
             group = .id,
             col = control_policy,
             shape = control_policy
             #group = interaction(.id,t_nothing, t_social_dist, control_policy),
             #col = interaction(.id,t_nothing, t_social_dist, control_policy)
             # shape = col = interaction(.id,t_nothing, t_social_dist, control_policy
         )) +
  geom_point() +
  scale_x_log10() +
  facet_wrap(~t_social_dist) +
  theme(legend.position = "top") +
  ylim(0, 10000) +
  labs(title = "Trade-offs between total dead and person-days under social distancing",
       subtitle = "Person-days calculated for all non-hospitalized and non-symptomatic individuals",
       y = "total dead",
       x = "person days under social distancing",
       caption = "NOTE: Surfing policies cause the outbreak to persist beyond the simulation time,\n 
       so the total dead and person-days under social distancing are lower bounds for this policy.")

# todo
# deaths as a function of hospital capacity
# let social distancing switch off after a certain point? 
# Necessary to look at since it is ridiculous to expect 3 yrs of cont social distancing
# also need to explore assummed continued isolation of symptomatic individuals. WOuld prob stop after some point...
# run surfing policies until outbreak is over? need to start thinking about no birth-deaths assumption for long time scales...
# economic part?

# I think the results are promising but I have barely explored any parameters. I think the next two important things are to include hospital capacity (deaths should be higher when over capacity) and to look more closely at the intervention thresholds for the surfing policy. Once I've looked at those, I think we're ready to start trying to talk to economists.
# In a recent poll of economists about covid-19, this guy stated (without any evidence) that "Optimal strategy involves multiple waves of contact reduction." He ight be someone worth reaching out to.
# shit, my calculation of person-days is totally wrong.
# should stop counting after outbreak is over (continue social distancing)

# add control policy as a state variable: 0 = nothing, 1 = social dist, 2 = surfing