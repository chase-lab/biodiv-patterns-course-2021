# demo code for 'experimental' data
rm(list = ls())
library(mobsim)

# the 'development' branch of mobr has some new functions that
# I want to use
# so first, go into development mode (this prevents main version being overwritten)
devtools::dev_mode(TRUE)
devtools::install_github('MoBiodiv/mobr', ref = 'dev', force = TRUE)
devtools::dev_mode(FALSE)
library(mobr)
library(tidyverse)

# paths to files, and locations where you save any output will
# need to be modified for your environment (computer)

# load data (I can't get this working when using a github url currently, need to download to local directory first)
load('~/Dropbox/4teaching/biodiv-patterns-course-2021/week 1/4 - Thursday/data/exp1.Rdata')
load('~/Downloads/exp1.Rdata')

# calculate N, S, and S_PIE
bio_metrics <- experiment1_long %>% 
  group_by(experiment, rep, treatment) %>% 
  summarise(N = sum(n),
            S = n_distinct(species),
            S_PIE = calc_SPIE(n)) %>% 
  ungroup()

# to calculate Sn, we need to know what the minimum number of individuals is.
# Because we want to compare the treatments, we want the minimum N across both treatments
targetN <- min(bio_metrics$N)

# calculate expected richness for targetN
Sn <- experiment1_long %>% 
  mutate(targetN = targetN) %>% 
  group_by(experiment, rep, treatment) %>% 
  nest(data = c(species, n, targetN)) %>% 
  mutate(Sn = map(data, ~rarefaction(.x$n, method = 'IBR', effort = .x$targetN) %>% 
                    # retain the one unique value
                    unique)) %>% 
  ungroup()

# join with other metrics
bio_metrics <- left_join(bio_metrics,
                         Sn %>% 
                           unnest(Sn) %>% 
                           select(-data),
                         by = c('experiment', 'rep', 'treatment'))

# let's calculate the rarefaction curves first
alpha_ibr <- experiment1_long %>% 
  group_by(experiment, rep, treatment) %>% 
  nest() %>% 
  mutate(ibr = map(data, ~rarefaction(.x$n, method = 'IBR'))) %>% 
  unnest(ibr) %>% 
  mutate(individuals = 1:n()) %>% 
  ungroup()

# plot ibr curves
ggplot() +
  facet_wrap(~treatment) +
  geom_line(data = alpha_ibr,
            aes(x = individuals, y = ibr, group = rep),
            alpha = 0.5) +
  labs(y = 'Number of species',
       x = 'Number of individuals')


# boxplots of the metrics as a function of treatment
N_bxplot <- ggplot() +
  geom_boxplot(data = bio_metrics,
               aes(x = treatment, y = N))

S_bxplot <- ggplot() +
  geom_boxplot(data = bio_metrics,
               aes(x = treatment, y = S))

Sn_bxplot <- ggplot() +
  geom_boxplot(data = bio_metrics,
               aes(x = treatment, y = Sn))

S_PIE_bxplot <- ggplot() +
  geom_boxplot(data = bio_metrics,
               aes(x = treatment, y = S_PIE))


cowplot::plot_grid(N_bxplot, S_bxplot,  Sn_bxplot, S_PIE_bxplot,
                   nrow = 2)

##------all discrete scales
wide_dat <- experiment1_long %>% 
  spread(key = species, value = n)

wide_dat[is.na(wide_dat)] <- 0

# this function will calculate our chosen metrics at alpha-, beta- and gamma-scales
# need to call the function twice, once for each 'group' (here, 'treatment')
crtl1_discrete_scales <- calc_comm_div(wide_dat %>% 
                                         filter(treatment=='control') %>% 
                                         select(-experiment, -rep, -treatment),
                                      index = c('N', 'S', 'S_n', 'S_PIE'),
                                      # Sn needs an effort value, use the targetN calculated for this experiment
                                      effort = targetN, coverage = FALSE)

trt1_discrete_scales <- calc_comm_div(wide_dat %>% 
                                         filter(treatment=='treatment') %>% 
                                         select(-experiment, -rep, -treatment),
                                       index = c('N', 'S', 'S_n', 'S_PIE'),
                                       # Sn needs an effort value, use the targetN calculated for this experiment
                                       effort = targetN, coverage = FALSE)

# join back together
exp1_discrete_scales <- bind_rows(crtl1_discrete_scales %>% 
                                    mutate(treatment = 'control'),
                                  trt1_discrete_scales %>% 
                                    mutate(treatment = 'treatment')) %>% 
  as_tibble()

# check that our alpha-scale looks the same...
ggplot() +
  facet_wrap(~index, scales = 'free_y') +
  geom_boxplot(data = exp1_discrete_scales %>% 
               filter(scale=='alpha'),
             aes(x = treatment, y = value))

# gamma-scale; drop N as it scales approximately linearly with effort
ggplot() +
  facet_wrap(~index, scales = 'free_y') +
  geom_point(data = exp1_discrete_scales %>% 
                 filter(scale=='gamma' & index!='N'),
               aes(x = treatment, y = value))

# beta-scale
ggplot() +
  facet_wrap(~index, scales = 'free_y') +
  geom_point(data = exp1_discrete_scales %>% 
               filter(scale=='beta'),
             aes(x = treatment, y = value))

# gamma-scale rarefaction
gamma_ibr <- experiment1_long %>% 
  # first, gather collate species across the different reps
  group_by(experiment, treatment, species) %>% 
  summarise(n = sum(n)) %>% 
  ungroup() %>% 
  group_by(experiment, treatment) %>% 
  nest() %>% 
  mutate(ibr = map(data, ~rarefaction(.x$n, method = 'IBR'))) %>% 
  ungroup() %>% 
  unnest(ibr) %>% 
  select(-experiment) %>% 
  group_by(treatment) %>% 
  mutate(individuals = 1:n()) %>% 
  ungroup()

# plot gamma ibr curves
ggplot() +
  facet_wrap(~treatment) +
  geom_line(data = gamma_ibr,
            aes(x = individuals, y = ibr))

# sometimes it is useful to visualise the 'mean' alpha-scale curve
# get the mins for each group
minN <- bio_metrics %>% 
  group_by(treatment) %>% 
  summarise(minN = min(N))

alpha_ibr_bar <- alpha_ibr %>% 
  group_by(experiment, treatment, individuals) %>% 
  summarise(ibr_bar = mean(ibr)) %>% 
  ungroup() %>% 
  left_join(minN)

# plot ibr curves
ggplot() +
  facet_wrap(~treatment) +
  geom_line(data = alpha_ibr,
            aes(x = individuals, y = ibr, group = rep),
            alpha = 0.5) +
  geom_line(data = alpha_ibr_bar %>% 
              filter(individuals < minN),
            aes(x = individuals, y = ibr_bar), size = 2) +
  labs(y = 'Number of species',
       x = 'Number of individuals')
