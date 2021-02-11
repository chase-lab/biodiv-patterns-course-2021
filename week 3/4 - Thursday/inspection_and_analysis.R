library(tidyverse)

read_rds("week 3/3 - Wednesday/results.rds")

# visual inspection and preliminary modelling of functional forms

# this should allow us to answer our question of whether the functional
# form of the different components of diversity are the same.

# richness (S)
S_GAM <- obs %>%
  ggplot() + 
  # facet_wrap(~studyID) +
  geom_point(aes(x=elevation, y= S, col= studyID)) + 
  # fit gam with a penalised cubic spline; reduces to linear model
  # if the data do not support non-linear 'wiggles'
  # the parameter k (knots) determines how wiggly the function can get...
  stat_smooth(aes(x=elevation, y= S, col= studyID),
              se = F,
              method = 'gam',
              formula = y ~ s(x, bs = 'cs', k = 4)) +
  # single relationship for across all studies
  # stat_smooth(aes(x=elevation, y= S), colour = 'black',
  #             se = F,
  #             method = 'gam',
  #             formula = y ~ s(x, bs = 'cs', k = 4)) +
  scale_y_continuous(#trans = 'log2',
                     name = 'Species richness (S)') +
  theme(legend.position = c(1,1),
        legend.justification = c(1,1))
   

# abundance (N)
N_GAM <- obs %>%
  ggplot() + 
  # facet_wrap(~studyID) +
  geom_point(aes(x=elevation, y= N, col= studyID)) + 
  # fit gam with a penalised cubic spline; reduces to linear model
  # if the data do not support non-linear 'wiggles';
  stat_smooth(aes(x=elevation, y= N, col= studyID),
              se = F,
              method = 'gam',
              formula = y ~ s(x, bs = 'cs', k = 4)) +
  # single relationship for across all studies
  # stat_smooth(aes(x=elevation, y= N), colour = 'black',
  #             se = F,
  #             method = 'gam',
  #             formula = y ~ s(x, bs = 'cs', k = 4)) +
  scale_y_continuous(name = 'Number of individuals (N)') +
  theme(legend.position = 'none')

# rarefied richness (S_n)
Sn_GAM <- obs %>%
  ggplot() + 
  # facet_wrap(~studyID) +
  geom_point(aes(x=elevation, y= S_n, col= studyID)) + 
  # fit gam with a penalised cubic spline; reduces to linear model
  # if the data do not support non-linear 'wiggles'
  stat_smooth(aes(x=elevation, y= S_n, col= studyID),
              se = F,
              method = 'gam',
              formula = y ~ s(x, bs = 'cs', k = 4)) +
  # single relationship across all studies
  # stat_smooth(aes(x=elevation, y= S_n), colour = 'black',
  #             se = F,
  #             method = 'gam',
  #             formula = y ~ s(x, bs = 'cs', k = 4)) +
  scale_y_continuous(name = expression(paste('Rarefied richness (', S[n], ')'))) +
  theme(legend.position = 'none')

# "evenness" (S_PIE)
S_PIE_GAM <- obs %>%
  ggplot() + 
  # facet_wrap(~studyID) +
  geom_point(aes(x=elevation, y= S_PIE, col= studyID)) + 
  # fit gam with a penalised cubic spline; reduces to linear model
  # if the data do not support non-linear 'wiggles'
  stat_smooth(aes(x=elevation, y= S_PIE, col= studyID),
              se = F,
              method = 'gam',
              formula = y ~ s(x, bs = 'cs', k = 4)) +
  # single relationship across all studies
  # stat_smooth(aes(x=elevation, y= S_PIE), colour = 'black',
  #             se = F,
  #             method = 'gam',
  #             formula = y ~ s(x, bs = 'cs', k = 4)) +
  scale_y_continuous(name = expression(paste('Evenness (', S[PIE], ')'))) +
  theme(legend.position = 'none')

cowplot::plot_grid(S_GAM, N_GAM,
                   Sn_GAM, S_PIE_GAM)


# Fit GAMs to each data set one at a time, and look at some diagnostic plots 
library(mgcv)
S_gam_fit_9_k3 <- gam(formula = S ~ s(elevation, k = 3), 
                 data = obs %>% 
                   filter(studyID=='9_Carvalho-Rocha_2021'))
par(mfrow=c(2,2))
gam.check(S_gam_fit_9_k3)

S_gam_fit_12_k3 <- gam(formula = S ~ s(elevation, k = 3), 
                   data = obs %>% 
                     filter(studyID=='12_Toasaa_2020'))
par(mfrow=c(2,2))
gam.check(S_gam_fit_12_k3)

S_gam_fit_15_k3 <- gam(formula = S ~ s(elevation, k = 3), 
                   data = obs %>% 
                     filter(studyID=='15_Beirao_2020'))
par(mfrow=c(2,2))
gam.check(S_gam_fit_15_k3)

S_gam_fit_17_k3 <- gam(formula = S ~ s(elevation, k = 3), 
                       data = obs %>% 
                         filter(studyID=='17_Acharya_2015'))
par(mfrow=c(2,2))
gam.check(S_gam_fit_17_k3)

S_gam_fit_7_k3 <- gam(formula = S ~ s(elevation, k = 3), 
                       data = obs %>% 
                         filter(studyID=='7_Longino_2019'))
par(mfrow=c(2,2))
gam.check(S_gam_fit_7_k3)

# repeat for N
N_gam_fit_9_k3 <- gam(formula = N ~ s(elevation, k = 3), 
                      data = obs %>% 
                        filter(studyID=='9_Carvalho-Rocha_2021'))
par(mfrow=c(2,2))
gam.check(N_gam_fit_9_k3)

N_gam_fit_12_k3 <- gam(formula = N ~ s(elevation, k = 3), 
                       data = obs %>% 
                         filter(studyID=='12_Toasaa_2020'))
par(mfrow=c(2,2))
gam.check(N_gam_fit_12_k3)

N_gam_fit_15_k3 <- gam(formula = N ~ s(elevation, k = 3), 
                       data = obs %>% 
                         filter(studyID=='15_Beirao_2020'))
par(mfrow=c(2,2))
gam.check(N_gam_fit_15_k3)

N_gam_fit_17_k3 <- gam(formula = N ~ s(elevation, k = 3), 
                       data = obs %>% 
                         filter(studyID=='17_Acharya_2015'))
par(mfrow=c(2,2))
gam.check(N_gam_fit_17_k3)

N_gam_fit_7_k3 <- gam(formula = N ~ s(elevation, k = 3), 
                      data = obs %>% 
                        filter(studyID=='7_Longino_2019'))
par(mfrow=c(2,2))
gam.check(N_gam_fit_7_k3)

# repeat for Sn
S_n_gam_fit_9_k3 <- gam(formula = S_n ~ s(elevation, k = 3), 
                      data = obs %>% 
                        filter(studyID=='9_Carvalho-Rocha_2021'))
par(mfrow=c(2,2))
gam.check(S_n_gam_fit_9_k3)

S_n_gam_fit_12_k3 <- gam(formula = S_n ~ s(elevation, k = 3), 
                       data = obs %>% 
                         filter(studyID=='12_Toasaa_2020'))
par(mfrow=c(2,2))
gam.check(S_n_gam_fit_12_k3)

S_n_gam_fit_15_k3 <- gam(formula = S_n ~ s(elevation, k = 3), 
                       data = obs %>% 
                         filter(studyID=='15_Beirao_2020'))
par(mfrow=c(2,2))
gam.check(S_n_gam_fit_15_k3)

# this is the one that changed shape most dramatically, 
# let's have a closer look at k = 3 versus k = 4
S_n_gam_fit_17_k3 <- gam(formula = S_n ~ s(elevation, k = 3), 
                       data = obs %>% 
                         filter(studyID=='17_Acharya_2015'))
par(mfrow=c(2,2))
gam.check(S_n_gam_fit_17_k3)

S_n_gam_fit_17_k4 <- gam(formula = S_n ~ s(elevation, k = 4), 
                         data = obs %>% 
                           filter(studyID=='17_Acharya_2015'))
par(mfrow=c(2,2))
gam.check(S_n_gam_fit_17_k4)

S_n_gam_fit_7_k3 <- gam(formula = S_n ~ s(elevation, k = 3), 
                      data = obs %>% 
                        filter(studyID=='7_Longino_2019'))
par(mfrow=c(2,2))
gam.check(S_n_gam_fit_7_k3)

# repeat for S_PIE
S_PIE_gam_fit_9_k3 <- gam(formula = S_PIE ~ s(elevation, k = 3), 
                        data = obs %>% 
                          filter(studyID=='9_Carvalho-Rocha_2021'))
par(mfrow=c(2,2))
gam.check(S_PIE_gam_fit_9_k3)

S_PIE_gam_fit_12_k3 <- gam(formula = S_PIE ~ s(elevation, k = 3), 
                         data = obs %>% 
                           filter(studyID=='12_Toasaa_2020'))
par(mfrow=c(2,2))
gam.check(S_PIE_gam_fit_12_k3)

S_PIE_gam_fit_15_k3 <- gam(formula = S_PIE ~ s(elevation, k = 3), 
                         data = obs %>% 
                           filter(studyID=='15_Beirao_2020'))
par(mfrow=c(2,2))
gam.check(S_PIE_gam_fit_15_k3)

S_PIE_gam_fit_17_k3 <- gam(formula = S_PIE ~ s(elevation, k = 3), 
                         data = obs %>% 
                           filter(studyID=='17_Acharya_2015'))
par(mfrow=c(2,2))
gam.check(S_PIE_gam_fit_17_k3)

S_PIE_gam_fit_7_k3 <- gam(formula = S_PIE ~ s(elevation, k = 3), 
                        data = obs %>% 
                          filter(studyID=='7_Longino_2019'))
par(mfrow=c(2,2))
gam.check(S_PIE_gam_fit_7_k3)
