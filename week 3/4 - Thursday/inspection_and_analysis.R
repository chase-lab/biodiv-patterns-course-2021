library(tidyverse)

read_rds("week 3/3 - Wednesday/results.rds")

# set some colours for each study
study_col <- c('9_Carvalho-Rocha_2021' = '#43285c',
               '12_Toasaa_2020' = '#732d68',
               '15_Beirao_2020' = '#a23269',
               '17_Acharya_2015' = '#cc3f60',
               '7_Longino_2019' = '#eb584d',
               '16_Veintimilla_2019' = '#fd7d33',
               '18_Highland_2013' = '#ffa600')

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
              method.args = list(family = 'poisson'),
              formula = y ~ s(x, bs = 'cs', k = 3)) +
  # single relationship for across all studies
  # stat_smooth(aes(x=elevation, y= S), colour = 'black',
  #             se = F,
  #             method = 'gam',
  #             formula = y ~ s(x, bs = 'cs', k = 4)) +
  scale_y_continuous(#trans = 'log2',
                     name = 'Species richness (S)') +
  scale_colour_manual(values = study_col) + 
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
              method.args = list(family = 'poisson'),
              formula = y ~ s(x, bs = 'cs', k = 3)) +
  # single relationship for across all studies
  # stat_smooth(aes(x=elevation, y= N), colour = 'black',
  #             se = F,
  #             method = 'gam',
  #             formula = y ~ s(x, bs = 'cs', k = 4)) +
  scale_y_continuous(name = 'Number of individuals (N)') +
  scale_colour_manual(values = study_col) +
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
              method.args = list(family = Gamma(link = 'log')),
              formula = y ~ s(x, bs = 'cs', k = 3)) +
  # single relationship across all studies
  # stat_smooth(aes(x=elevation, y= S_n), colour = 'black',
  #             se = F,
  #             method = 'gam',
  #             formula = y ~ s(x, bs = 'cs', k = 4)) +
  scale_y_continuous(name = expression(paste('Rarefied richness (', S[n], ')'))) +
  scale_colour_manual(values = study_col) +
  theme(legend.position = 'none')

# "evenness" (S_PIE)
S_PIE_GAM <- obs %>%
  filter(studyID!='12_Toasaa_2020') %>% 
  ggplot() + 
  # facet_wrap(~studyID) +
  geom_point(aes(x=elevation, y= S_PIE, col= studyID)) + 
  # fit gam with a penalised cubic spline; reduces to linear model
  # if the data do not support non-linear 'wiggles'
  stat_smooth(aes(x=elevation, y= S_PIE, col= studyID),
              se = F,
              method = 'gam',
              method.args = list(family = Gamma(link = 'log')),
              formula = y ~ s(x, bs = 'cs', k = 3)) +
  # single relationship across all studies
  # stat_smooth(aes(x=elevation, y= S_PIE), colour = 'black',
  #             se = F,
  #             method = 'gam',
  #             formula = y ~ s(x, bs = 'cs', k = 4)) +
  scale_y_continuous(name = expression(paste('Evenness (', S[PIE], ')'))) +
  scale_colour_manual(values = study_col) +
  theme(legend.position = 'none')

cowplot::plot_grid(S_GAM, N_GAM,
                   Sn_GAM, S_PIE_GAM)

ggsave('~/Dropbox/4teaching/biodiv-patterns-course-2021/week 3/4 - Thursday/elevation_gradient_results.png', 
       width = 200, height = 200, units = 'mm')
# Fit GAMs to each data set one at a time, and look at some diagnostic plots 
library(mgcv)
S_gam_fit_9_k3 <- gam(formula = S ~ s(elevation, k = 3),
                      method = 'REML',
                      family = 'poisson',
                      data = obs %>% 
                        filter(studyID=='9_Carvalho-Rocha_2021'))

par(mfrow=c(2,2))
gam.check(S_gam_fit_9_k3)
# increase the dimension of the basis by one (more wiggle)
S_gam_fit_9_k4 <- gam(formula = S ~ s(elevation, k = 4),
                      method = 'REML',
                      family = 'poisson',
                      data = obs %>% 
                        filter(studyID=='9_Carvalho-Rocha_2021'))
summary(S_gam_fit_9_k4)
# lower values of AICc suggest better model
MuMIn::AICc(S_gam_fit_9_k3, S_gam_fit_9_k4)

library(MuMIn)
# let's do this a bit more efficiently
obs_nest <- obs %>% 
  select(-siteID, -site_name, -abundances, -min_n, -latitude, -longitude) %>% 
  group_by(studyID) %>% 
  nest(data = c(S, S_n, S_PIE, N, elevation))

fit_gams <- obs_nest %>% 
  mutate(S_gam_k3 = map(data, ~gam(formula = .x$S ~ s(.x$elevation, k = 3),
                                   method = 'REML',
                                   family = 'poisson')),
         S_gam_k4 = map(data, ~gam(formula = .x$S ~ s(.x$elevation, k = 4),
                                   method = 'REML', 
                                   family = 'poisson')),
         N_gam_k3 = map(data, ~gam(formula = .x$N ~ s(.x$elevation, k = 3),
                                   method = 'REML',
                                   family = 'poisson')),
         N_gam_k4 = map(data, ~gam(formula = .x$N ~ s(.x$elevation, k = 4),
                                   method = 'REML', 
                                   family = 'poisson')),
         Sn_gam_k3 = map(data, ~gam(formula = .x$S_n ~ s(.x$elevation, k = 3),
                                   method = 'REML',
                                   family = Gamma(link = 'log'))),
         Sn_gam_k4 = map(data, ~gam(formula = .x$S_n ~ s(.x$elevation, k = 4),
                                   method = 'REML', 
                                   family = Gamma(link = 'log'))),
         S_PIE_gam_k3 = map(data, ~gam(formula = .x$S_PIE ~ s(.x$elevation, k = 3),
                                    method = 'REML',
                                    family = Gamma(link = 'log'))),
         S_PIE_gam_k4 = map(data, ~gam(formula = .x$S_PIE ~ s(.x$elevation, k = 4),
                                    method = 'REML', 
                                    family = Gamma(link = 'log'))))

model_output <- fit_gams %>% 
  mutate(S_k3_AICc = map(S_gam_k3, ~AICc(.x)),
         S_k4_AICc = map(S_gam_k4, ~AICc(.x)),
         N_k3_AICc = map(N_gam_k3, ~AICc(.x)),
         N_k4_AICc = map(N_gam_k4, ~AICc(.x)),
         Sn_k3_AICc = map(Sn_gam_k3, ~AICc(.x)),
         Sn_k4_AICc = map(Sn_gam_k4, ~AICc(.x)),
         S_PIE_k3_AICc = map(S_PIE_gam_k3, ~AICc(.x)),
         S_PIE_k4_AICc = map(S_PIE_gam_k4, ~AICc(.x)),
         # residuals
         S_k3_resids = map(S_gam_k3, ~residuals(.x)),
         S_k4_resids = map(S_gam_k4, ~residuals(.x)),
         N_k3_resids = map(N_gam_k3, ~residuals(.x)),
         N_k4_resids = map(N_gam_k4, ~residuals(.x)),
         Sn_k3_resids = map(Sn_gam_k3, ~residuals(.x)),
         Sn_k4_resids = map(Sn_gam_k4, ~residuals(.x)),
         S_PIE_k3_resids = map(S_PIE_gam_k3, ~residuals(.x)),
         S_PIE_k4_resids = map(S_PIE_gam_k4, ~residuals(.x)),
         # fitted values
         S_k3_fitted = map(S_gam_k3, ~fitted(.x)),
         S_k4_fitted = map(S_gam_k4, ~fitted(.x)),
         N_k3_fitted = map(N_gam_k3, ~fitted(.x)),
         N_k4_fitted = map(N_gam_k4, ~fitted(.x)),
         Sn_k3_fitted = map(Sn_gam_k3, ~fitted(.x)),
         Sn_k4_fitted = map(Sn_gam_k4, ~fitted(.x)),
         S_PIE_k3_fitted = map(S_PIE_gam_k3, ~fitted(.x)),
         S_PIE_k4_fitted = map(S_PIE_gam_k4, ~fitted(.x)))

# warning ugliness follows
wrangle <- bind_rows(model_output %>% 
  mutate(metric = 'S',
         k = 3) %>% 
  rename(gam = S_gam_k3,
         residuals = S_k3_resids,
         fitted = S_k3_fitted, 
         aicc = S_k3_AICc) %>% 
  select(data, metric, k, gam, residuals, fitted, aicc),
  # S, k = 4
  model_output %>% 
    mutate(metric = 'S',
           k = 4) %>% 
    rename(gam = S_gam_k4,
           residuals = S_k4_resids,
           fitted = S_k4_fitted, 
           aicc = S_k4_AICc) %>% 
    select(data, metric, k, gam, residuals, fitted, aicc),
  # N, k = 3
  model_output %>% 
    mutate(metric = 'N',
           k = 3) %>% 
    rename(gam = N_gam_k3,
           residuals = N_k3_resids,
           fitted = N_k3_fitted, 
           aicc = N_k3_AICc) %>% 
    select(data, metric, k, gam, residuals, fitted, aicc),
  # N, k = 4
  model_output %>% 
    mutate(metric = 'N',
           k = 4) %>% 
    rename(gam = N_gam_k4,
           residuals = N_k4_resids,
           fitted = N_k4_fitted, 
           aicc = N_k4_AICc) %>% 
    select(data, metric, k, gam, residuals, fitted, aicc),
  # Sn, 
  model_output %>% 
    mutate(metric = 'Sn',
           k = 3) %>% 
    rename(gam = Sn_gam_k3,
           residuals = Sn_k3_resids,
           fitted = Sn_k3_fitted, 
           aicc = Sn_k3_AICc) %>% 
    select(data, metric, k, gam, residuals, fitted, aicc),
  model_output %>% 
    mutate(metric = 'Sn',
           k = 4) %>% 
    rename(gam = Sn_gam_k4,
           residuals = Sn_k4_resids,
           fitted = Sn_k4_fitted, 
           aicc = Sn_k4_AICc) %>% 
    select(data, metric, k, gam, residuals, fitted, aicc),
  # S_PIE
  model_output %>% 
    mutate(metric = 'S_PIE',
           k = 3) %>% 
    rename(gam = S_PIE_gam_k3,
           residuals = S_PIE_k3_resids,
           fitted = S_PIE_k3_fitted, 
           aicc = S_PIE_k3_AICc) %>% 
    select(data, metric, k, gam, residuals, fitted, aicc),
  model_output %>% 
    mutate(metric = 'S_PIE',
           k = 4) %>% 
    rename(gam = S_PIE_gam_k4,
           residuals = S_PIE_k4_resids,
           fitted = S_PIE_k4_fitted, 
           aicc = S_PIE_k4_AICc) %>% 
    select(data, metric, k, gam, residuals, fitted, aicc))
  
# plot residuals ~ elevation for the two different values of k 
wrangle %>% 
  unnest(cols = c(data, residuals)) %>%
  ggplot() +
  facet_grid(metric~studyID, scales = 'free_y') +
  geom_point(aes(x = elevation, y = residuals, col = as.factor(k)),
             alpha = 0.5) +
  geom_hline(yintercept = 0, lty = 2)


# plot residuals ~ fitted for the two different values of k 
wrangle %>% 
  unnest(cols = c(data, residuals, fitted)) %>%
  ggplot() +
  facet_wrap(metric~studyID, scales = 'free', nrow = 4) +
  geom_point(aes(x = fitted, y = residuals, col = as.factor(k)),
             alpha = 0.5) +
  geom_hline(yintercept = 0, lty = 2)

# plot observations ~ fitted for the two different values of k 
wrangle %>% 
  unnest(cols = c(data, residuals, fitted)) %>%
  filter(metric=='S') %>% 
  ggplot() +
  facet_wrap(~studyID, scales = 'free', nrow = 4) +
  geom_point(aes(x = fitted, y = S, col = as.factor(k)),
             alpha = 0.5) +
  geom_abline(intercept = 0, slope = 1, lty = 2)

wrangle %>% 
  unnest(cols = c(data, residuals, fitted)) %>%
  filter(metric=='N') %>% 
  ggplot() +
  facet_wrap(~studyID, scales = 'free', nrow = 4) +
  geom_point(aes(x = fitted, y = N, col = as.factor(k)),
             alpha = 0.5) +
  geom_abline(intercept = 0, slope = 1, lty = 2)

wrangle %>% 
  unnest(cols = c(data, residuals, fitted)) %>%
  filter(metric=='Sn') %>% 
  ggplot() +
  facet_wrap(~studyID, scales = 'free', nrow = 4) +
  geom_point(aes(x = fitted, y = Sn, col = as.factor(k)),
             alpha = 0.5) +
  geom_abline(intercept = 0, slope = 1, lty = 2)

wrangle %>% 
  unnest(cols = c(data, residuals, fitted)) %>%
  filter(metric=='S_PIE') %>% 
  ggplot() +
  facet_wrap(~studyID, scales = 'free', nrow = 4) +
  geom_point(aes(x = fitted, y = S_PIE, col = as.factor(k)),
             alpha = 0.5) +
  geom_abline(intercept = 0, slope = 1, lty = 2)

aic_selection <- wrangle %>% 
  unnest(cols = c(aicc)) %>% 
  select(studyID, metric, k, aicc) %>% 
  group_by(studyID, metric) %>% 
  summarise(min_aicc = min(aicc),
            k = k[aicc==min_aicc],
            delta_aicc = max(aicc) - min(aicc))

best_model_filter <- aic_selection %>% 
  unite(filter, c(studyID, metric, k))

best_models <- wrangle %>% 
  unite(filter, c(studyID, metric, k), remove = F) %>% 
  filter(filter %in% best_model_filter$filter) %>% 
  select(-filter)

predicted_values <- best_models %>% 
  unnest(cols = c(data)) %>% 
  group_by(studyID, metric) %>% 
  summarise(elevation = seq(min(elevation), max(elevation), length.out = 50)) %>% 
  nest(data = c(elevation)) %>% 
  left_join(best_models %>% 
              select(studyID, metric, gam)) %>% 
  mutate(predicted = map(.x = gam,  ~predict(.x, newdata = elevation)))




# what about a map for the presentation...
world <- map_data('world') %>% 
  as_tibble()

ggplot() +
  geom_polygon(data=world, 
               aes(long, lat, group = group), colour=NA, fill='#CCCCCC', size=0) +
  geom_point(data = obs,
             aes(x = longitude, y = latitude, colour = studyID),#,shape = taxa),
             # alpha = 0.6,
             size = 1) +
  coord_map('mollweide', ylim = c(-60, 90), xlim = c(-180, 180)) +
  scale_x_continuous(breaks = seq(-180, 180, by = 30)) +
  scale_y_continuous(breaks = c(0, -23.5, 23.5, -60, 60)) 
