library(tidyverse)
library(mgcv)
library(MuMIn)

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


# let's do this a bit more efficiently
obs_nest <- obs %>% 
  select(studyID, S, N, S_n, S_PIE, elevation) %>% 
  group_by(studyID) %>% 
  nest(data = c(S, S_n, S_PIE, N, elevation))

fit_gams <- obs_nest %>% 
  mutate(S_gam_k3 = map(data, ~gam(formula = S ~ s(elevation, k = 3),
                                   method = 'REML',
                                   data = ., 
                                   family = 'poisson')),
         S_gam_k4 = map(data, ~gam(formula = S ~ s(elevation, k = 4),
                                   method = 'REML', 
                                   data = ., 
                                   family = 'poisson')),
         N_gam_k3 = map(data, ~gam(formula = N ~ s(elevation, k = 3),
                                   method = 'REML',
                                   data = ., 
                                   family = 'poisson')),
         N_gam_k4 = map(data, ~gam(formula = N ~ s(elevation, k = 4),
                                   method = 'REML', 
                                   data = ., 
                                   family = 'poisson')),
         Sn_gam_k3 = map(data, ~gam(formula = S_n ~ s(elevation, k = 3),
                                   method = 'REML',
                                   data = ., 
                                   family = Gamma(link = 'log'))),
         Sn_gam_k4 = map(data, ~gam(formula = S_n ~ s(elevation, k = 4),
                                   method = 'REML', 
                                   data = ., 
                                   family = Gamma(link = 'log'))),
         S_PIE_gam_k3 = map(data, ~gam(formula = S_PIE ~ s(elevation, k = 3),
                                    method = 'REML',
                                    data = ., 
                                    family = Gamma(link = 'log'))),
         S_PIE_gam_k4 = map(data, ~gam(formula = S_PIE ~ s(elevation, k = 4),
                                    method = 'REML', 
                                    data = ., 
                                    family = Gamma(link = 'log'))))
library(broom)

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
         S_PIE_k4_fitted = map(S_PIE_gam_k4, ~fitted(.x)),
         # get some statistics from the fit models
         S_k3_tidy = map(S_gam_k3, ~tidy(.x)),
         S_k4_tidy = map(S_gam_k4, ~tidy(.x)),
         N_k3_tidy = map(N_gam_k3, ~tidy(.x)),
         N_k4_tidy = map(N_gam_k4, ~tidy(.x)),
         Sn_k3_tidy = map(Sn_gam_k3, ~tidy(.x)),
         Sn_k4_tidy = map(Sn_gam_k4, ~tidy(.x)),
         S_PIE_k3_tidy = map(S_PIE_gam_k3, ~tidy(.x)),
         S_PIE_k4_tidy = map(S_PIE_gam_k4, ~tidy(.x)))

model_output$S_k3_tidy[[1]]

# warning ugliness follows
wrangle <- bind_rows(model_output %>% 
  mutate(metric = 'S',
         k = 3) %>% 
  rename(gam = S_gam_k3,
         residuals = S_k3_resids,
         fitted = S_k3_fitted, 
         aicc = S_k3_AICc,
         stats = S_k3_tidy) %>% 
    select(data, metric, k, gam, residuals, fitted, aicc, stats),
  # S, k = 4
  model_output %>% 
    mutate(metric = 'S',
           k = 4) %>% 
    rename(gam = S_gam_k4,
           residuals = S_k4_resids,
           fitted = S_k4_fitted, 
           aicc = S_k4_AICc,
           stats = S_k4_tidy) %>% 
    select(data, metric, k, gam, residuals, fitted, aicc, stats),
  # N, k = 3
  model_output %>% 
    mutate(metric = 'N',
           k = 3) %>% 
    rename(gam = N_gam_k3,
           residuals = N_k3_resids,
           fitted = N_k3_fitted, 
           aicc = N_k3_AICc,
           stats = N_k3_tidy) %>% 
    select(data, metric, k, gam, residuals, fitted, aicc, stats),
  # N, k = 4
  model_output %>% 
    mutate(metric = 'N',
           k = 4) %>% 
    rename(gam = N_gam_k4,
           residuals = N_k4_resids,
           fitted = N_k4_fitted, 
           aicc = N_k4_AICc,
           stats = N_k4_tidy) %>% 
    select(data, metric, k, gam, residuals, fitted, aicc, stats),
  # Sn, 
  model_output %>% 
    mutate(metric = 'Sn',
           k = 3) %>% 
    rename(gam = Sn_gam_k3,
           residuals = Sn_k3_resids,
           fitted = Sn_k3_fitted, 
           aicc = Sn_k3_AICc,
           stats = Sn_k3_tidy) %>% 
    select(data, metric, k, gam, residuals, fitted, aicc, stats),
  model_output %>% 
    mutate(metric = 'Sn',
           k = 4) %>% 
    rename(gam = Sn_gam_k4,
           residuals = Sn_k4_resids,
           fitted = Sn_k4_fitted, 
           aicc = Sn_k4_AICc,
           stats = Sn_k4_tidy) %>% 
    select(data, metric, k, gam, residuals, fitted, aicc, stats),
  # S_PIE
  model_output %>% 
    mutate(metric = 'S_PIE',
           k = 3) %>% 
    rename(gam = S_PIE_gam_k3,
           residuals = S_PIE_k3_resids,
           fitted = S_PIE_k3_fitted, 
           aicc = S_PIE_k3_AICc,
           stats = S_PIE_k3_tidy) %>% 
    select(data, metric, k, gam, residuals, fitted, aicc, stats),
  model_output %>% 
    mutate(metric = 'S_PIE',
           k = 4) %>% 
    rename(gam = S_PIE_gam_k4,
           residuals = S_PIE_k4_resids,
           fitted = S_PIE_k4_fitted, 
           aicc = S_PIE_k4_AICc,
           stats = S_PIE_k4_tidy) %>% 
    select(data, metric, k, gam, residuals, fitted, aicc, stats))
  
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


# generate predicted values for each metric using the best fit models
predicted_values <- best_models %>% 
  unnest(cols = c(data)) %>% 
  group_by(studyID, metric) %>% 
  summarise(elevation = seq(min(elevation), max(elevation), length.out = 50)) %>% 
  nest(data = c(`elevation`)) %>% 
  left_join(best_models %>% 
              select(studyID, metric, gam)) %>% 
  mutate(predicted = map2(.x = gam, .y = data, ~predict(.x, newdata = .y, 
                                                        # recall the link function, specifying type can be
                                                        # used to back-transform
                                                        type = 'response')),
         predicted.se = map2(.x = gam, .y = data, ~predict(.x, newdata = .y, 
                                                        # recall the link function, specifying type can be
                                                        # used to back-transform
                                                        type = 'response', 
                                                        # standard error of predictions
                                                        se.fit = TRUE)[['se.fit']])) %>% 
  unnest(cols = c(data, predicted, predicted.se))

# wrangle obs dataframe for easier plotting
wrangle_obs <- bind_rows(obs %>% 
                           rename(value = S) %>% 
                           select(studyID, value, elevation) %>% 
                           mutate(metric = 'S'),
                         obs %>% 
                           rename(value = N) %>% 
                           select(studyID, value, elevation) %>% 
                           mutate(metric = 'N'),
                         obs %>% 
                           rename(value = S_n) %>% 
                           select(studyID, value, elevation) %>% 
                           mutate(metric = 'Sn'),
                         obs %>% 
                           rename(value = S_PIE) %>% 
                           select(studyID, value, elevation) %>% 
                           mutate(metric = 'S_PIE'))

ggplot() +
  # we can clean up the labels for the facets by using the factor function
  facet_wrap(~factor(metric,
                     levels = c('N', 'S', 'Sn', 'S_PIE'),
                     labels = c('Numbers of individuals (N)',
                                'Species richness (S)',
                                'Rarefied richness (Sn)',
                                'Evenness (S_PIE)')),
                     scales = 'free_y') +
  geom_point(data = wrangle_obs %>% 
             # optional: remove the odd values of S_PIE,
               filter(!(studyID=='12_Toasaa_2020' & metric=='S_PIE')),
             aes(x = elevation, y = value, colour = studyID)) +
  geom_line(data = predicted_values %>% 
              # optional: remove the odd values of S_PIE,
              filter(!(studyID=='12_Toasaa_2020' & metric=='S_PIE')),
            aes(x = elevation, y = predicted, colour = studyID),
            size = 1.5) +
  # add estiamte of uncertainty for predictions
  geom_ribbon(data = predicted_values %>% 
                # optional: remove the odd values of S_PIE,
                filter(!(studyID=='12_Toasaa_2020' & metric=='S_PIE')),
              aes(x = elevation, ymin = predicted - 2*predicted.se, ymax = predicted + 2*predicted.se,
                  fill = studyID),
              alpha = 0.3) +
  scale_colour_manual(values = study_col) +
  scale_fill_manual(values = study_col) + 
  theme_minimal() +
  theme(legend.position = c(1,1),
        legend.justification = c(1,1),
        strip.text = element_text(hjust = 0))
  
# what about looking at all metrics for each study together?
# looks like the model for the total abundance data for the 15_Beirao_2020 is 'overfit'??
ggplot() +
  # we can clean up the labels for the facets by using the factor function
  facet_wrap(~ studyID,
             scales = 'free_y') +
  geom_point(data = wrangle_obs %>% 
               # optional: remove the odd values of S_PIE,
               filter(!(studyID=='12_Toasaa_2020' & metric=='S_PIE')),
             aes(x = elevation, y = value, colour = metric)) +
  geom_line(data = predicted_values %>% 
              # optional: remove the odd values of S_PIE,
              filter(!(studyID=='12_Toasaa_2020' & metric=='S_PIE')),
            aes(x = elevation, y = predicted, colour = metric),
            size = 1.5) +
  # add estiamte of uncertainty for predictions
  geom_ribbon(data = predicted_values %>% 
                # optional: remove the odd values of S_PIE,
                filter(!(studyID=='12_Toasaa_2020' & metric=='S_PIE')),
              aes(x = elevation, ymin = predicted - 2*predicted.se, ymax = predicted + 2*predicted.se,
                  fill = metric),
              alpha = 0.3) +
  # scale_colour_manual(values = study_col) +
  # scale_fill_manual(values = study_col) + 
  # log-yaxis for clarity
  scale_y_continuous(trans = 'log2') +
  theme_minimal() +
  theme(legend.position = c(1,0),
        legend.justification = c(1,0),
        legend.direction = 'horizontal',
        strip.text = element_text(hjust = 0))


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
