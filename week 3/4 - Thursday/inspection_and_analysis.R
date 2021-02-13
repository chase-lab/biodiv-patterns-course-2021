library(tidyverse)
library(mgcv)
library(MuMIn)
library(broom)

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


# let's fit multiple models more efficiently. First we need to 'tidy' up.
# We want a row per observation, where an 'observation' will be a data set to
# which we fit a model, specifically, one row per metric for each study.
obs_nest <- bind_rows(obs %>% 
                        rename(value = S) %>% 
                        mutate(metric = 'S') %>% 
                        select(studyID, metric, value, elevation),
                      # N
                      obs %>% 
                        rename(value = N) %>% 
                        mutate(metric = 'N') %>% 
                        select(studyID, metric, value, elevation),
                      # Sn
                      obs %>% 
                        rename(value = S_n) %>% 
                        mutate(metric = 'Sn') %>% 
                        select(studyID, metric, value, elevation),
                      # S_PIE
                      obs %>% 
                        rename(value = S_PIE) %>% 
                        mutate(metric = 'S_PIE') %>% 
                        select(studyID, metric, value, elevation)) %>% 
  select(studyID, metric, value, elevation) %>% 
  group_by(studyID, metric) %>% 
  nest(data = c(value, elevation))

# we want to compare two values of the parameter k (that controls the basis
# function of the smoother)
obs_nest <- full_join(obs_nest %>% 
  expand(k = rep(3:4)),
  obs_nest)

fit_gams <- obs_nest %>% 
  group_by(studyID, metric, k) %>% 
  mutate(gam = map(data, ~gam(formula = value ~ s(elevation, k = k),
                                   method = 'REML',
                                   data = ., 
                                 # want poisson for S & N, and Gamma with log-link for Sn & S_PIE
                                   family = ifelse((metric=='S' | metric=='N'), 
                                                          'poisson', 
                                                          "Gamma(link = 'log')"))))

# calculate AICc (for model selection), residuals and fitted values (for model inspection)
fit_gams <- fit_gams %>% 
  mutate(AICc = map(gam, ~AICc(.x)),
         # residuals
         resids = map(gam, ~residuals(.x)),
         # fitted values
         fitted = map(gam, ~fitted(.x)),
         # get some statistics from the fit models
         tidy = map(gam, ~tidy(.x)))


# check p-values: this indicates whether there was a relationship between each response and elevation;
# models that were approximately flat lines (i.e., no relationship) will have large p-values
fit_gams %>% 
  unnest(tidy) %>% 
  ggplot() +
  facet_wrap(~studyID) +
  geom_point(aes(x = metric, y = p.value, colour = as.factor(k))) +
  geom_hline(yintercept = 0.05, lty = 2)
  
# plot residuals ~ elevation for the two different values of k 
fit_gams %>% 
  unnest(cols = c(data, resids)) %>%
  ggplot() +
  facet_grid(metric~studyID, scales = 'free_y') +
  geom_point(aes(x = elevation, y = resids, col = as.factor(k)),
             alpha = 0.5) +
  geom_hline(yintercept = 0, lty = 2)


# plot residuals ~ fitted for the two different values of k 
fit_gams %>% 
  unnest(cols = c(data, resids, fitted)) %>%
  ggplot() +
  facet_wrap(metric~studyID, scales = 'free', nrow = 4) +
  geom_point(aes(x = fitted, y = resids, col = as.factor(k)),
             alpha = 0.5) +
  geom_hline(yintercept = 0, lty = 2)

# plot observations ~ fitted for the two different values of k 
fit_gams %>% 
  unnest(cols = c(data, resids, fitted)) %>%
  ggplot() +
  facet_wrap(metric~studyID, scales = 'free', nrow = 4) +
  geom_point(aes(x = fitted, y = value, col = as.factor(k)),
             alpha = 0.5) +
  geom_abline(intercept = 0, slope = 1, lty = 2)


aic_selection <- fit_gams %>% 
  unnest(cols = c(AICc)) %>% 
  select(studyID, metric, k, AICc) %>% 
  group_by(studyID, metric) %>% 
  summarise(min_AICc = min(AICc),
            k = k[AICc==min_AICc],
            delta_AICc = max(AICc) - min(AICc))

best_model_filter <- aic_selection %>% 
  unite(filter, c(studyID, metric, k))

best_models <- fit_gams %>% 
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
                                                        # recall the link function. Specifying type can be
                                                        # used to back-transform:
                                                        type = 'response')),
         predicted.se = map2(.x = gam, .y = data, ~predict(.x, newdata = .y, 
                                                        # recall the link function. Specifying type can be
                                                        # used to back-transform:
                                                        type = 'response', 
                                                        # standard error of predictions
                                                        se.fit = TRUE)[['se.fit']])) %>% 
  unnest(cols = c(data, predicted, predicted.se))

# fit_gams obs dataframe for easier plotting
fit_gams_obs <- bind_rows(obs %>% 
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
  geom_point(data = fit_gams_obs %>% 
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
  # scale_y_continuous(trans = 'log2') +
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
  geom_point(data = fit_gams_obs %>% 
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
