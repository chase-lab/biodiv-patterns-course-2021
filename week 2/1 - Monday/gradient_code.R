library(tidyverse)
library(mobr)
library(vegan)
library(viridisLite)


#######################################################
###### Part 1: Trees ##################################
#######################################################

# 1 Exploring the dataset
#################################

trees <- readRDS("week 2/1 - Monday/data/trees.rds") %>% as_tibble()

# Explore the data (range of values etc.)
summary(trees)

# How many plots are there?
trees %>%
    select(plot_name) %>%
    n_distinct()

# NOW YOU: How many plots are in the northern hemisphere (use filter)?
#
#


# Draw a map of all plots
world_map<-
ggplot() +
    geom_polygon(data= map_data("world"),
                 aes(x = long, y = lat, group = group),
        fill="lightgray", colour = "white")

world_map+
    geom_point(data= distinct(select(trees,Latitude, Longitude)),
               aes(x=Longitude, y= Latitude))

# What are the coordinates of the plot "COLORADO"?
trees %>%
    filter(plot_name == "COLORADO") %>%
    select(Latitude, Longitude) %>%
    distinct()

# NOW YOU: What are the coordinates of the plot "MTSTHILA"?
#
#
#

# Which species occur in plot the COLORADO and how many (species column: "scrubbed_species_binomial")?

trees %>%
    filter(plot_name == "COLORADO") %>%
    select(scrubbed_species_binomial) %>%
    distinct()

# What are the species counts in the subplots of the plot "COLORADO"?
trees %>%
    filter(plot_name == "COLORADO") %>%
    select(subplot,scrubbed_species_binomial, individual_count) %>%
    arrange(subplot) %>%
    View()

# We don't care about the subplots now.
# Add the species counts from all subplots of "COLORADO" together and save them as a vector.

COLORADO_abu<- trees %>%
    filter(plot_name == "COLORADO") %>%
    group_by(scrubbed_species_binomial) %>%
    summarise(Total = sum(individual_count)) %>%
    pull(Total)

# calculate and plot the rarefaction curve of COLORADO
COLORADO_IBR <- tibble(
    n=1:sum(COLORADO_abu),
    Sn= rarefaction(COLORADO_abu, method= "IBR"),
    plot="COLORADO"
)

COLORADO_IBR %>%
    ggplot(aes(x=n, y=Sn))+
    geom_line()


# NOW YOU: repeat the last two tasks for the plot "MTSTHILA".
#
#
#
#


# Plot both curves in the same graph

COLORADO_IBR %>%
    full_join(MTSTHILA_IBR) %>%
    ggplot(aes(x=n, y=Sn, col= plot))+
    geom_line()

# NOW YOU: Describe how species diversity changes between the two plots.
#
#



#  Analyzing the data set
#####################################

# Let's drop some of the columns and reformat the data

dat <-
    trees %>%
    group_by(plot_name, scrubbed_species_binomial) %>%
    summarise(total= sum(individual_count),
              Latitude= first(Latitude),
              Longitude= first(Longitude)) %>%
    nest(abundances=c(scrubbed_species_binomial, total)) %>%
    mutate(abundances = map(abundances, function(x) {
        t(as.matrix(column_to_rownames(x, "scrubbed_species_binomial")))
    }))


# calculate biodiversity metrics

## number of individuals
dat <-
    dat %>%
    mutate(
        N= map_dbl(abundances, sum)
    )

# Richness
dat <-
    dat %>%
    mutate(
        S= map_dbl(abundances, function(x) length(x[x>0]))
    )


# NOW YOU: Exclude data sets with fewer than 20 trees (We want forests!)
#
#
#


# Calculate Rarefied richness (Sn) for n=20 and SPIE

dat <-
    dat %>%
    mutate(
        Sn = map_dbl(abundances,rarefaction, method= "IBR", effort= 20),
        SPIE = map_dbl(abundances,calc_SPIE)
    )


# Next we'll plot the metrics against absolute latitude. We'll also add a smooth line.
# The smooth line is a generalized additive model (GAM). Ideally, we would fit
# different types of models and inspect and compare them but that is not the focus of this tutorial.
# We'll just use this one as a visual aid.

dat <-
    dat %>%
    mutate(Absolute_Latitude = abs(Latitude))
dat %>%
    ggplot(aes(Absolute_Latitude, S)) +
    geom_point() +
    geom_smooth(method = "gam",method.args = list(family = "Gamma", method = "REML"))

dat %>% ggplot(aes(Absolute_Latitude, N)) +
    geom_point()+
    geom_smooth(method = "gam",method.args = list(family = "Gamma", method = "REML"))
dat %>% ggplot(aes(Absolute_Latitude, SPIE)) +
    geom_point() +
    geom_smooth(method = "gam",method.args = list(family = "Gamma", method = "REML"))

dat %>% ggplot(aes(Absolute_Latitude, Sn)) +
    geom_point() +
    geom_smooth(method = "gam",method.args = list(family = "Gamma", method = "REML"))

# How would you characterize the richness gradient in terms of its components (SAD and N)?


# Lets look at all rarefaction curves and plot them by latitude
dat <- dat %>%
    mutate(Sn_curve= map(abundances, function(x)
        tibble(n = 1:sum(x),
               Sn_curve = rarefaction(x, method= "IBR")
               )
    )
    )

dat %>% unnest(Sn_curve) %>%
    ggplot(aes(n, Sn_curve, group = plot_name, col = Absolute_Latitude))+
    geom_line()+scale_color_viridis_c()


#######################################################
###### Part 2: Fish ##################################
#######################################################

# 1 Exploring the dataset
#################################

fish <- readRDS("week 2/1 - Monday/data/fish.rds") %>% as_tibble()

# NOW YOU:
# 1. What is the latitudinal range of this data set?
# 2. How many distinct "surveys" (i.e. plots) does it contain?
# 3. Which columns contain the species names and abundances?
# 4. Draw a map of the survey locations.

summary(fish)

fish %>% select(SurveyID) %>% n_distinct()

world_map+
    geom_point(data= distinct(select(fish,Latitude, Longitude)),
               aes(x=Longitude, y= Latitude))



# 2 Analysis
####################################

# Reformat the data (code below)
dat2 <-
    fish %>%
    group_by(SurveyID, Taxon) %>%
    summarise(total= sum(Total),
              Latitude= first(Latitude),
              Longitude= first(Longitude)) %>%
    nest(abundances=c(Taxon, total)) %>%
    mutate(abundances = map(abundances, function(x) {
        t(as.matrix(column_to_rownames(x, "Taxon")))
    }))

# NOW YOU
# 1. Calculate total Abundance (N) per survey.
# 2. Exclude surveys with fewer than 50 individuals
# 3. Calculate observed richness (S), rarefied richness (Sn) and "evenness" (SPIE).
# 4. Make a new column "Absolute_Latitude"
# 5. Look at the latitudinal patterns of all metrics.
# 6. How would you characterize the richness gradient in terms of its components (SAD and N)?


##############################################################
######### Part 3: Synthesis ##################################
##############################################################

# 1. What are the similarities and differences between the patterns of trees and fish?
# 2. How would you explain the differences with biology?
