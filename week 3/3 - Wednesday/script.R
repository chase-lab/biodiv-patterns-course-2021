library(googlesheets4)
library(tidyverse)

# Read in data from google sheets
link =
  "" # paste the link to sheet here.

# Observation table
obs<- read_sheet(link,
                 sheet= "Observations")
summary(obs)
# plotID seems to be true false? check what that means.


obs <- obs %>%
  group_by(studyID, siteID) %>% # grouping data by study (this is for the nesting below)
  filter(number_individuals>0) %>% # we dont need the 0's
  select(-`endangered?`, -taxon_type,- plotID) %>% # We don't need these columns at the moment
  nest(abundances= c("species_name", "number_individuals")) %>% # nesting the data inside a cell
  ungroup() # removing the grouping

obs <- obs %>%
  mutate(S = map_dbl(abundances, function(x) pull(x,"number_individuals") %>% vegan::specnumber()),
         N = map_dbl(abundances, function(x) pull(x,"number_individuals") %>% sum()),
         S_PIE = map_dbl(abundances, function(x) pull(x,"number_individuals") %>% mobr::calc_SPIE())
         )


# filter out sites with very low N
obs <- obs %>%
  filter(N>5) # this number is kind of arbitrary

# every study will be standardized to a different common N (S_n)
min_n <- obs %>%
  group_by(studyID) %>%
  summarise(min_n= min(N))

obs <- obs %>% full_join(min_n, by= "studyID")

#calculate rarefied richness
obs <- obs %>%
  mutate(S_n= map2_dbl(abundances,
                       min_n,
                       function(x,y)
                         pull(x, "number_individuals") %>%
                         mobr::rarefaction(method = "IBR", effort = y)
                       )
         )


##############
# Read in the site information

sites<- read_sheet(link,
                 sheet= "Site")

# Add latitude, longitude and elevation to our obs object

obs <- obs %>% left_join(dplyr::select(sites, elevation,latitude, longitude, siteID),
                        by= "siteID")


# plot biodiversity metrics against elevation

# richness (S)
obs %>%
ggplot(aes(x=elevation, y= S, col= studyID))+geom_point()+ facet_wrap(~studyID, scales= "free")

# abundance (N)
obs %>%
  ggplot(aes(x=elevation, y= N, col= studyID))+geom_point()+facet_wrap(~studyID, scales= "free")

# rarefied richness (S_n)
obs %>%
  ggplot(aes(x=elevation, y= S_n, col= studyID))+geom_point()+ facet_wrap(~studyID, scales= "free")

# "evenness" (S_PIE)
obs %>%
  ggplot(aes(x=elevation, y= S_PIE, col= studyID))+geom_point()+ facet_wrap(~studyID, scales= "free")

# Compare the patterns to the original studies. does it make sense? are we missing something?
# What's going on  with Toasaa?


# Draw the entire rarefaction curves.
obs<- obs %>%
  mutate(Sn_curve= map(abundances, function(x){
    x=pull(x, "number_individuals")
    out=tibble(n = 1:sum(x),
               Sn_curve = mobr::rarefaction(x, method= "IBR")
    )

    return(out)
  }

  )
  )


obs %>% unnest(Sn_curve) %>%
  ggplot(aes(n, Sn_curve, group = siteID, col = elevation))+
  geom_line()+
  facet_wrap(~studyID, scales = "free")+
  scale_color_viridis_c( )


# save our obs object with the results so you can use it for the statistics tomorrow
saveRDS(obs, "week 3/3 - Wednesday/results.rds")

