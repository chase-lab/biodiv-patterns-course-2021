rm(list = ls())
# install.packages("FD")
# install.packages("hypervolume")
# install.packages("BAT")
# install.packages("missMDA")
# install.packages("FactoMineR")
# install.packages(GGally)
# install.packages(ggplot2)
# install.packages(dplyr)

library(FD)
library(hypervolume)
library(BAT)
library(missMDA)
library(FactoMineR)
library(factoextra)
library(GGally)
library(ggplot2)
library(dplyr)

#_________________________________________________________
# 1. Handling data
#_________________________________________________________

# Notes:
# The tussock contains data on 16 functional traits measured on 53 vascular plant species 
# from New Zealand short-tussock grasslands.
# It contains the relative abundances (percent cover) of these 53 species from 30 (8x50-m) plots

tussock <- tussock 

trait <- data.frame(tussock$trait)
trait <- trait[, c(
          "leafN" ,              # (mg g^-1)     
          "leafsize" ,           # (mm^2)
          "seedmass" ,           # (mg)
          "height"   ,           # (m)   
          "SLA"      ,           # (m^2 kg^-1) SSD is almost the opposite to SLA
          "LDMC"   )]            # (mg g^-1)
              
# Check for missing values?

anyNA(trait)
View(trait)

# Deal with missing values - Trait imputation

nb_dim  <- estim_ncpPCA(trait, ncp.min=3, ncp.max=5, 
                        scale= T, method.cv="loo",nbsim=9, verbose=FALSE) #Estimate the number of dimensions for the Principal Component Analysis by cross-validation
trait_imp <- missMDA::imputePCA(trait, ncp=nb_dim$ncp) # Imputes missing values based on PCA

trait_c   <- trait_imp$completeObs
trait_c   <- as.data.frame(trait_c)

#_________________________________________________________
# 2. Explore the species and their traits
#_________________________________________________________

# Univariate analysis & correlations

summary(trait_c) # A4 area is about 623 cm^2. largest leaf we have is 83 cm^2

GGally::ggpairs(data=trait_c, columns=1:6,
                   upper = list(continuous = wrap("cor"), 
                                mapping= aes()), 
                   diag = list(continuous = wrap("densityDiag")),
                   lower = list(continuous = wrap("points", size=0.8), 
                                mapping= aes())) 

# Multivariate analysis

PCA         <- prcomp(trait_c, scale = TRUE)
PCAvalues   <- data.frame(PCA$x) # Extract PC axes for plotting
PCAloadings <- data.frame(Variables = rownames(PCA$rotation), PCA$rotation) # Extract loadings of the variables

summary(PCA)

# A nice PCA plot 
PCA_plot         <- FactoMineR::PCA(trait_c, scale.unit=T)
factoextra::fviz_pca_biplot(PCA_plot,  geom = "text", col.var = "deepskyblue3")

#_________________________________________________________
# 3. Lets measure functional diversity using multiple traits
#_________________________________________________________
#### Practice II

#___________________________________________ESTIMATION OF FUNTIONAL DIVERSITY METRICS

# FD package
# dbFD function gives you three FD indices (Villéger et al. 2008):
# 1. Functional richness (FRic) 2. functional evenness (FEve) 3. functional divergence (FDiv).
# Also functional dispersion (FDis) (Laliberté and Legendre 2010). 

results_FD <- dbFD(x = trait_c,# (mg g^-1) related to life span and max plant height
                a = tussock$abun, w.abun = TRUE, stand.x = TRUE,
                corr = "lingoes", m = "max", stand.FRic = FALSE, print.pco = TRUE)

# Extract the functional diversity metrics
re_Fric <- results_FD$FRic
re_Fdis <- results_FD$FDis
re_Feve <- results_FD$FEve

# Hypervolume & BAT package
# This analysis may take about an hour to run.
# You can load the results of the analysis in line 109

re_HVric <- kernel.alpha(comm = tussock$abun,  trait = trait_c,
             method = "gaussian", abund = TRUE, return.hv = F)

re_HVdis <- kernel.dispersion(comm = tussock$abun, trait = trait_c) # A vector of dispersion values for each site.

re_HVeve <- kernel.evenness(comm = tussock$abun, trait = trait_c) # A vector of evenness values for each site.

# save(re_HVric, re_HVdis, re_HVeve, file = "C:/Users/iq37aceh/Desktop/Lecture/HV_results.RData")

load("C:/Users/iq37aceh/Desktop/Lecture/HV_results.RData")

# Plot differen metrics. Do we see differences?

par(mfrow=c(3,2))
plot(re_Fric,     col = "darkgoldenrod2", type = "h", lwd = 3, xlab= "", main = "Convex Hull")
plot(re_HVric,  col = "deepskyblue3", type = "h", lwd = 3, xlab= "", main = "Hypervolume")

plot(re_Fdis, col = "darkgoldenrod2", type = "h", lwd = 3, xlab= "")
plot(re_HVdis, col = "deepskyblue3", type = "h", lwd = 3, xlab= "")

plot(re_Feve, col = "darkgoldenrod2", type = "h", lwd = 3, xlab= "Site")
plot(re_HVeve, col = "deepskyblue3",type = "h", lwd = 3, xlab= "Site")

