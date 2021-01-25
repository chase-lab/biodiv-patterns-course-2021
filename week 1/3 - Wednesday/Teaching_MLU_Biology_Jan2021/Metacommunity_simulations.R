###########################################################################
# Metacommunity sumulation models #########################################
###########################################################################

# Duarte Viana
# June 2020


###########################################################################
# Part A. Metapopulation: basics of a metacommunity simulation ############
###########################################################################

# Load libraries
library(sp)
library(ggplot2)
library(dplyr)
library(tidyr)


# Build the landscape
#====================

# Grid
grid.side <- 10 # cells each side
xy <- expand.grid(0:(grid.side-1), 0:(grid.side-1))
names(xy) <- c("x","y")
gridded(xy) <- ~x+y
# extract the spatial coordinates of the grid
coords0 <- coordinates(xy)
# plot grid
plot(xy)

# Discrete sites/pathces

# Number of sites/patches (P)
P <- 100

# Random distribution of sites
x <- runif(P,0,10)
y <- runif(P,0,10)
plot(x,y,cex=3)

# Other spatial structures
xy <- spsample(SpatialPoints(coords0), P, "random") # regular, clustered, random
coords <- coordinates(xy)
plot(coords,cex=3)
text(coords[,1],coords[,2],1:P)

# Calculate distances from each patch to every other patch
dists <- as.matrix(dist(coords))


# Set species (metapopulation of 1 species)
#=========================================

# Number of species (only 1 species)
S <- 1

# Population growth rate
Nr <- 0.02

# Dispersal rate (emmigration rate)
dr <- 0.02

# Assign some kind of attribute to the species
# Set a dispersal ability (dispersal kernel; exponential decay)
exp.rate <- 5
# calculate the probability of dispersal from and to each patch
disp_mat <- exp(-exp.rate * dists)
diag(disp_mat) <- 0
prob.dists <- apply(disp_mat, 1, function(x) x / sum(x))
plot(dists, disp_mat)

# plot connectivity among patches
as.data.frame(prob.dists) %>%
  dplyr::mutate(to.patch = rownames(prob.dists)) %>%
  tidyr::gather(key = from.patch, value = dispersal, -to.patch) %>%
  dplyr::mutate(from.patch = as.numeric(as.character(from.patch)),
                to.patch = as.numeric(as.character(to.patch))) %>%
  ggplot2::ggplot(ggplot2::aes(x = from.patch, y = to.patch, fill = dispersal))+
  ggplot2::geom_tile()+
  scale_fill_viridis_c()


# Simulate dynamics 
#==================

# Distribute it on the landscape: initialise model
# 100 individuals in one site

# Set initial abundance
N <- rep(0,P)
N[sample(1:P,1)] <- 100
plot(coords,cex=3)
points(coords,cex=3,pch=16,col=grey(1-N/max(N)))


# Run model one time step (individuals on the move)

# Calculate number of births
B <- round(Nr * N)
# Recalculate population size
N <- N + B
# Calculate number of emmigrants
E <- round(dr * N)
# create immigration vector (to store number of immigrants)
I <- numeric(P)
# Calculate immigration
# loop through sites (for each site, simulate dispersal)
for(i in 1:P){
 if(E[i]>0) {
   # sample the sites that receive immigrants from site i based on the dispersal kernel
   dest <- sample(1:P, E[i], replace=TRUE, prob=prob.dists[i,])
   # Count the number of immigrants in each site
   Ii <- table(dest)
   # store the number of immigrants in each site
   I[sort(unique(dest))] <- I[sort(unique(dest))] + Ii
 }
}
# Calculate the abundance after dispersal
N <- N - E + I
N[N<0] <- 0
# plot abundance in each site (grey scale)
plot(coords,cex=3)
points(coords,cex=3,pch=16,col=grey(1-N/max(N)))


# (run the model a few times manually)


# Run the model for 100 time steps
# e.g. using a "for" loop
for(k in 1:100){
  B <- round(Nr * N)
  N <- N + B
  E <- round(dr * N)
  I <- numeric(P)
  for(i in 1:P){
    if(E[i]>0) {
      dest <- sample(1:P,E[i],replace = TRUE,prob=prob.dists[i,])
      Ii <- table(dest)
      I[sort(unique(dest))] <- I[sort(unique(dest))] + Ii
    }
  }
  N <- N - E + I
  N[N<0] <- 0
}
# plot abundance in each site (grey scale)
plot(coords,cex=3)
points(coords,cex=3,pch=16,col=grey(1-N/max(N)))




###########################################################################
# Part B. Individual-based model (SPATCOM) ################################
###########################################################################

# Install library "Rcpp"

# Load libraries
library(gstat)
library(sp)
library(raster)
library(plotrix)
library(dplyr)
library(wrswoR)
library(geiger)
library(vegan)
library(vegetarian)
library(gridExtra)

# load SPATCOM functions
setwd("~/Documents/iDiv/Teaching/Teaching_MLU_Biology_Jan2021/SPATCOM_Viana2018")
source("SPATCOM_functions.R")


# Examples of dispersal kernel and Gaussian niche shape

# Dispersal
curve(dlnorm(x,0,1),from=0,to=10,lwd=2,col="darkcyan",xlab="Dispersal distance",ylab="Probability")
curve(dlnorm(x,0,5),from=0,to=10,lwd=2,add=T,col="orange")

# Niche
par(mfrow=c(2,1),mar=c(4,4,2,2))
curve(dnorm(x,10,1),from=0,to=30,lwd=2,col="darkcyan",xlab="Environment",ylab="Probability")
curve(dnorm(x,20,1),from=0,to=30,lwd=2,add=T,col="orange")
curve(dnorm(x,10,20),from=0,to=30,lwd=2,col="darkcyan",xlab="Environment",ylab="Probability")
curve(dnorm(x,20,20),from=0,to=30,lwd=2,add=T,col="orange")

#----------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------

# Simulation

# Define initial conditions
#==========================

grid.side <- 30 # number of cells in each side of the quadrangular grid
R <- 20 # number of species (pool)
J <- 0.80 # initial proportion of occupied cells
N_env <- 1 # number of environmental variables
sp_distr <- "random"
cor_env <- 25 # variogram range parameter (spatial correlation in the environment)

# Initialize species
set.seed(49)
spi <- sp.set(grid.side=grid.side, R=R, Ji=J, N_env=N_env, evolve=FALSE, sp.distr=sp_distr)
# Initialize environment
set.seed(79)
envi <- env.set(grid.side=grid.side, R=R, Nenv=N_env, type="mosaic", cor.range=cor_env)

# Extract useful values
# Spatial coordinates of the grid cells
xy <- envi$xy
coords2 <- as.data.frame(envi$coords)
# Environmental optima of the species
opt2 <- spi$opt
# Environment values
yy.data <- as.data.frame(envi$yy.data[,1:N_env])

# plot landscape
sp.distr <- SpatialPixelsDataFrame(coordinates(xy),as.data.frame(yy.data))
spplot(sp.distr)


# Run model
#==========

# Parameters
D <- 0.01 # mortality rate
M <- 0.5 # productivity (proportion of individuals reproducing)
Z <- 10 # number of dispersed propagules per individual
kern.type <- "lnorm" 
sd.disp <- 0.7 # dispersal capacity (log SD of lognormal kernel)
sd.niche <- 10 # niche breadth (SD of normal distribution)

# metacommunity archetype (neutral model,"NM; species sorting, "SS"; mass effects, "ME")
meta.type <- "NM" 

# Run simulation
set.seed(77)
sp.ini <- spatcom(sp.i=spi, env.i=envi, D=D, M=M, Z=Z, kern.type=kern.type,
                  peak.disp=0, sd.disp=sd.disp, sd.niche=sd.niche, meta.type=meta.type, G=1000)


#----------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------

# Pattern analyses - explore results

# Crop grid to avoid edge effects

# crop the grid and data to avoid edge effects
edge <- 0

# crop
xyf <- xy[coords2$x>=edge&coords2$x<=(grid.side-edge)&coords2$y>=edge&coords2$y<=(grid.side-edge)]
sp.ini <- sp.ini[coords2$x>=edge&coords2$x<=(grid.side-edge)&coords2$y>=edge&coords2$y<=(grid.side-edge)]
yyf <- yy.data[coords2$x>=edge&coords2$x<=(grid.side-edge)&coords2$y>=edge&coords2$y<=(grid.side-edge),1]


#-----------------------------

# PATTERN METRICS

# Final picture - environment
env.distr <- SpatialPixelsDataFrame(coordinates(xyf),as.data.frame(yyf))
spplot(env.distr)

# Final picture - species
fsample.sp <- as.factor(sp.ini)
sp.distr <- SpatialPixelsDataFrame(coordinates(xyf),as.data.frame(fsample.sp))
spplot(sp.distr,col.regions=rainbow(R))

# Plot environment and species together
grid.arrange(spplot(env.distr), spplot(sp.distr), ncol=2)

# Regional (gamma) richness at the end
length(unique(sp.ini[!is.na(sp.ini)]))

# SAD
commt<-sort(table(sp.ini),decreasing=TRUE) # final
barplot(commt)


# Virtual ecologist: sample the grid
#===================================

# Transform the grid data into a species-by-sites matrix
sp.cell <- as.numeric(sp.distr@data[,1])
Sreg <- length(unique(sp.ini[!is.na(sp.ini)]))
mat.sp0 <- as.data.frame(matrix(0,nrow=length(sp.cell),ncol=Sreg))
for(i in 1:nrow(mat.sp0)) if(!is.na(sp.cell[i])) mat.sp0[i,sp.cell[i]] <- 1
# and attach it to the spatial grid object
sp.distr@data <- mat.sp0

# Stack each species as a band in a raster
tmp <- raster(sp.distr, layer = 1)
for (i in 2:Sreg) {
  tmp <- stack(tmp, raster(sp.distr, layer = i))
}

# Aggregate the raster to make coarser samples:
mat.sp <- aggregate(tmp, fact = 5, fun = sum)
plot(env.distr)
plot(as(mat.sp[[1]], 'SpatialPixels'),add=T,lwd=3)

# Make final species matrix
mat.sp <- as.data.frame(mat.sp)

# Calculate diversity metrics from samples
# alpha-diversity
d(mat.sp, lev="alpha", q=0) 
# gamma-diversity
d(mat.sp, lev="gamma", q=0) 
# beta-diversity
d(mat.sp, lev="beta", q=0) 


#------------------------------

# Inference of assembly processes

# Calculate spatial variables: PCNM (or MEM)
xy.df <- data.frame(x=coordinates(xyf)[,1],y=coordinates(xyf)[,2])
xy.pcnm0 <- pcnm(dist(xy.df))
xy.pcnm0 <- as.data.frame(xy.pcnm0$vectors[,which(xy.pcnm0$values>0)])

# Prepare data (species, environment and space)
cell.df <- as.data.frame(cbind(sp=opt2[sp.ini,],env=yyf,xy.pcnm0))
cell.df <- na.exclude(cell.df)

# Variation partitioning
vp <- varpart(cell.df$sp,cell.df$env,cell.df[,-c(1,2)])
plot(vp)

# environmental fraction (a+b)
vp$part$fract[1,3]
# spatial fraction
vp$part$indfract[3,3]


###########################################################################
# Part C. Population-based model (mcomsimr) ###############################
###########################################################################

# Code available at https://github.com/plthompson/mcomsimr
# Shiny app for a more user-friendly interface:
# https://shiney.zoology.ubc.ca/pthompson/meta_com_shiny/

# to install the library:
# devtools::install_github("plthompson/mcomsimr")

# Load libraries
library(tidyr)
library(dplyr)
library(ggplot2)
library(mcomsimr)
library(vegan)
library(vegetarian)

# Parameters
#===========

# Dispersal distance decay
disp.rate <- 0.5
plot(1:100,10^(-disp.rate*(1:100)),ylim=c(0,1))
# Niche
breadth <- 10
plot(seq(0,1,0.01),exp(-((seq(0,1,0.01)-0.5)/(2*breadth))^2),ylim=c(0,1))
# number of species
S <- 20 
# number of patches
M <- 20 

# Species interactions
intra <- 1
min_inter <- 0
max_inter <- 0


# Simulate metacommunity dynamics
#================================
?simulate_MC
# Tip: disable the plot option when doing the exercises
sim <- simulate_MC(patches=M, species=S, dispersal=0.01, kernel_exp=disp.rate, env1Scale=500,
                   env_niche_breadth=breadth, optima_spacing="even",
                   intra=intra, min_inter=min_inter, max_inter=max_inter, 
                   initialization=100, burn_in=300, timesteps=700, plot=TRUE)


# Extract data
#=============

# Species data
sp.long <- sim[["dynamics.df"]]
mat.sp0 <- sp.long %>% 
  dplyr::select(time, patch, species, N) %>% 
  arrange(time, patch, species) %>% 
  spread(key = species, value = N, fill = 0)
mat.sp <- mat.sp0[mat.sp0$time==max(mat.sp0$time),]
mat.sp <- mat.sp[,-c(1,2)]
#mat.sp <- mat.sp[,which(colSums(mat.sp)>0)]
row.names(mat.sp) <- 1:M
# Environment data
env.xy <- sim[["env.df"]]
mat.env <- env.xy[which(env.xy$time==max(env.xy$time,na.rm=T)),"env1",drop=F]
# Spatial coordinates
mat.xy <- sim[["landscape"]]

# plot landscape (grey scale for the environmental values)
plot(mat.xy$x,mat.xy$y,col=grey(mat.env[,1]),pch=16,cex=2)


# Calculate patterns
#===================

# alpha-diversity
d(mat.sp, lev="alpha", q=0) 
# gamma-diversity
d(mat.sp, lev="gamma", q=0) 
# beta-diversity
d(mat.sp, lev="beta", q=0) 

# Occupancy for each species (proportion of sites occupied)
occ <- apply(mat.sp,2,function(x) length(x[x>0])) / nrow(mat.sp)
occ

# Calculate species correlations in time and space

# in time
median.time <- c()
for(s in 1:M){
  mat.s <- mat.sp0[mat.sp0$patch==s & mat.sp0$time>500,-c(1,2)]
  cor.s <- cor(mat.s)
  diag(cor.s) <- NA
  median.time[s] <- median(cor.s,na.rm=T)
}
mean(median.time,na.rm=T)

# in space
median.space <- c()
for(t in 500:max(mat.sp0$time)){
  mat.t <- mat.sp0[mat.sp0$time==t,-c(1,2)]
  cor.t <- cor(mat.t)
  diag(cor.t) <- NA
  median.space <- c(median.space,median(cor.t,na.rm=T))
}
mean(median.space,na.rm=T)

# in space-time
cor.st <- cor(mat.sp0[mat.sp0$time>500,-c(1,2)])
median.st <- median(cor.st,na.rm=T)
mean(median.st,na.rm=T)

# plot histograms of median correlations in time and space
par(mfrow=c(2,1))
hist(median.time)
hist(median.space)


  









