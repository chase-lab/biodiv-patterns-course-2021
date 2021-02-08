#####################################################
# Extract covariates in R

#Set working directory
setwd("F:/iDiv/MLU Biodiversity_2021/Metadata")

#install and load packages
packages <- c("raster","sp","rgdal")
package.check <- lapply(packages,FUN=function(x)
  {
    if(!require(x,character.only=TRUE)){
    install.packages(x,dependencies=TRUE)
      library(x,character.only=TRUE)
    }
  }
)

#############################
## 1. We will read a raster of mean annual temperature and check its attributes; and then plot the raster; and crop the raster to your focused region

# Read a raster and check its attributes
mat = raster("Data/MAT.tif")
mat
ncol(mat)
ymin(mat)
xres(mat)

# Plot the raster
plot(mat,col=rainbow(100,start=0.2,end=0.8))

## Crop the raster and plot the subset
mat.sub <- crop(mat,extent(-10,40,35,70))
plot(mat.sub,col=rainbow(100,start=0.2,end=0.8))


#########################
## 2. Download climatic data from WorldClim (https://www.worldclim.org/); and prepare the coordinates of your sites

# Get bioclimatic data from Worldclim. Here we get the data at the resolution at 2.5 minutes; and save it in the folder "Data"
bio <- getData("worldclim",var="bio",res=2.5,path="Data")

# You can read the climatic raster from youe folder if you have download them. 
dir.bio <-paste0("Data/wc2-5/bio",1:19,".bil",sep="")
bio <- stack(dir.bio)


# Generate 10 random coordinates for exercise. The first column is "siteID", and the second and third column is "longitude" and latitude.
# You can read the coordinates for your sites to replace the random one.
set.seed(seed=1)
id <- sample(which(!is.na(getValues(bio[[1]]))),10)
sites <- xyFromCell(bio[[1]],id)
sites <- data.frame("siteID"=1:nrow(sites),sites)

# Check the positions for your sites 
plot(bio[[1]])
points(sites[,2:3],pch=16)



#######################
## Extract data from raster and also shapefiles

## Extract bioclimateic variables for sites
bio.sites <- extract(bio,sites[,2:3],df=TRUE)
# For the temperature variables from worldclim, the raw values was multplied by 10. Now, we transform it back to make the unit as degrees.
bio.sites[,c(2:3,5:12)] <- bio.sites[,c(2:3,5:12)]/10


## Extract human footprint at 1993 and 2009 and then calculate the change between these two periods.
# The human pressure is measured using eight variables including built-up environments, population density, 
# electric power infrastructure, crop lands, pasture lands, roads, railways, and navigable waterways. The maximum value is 50.
# The data was download from: https://sedac.ciesin.columbia.edu/data/set/wildareas-v3-2009-human-footprint.
 
# Read the raster
hfp1993 <- raster("Data/Human_footprint/wildareas-v3-1993-human-footprint.tif")
hfp2009 <- raster("Data/Human_footprint/wildareas-v3-2009-human-footprint.tif")

# As the data of human footpint is projected and its coordinates is not longitude and latitue, here we project the 
# coordinates of sites to the projection of footprint.
sites_proj.hfp <- project(as.matrix(sites[,2:3]),proj=projection(hfp1993))

# Extract human foortpint
hfp.sites <- data.frame("siteID"=sites[,1],"hfp1993"=NA,"hfp2009"=NA,"hfp.change"=NA)
hfp.sites[,2] <- extract(hfp1993,sites_proj.hfp)
hfp.sites[,3] <- extract(hfp2009,sites_proj.hfp)
##Due to the maximum possible value of footprint is 50, we replace those grater than 50 with NA
hfp.sites[hfp.sites[,2]>50,2] <- NA
hfp.sites[hfp.sites[,3]>50,3] <- NA
#Change of human footprint duing 1993-2009
hfp.sites[,4] <- hfp.sites[,3]-hfp.sites[,2]


##Extract human population density at 1970 and 2000 and then calculate the change between these two periods.
# The data was download from: https://sedac.ciesin.columbia.edu/data/set/popdynamics-global-pop-density-time-series-estimates
# read raster
hpd1970 <- raster("Data/Population_density/global-pop-density-time-series-estimates_1970.tif")
hpd2000 <- raster("Data/Population_density/global-pop-density-time-series-estimates_2000.tif")

# Extract human foortpint
hpd.sites <- data.frame("siteID"=sites[,1],"hpd1970"=NA,"hpd2000"=NA,"hpd.change"=NA)
hpd.sites[,2] <- extract(hpd1970,sites[,2:3])
hpd.sites[,3] <- extract(hpd2000,sites[,2:3])
hpd.sites[,4] <- hpd.sites[,3]-hpd.sites[,2]



##Extract the types of ecoregion, biome and realm for each site.
# The data is a shapefile and was download from: https://ecoregions2017.appspot.com/

# Read the shapefile of ecoregion
ecoregion <- readOGR("Data/ecoregions2017/Ecoregions2017","Ecoregions2017")

# Generate a SpatialPointsDataFrame for our sites for extraction of attributes from a shapefile
sites.shape <- sites
coordinates(sites.shape) = ~x+y
projection(sites.shape) <- projection(ecoregion)
plot(sites.shape,axes=T)

# Extract the types of ecoregion, biome and realm
ecoregion.sites <- over(sites.shape,ecoregion)
ecoregion.sites <- data.frame("siteID"=sites[,1],ecoregion.sites[,c(2,4,5)])


##Finally, you can combine all variables you extracted
env.sites <- data.frame(bio.sites,hfp.sites[,-1],hpd.sites[,-1],ecoregion.sites[,-1])


