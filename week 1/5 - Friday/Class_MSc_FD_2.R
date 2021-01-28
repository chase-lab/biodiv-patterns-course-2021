#### Practice II

#___________________________________________UNDERTANDING FUNCTIONAL TRAIT SPACE

rm(list = ls())
# install.packages(ggplot2)
# install.packages(dplyr)
# install.packages(reshape2)

library(ggplot2)
library(dplyr)
library(reshape2)

load("C:/Users/iq37aceh/Desktop/Lecture/vt_traits_2.RData")

# vt_traits # traits are scaled and ready for analysis 

#______________________________________________species are grouped/marked by the vegetation types they occur 

PCA         <- prcomp(vt_traits[1:6])
PCAvalues   <- data.frame(Species   = vt_traits$spp, 
                          Coast     = vt_traits$C, 
                          ThermFor  = vt_traits$T,
                          Laurel    = vt_traits$L,
                          Pinar     = vt_traits$P,
                          Summit    = vt_traits$S,
                          Rocks     = vt_traits$R,
                          PCA$x)
PCAloadings <- data.frame(Variables = rownames(PCA$rotation), PCA$rotation) 

dat = reshape2::melt(data = PCAvalues, id.vars = c("Species", "PC1", "PC2", "PC3"), 
                   measure.vars = c("Coast", "ThermFor", "Laurel", "Pinar", "Summit", "Rocks" ))
dat = na.omit(dat)

#______________________________________________ Using a nice plot to interpret the trait combinations of two different vegetation types

# Coast (Coastal scrub), Laurel (Laurel Forest), Summit (summit scrub)
trait_space <- dplyr::filter(dat, variable == "Laurel")  # Chance here the vegetation type

# Density plot

ggplot2::ggplot(trait_space, aes(PC1,  PC2)) + 
  
  stat_density_2d(geom = "polygon", contour = TRUE, 
                  aes(fill = after_stat(level)), colour = "gray", bins = 5) +
  
  scale_fill_distiller(palette = "Greens", direction = 1) +
  geom_jitter(alpha=0.3, size =1.3, colour = "gray40") +
  
  geom_segment(data = PCAloadings, size = 0.2,
               aes(x = 0, xend = PC1*4.2, y = 0, yend = PC2*4.2),
               arrow = arrow(length = unit(0.1, "cm")),colour = "black")   +
  geom_text(data = PCAloadings, aes(x = PC1*4.4, y = PC2*4.4, label = Variables), size = 4) +
  
  ggtitle("Laurel") +   # Change the title depending on the vegetation type filtered. 
  
  scale_y_reverse () +
  scale_x_reverse () + 
  theme_minimal()



