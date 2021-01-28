#####################################################
# Phylogenetic diversity in R

#Set working directory
setwd("D:/Dropbox/iDiv/MLU Biodiversity_2021/phylogenetic_diversity/Practice")

#install and load package "V.PhyloMaker" to generate phylogenoes based on mega-tree
if(!require("devtools")) install.packages("devtools")
library(devtools)
if(!require("V.PhyloMaker")) devtools::install_github("jinyizju/V.PhyloMaker")
library(V.PhyloMaker)

#install and load other packages
packages <- c("vegan","ape","picante","fields")
package.check <- lapply(packages,FUN=function(x)
  {
    if(!require(x,character.only=TRUE)){
    install.packages(x,dependencies=TRUE)
      library(x,character.only=TRUE)
    }
  }
)


###############
#1.we will load the BCI data in the packag "vegan", and assemble the taxonomic names for the species in BCI, 
#and finally build a phylogeny for these species

#a. BCI dataset
data(BCI, BCI.env)

##b.Regute taxonomic informaiton for species in BCI. Both geuns and family are needed to build a phylogeny here.
#The genus are extracted from species names. The species list is then submitted to the TNRS website http://tnrs.iplantcollaborative.org/.
#The TNRS will check taxonomic information and the return result includs the family. Check the website youself. You can also use the 
#result "bci.spp.final.csv" for next steps

bci.spp <- data.frame("ID"=1:ncol(BCI),"species.raw"=colnames(BCI),"species"=NA,"genus"=NA,"family"=NA,stringsAsFactors=FALSE)
for(i in 1:nrow(bci.spp)){
  names <- unlist(strsplit(bci.spp[i,2],split=".",fixed=TRUE))
  bci.spp[i,3]<- paste(names,collapse=" ")
  bci.spp[i,4] <- names[1]
}

#Output the taxonomic informaiton to check use TNRS: http://tnrs.iplantcollaborative.org/
write.csv(bci.spp,file="bci.spp.raw.csv", row.names=FALSE)

#Input the resultant taxonomic information for next steps
bci.spp <- read.csv("bci.spp.final.csv")

#c. Generate a phylogeny for species in BCI
?phylo.maker()
bci.tree.output <- phylo.maker(bci.spp[,3:5],tree=GBOTB.extended,nodes=nodes.info.1,scenarios="S3")

#write the tree generated to a txt.
write.tree(bci.tree.output$scenario.3,file="bci.tree.txt")


############
#2. To check the attributes of the generated phylogeny, and do simple manipulations

#Read the bci phylogenetic tree
bci.tree <- read.tree("bci.tree.txt")

#a. check the attributes
class(bci.tree)
str(bci.tree)
summary(bci.tree)
head(bci.tree$edge)          #the first six edge (branch), with the first column as parental node and the second as child node
length(bci.tree$edge.length) #the number of edge
length(bci.tree$tip.label)   #the nunber of tips


#b. Do some simple plotting. To find out what options are available for plotting phylogenies, use ?plot.phylo. 
#Then, make a set of plots of the phylogeny using different types.
?plot.phylo
plot(bci.tree,type="fan",cex=0.4)

par(mfrow=c(1,5),mar=c(0,1,0,1))
plot(bci.tree,cex=0.4)
plot(bci.tree,type="cladogram",cex=0.4)
plot(bci.tree,type="fan",cex=0.4)
plot(bci.tree,type="unrooted",cex=0.4)
plot(bci.tree,type="radial",cex=0.4)


#c. plot a zoomed part of the phylogeny 
#(in this case, try the first 14 tips) using the zoom() function.
zoom(bci.tree,focus=bci.tree$tip[5:19])


#d. Use drop.tip() to remove 180 random tips from the tree, and plot the new tree
bci.tree.drop <- drop.tip(bci.tree,sample(bci.tree$tip,180,replace=F))
par(mfrow=c(1,1),mar=c(4,4,4,4))
plot(bci.tree.drop,type="fan",cex=0.8)


##e. Creating a random tree
random.tree <- rtree(50)
plot(random.tree)


##f. Calculate phylogentic distance among species
bci.tree.drop.dist <- cophenetic(bci.tree.drop)
dim(bci.tree.drop.dist )
bci.tree.drop.dist [1:5,1:5]  #the distance among the first five species



####################
#3.	Now we will calculate some phylogenetic community measures 
#(PD, MPD, MNTD, and their related indices,PDI, NRI and NTI) using BCI community data, 

#a. Make the species names in the community data and phylogy are consistent in format 
BCI2 <- BCI
bci.tree2 <- bci.tree
colnames(BCI2) <- bci.spp[,3]
bci.tree2$tip.label <- gsub("_"," ",fixed=TRUE,bci.tree2$tip.label)


#b.Now use the mpd(), pd() and mntd() functions to calculate phylogenetic diversity.
#Calculate PD
bci.pd <- pd(BCI2,bci.tree2)
bci.pd.mat <- matrix(bci.pd$PD,ncol=10,nrow=5);
image.plot(x=seq(0,1000,by=100),y=seq(0,500,by=100),t(bci.pd.mat),xlab="",ylab="")

#Calculate MPD
bci.mpd <- mpd(BCI2,cophenetic(bci.tree2))
bci.mpd.mat <- matrix(bci.mpd,ncol=10,nrow=5);
image.plot(x=seq(0,1000,by=100),y=seq(0,500,by=100),t(bci.mpd.mat),xlab="",ylab="")

#Calculate MNTD
bci.mntd <- mntd(BCI2,cophenetic(bci.tree2))
bci.mntd.mat <- matrix(bci.mntd,ncol=10,nrow=5);
image.plot(x=seq(0,1000,by=100),y=seq(0,500,by=100),t(bci.mntd.mat),xlab="",ylab="")



#c.	Use ses.mpd(), ses.pd() and ses.mntd() to calculate the standardized effect size statistics (PDI, NRI, and NTI).
#Calculate PDI
bci.pdi <- ses.pd(BCI2,bci.tree2,runs=99)
bci.pdi.mat <- matrix(bci.pdi$pd.obs.z,ncol=10,nrow=5);
image.plot(x=seq(0,1000,by=100),y=seq(0,500,by=100),t(bci.pdi.mat),xlab="",ylab="")

#Calculate NRI
bci.nri <- ses.mpd(BCI2,cophenetic(bci.tree2),runs=99)
bci.nri.mat <- matrix(-bci.nri$mpd.obs.z,ncol=10,nrow=5)
image.plot(x=seq(0,1000,by=100),y=seq(0,500,by=100),t(bci.nri.mat),xlab="",ylab="")

#Calculate NTI
bci.nti <- ses.mntd(BCI2,cophenetic(bci.tree2),runs=99)
bci.nti.mat <- matrix(-bci.nti$mntd.obs.z,ncol=10,nrow=5)
image.plot(x=seq(0,1000,by=100),y=seq(0,500,by=100),t(bci.nti.mat),xlab="",ylab="")


#d.	Compare the results for NRI using the "taxa.labels" algorithm to two other null model algorithms.
NRI1 = ses.mpd(BCI2,cophenetic(bci.tree2),null.model="taxa.labels",runs=99)
NRI2 = ses.mpd(BCI2,cophenetic(bci.tree2),null.model="frequency",runs=99)
NRI3 = ses.mpd(BCI2,cophenetic(bci.tree2),null.model="trialswap",runs=99)

plot(NRI1$mpd.obs.z,NRI2$mpd.obs.z)
plot(NRI1$mpd.obs.z,NRI3$mpd.obs.z)
plot(NRI2$mpd.obs.z,NRI3$mpd.obs.z)
#Pretty different, but still highly correlated.

