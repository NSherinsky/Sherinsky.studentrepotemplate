# Load the packages from this week's tutorial.
setwd("C:/GitHub/Sherinskynl/code/Week 10")
library(spdep)
library(adespatial)
library(vegan)
#In the tutorial we looked at the community as a whole and the swimmers which ultimately matched a prediction we had for their distribution.

#Part 1: Look at two other subsets of the community and determine the relative influence of space and habitat on each following the methods in the tutorial. (10 points)
#The options include groupings by taxonomy, where Diptera (true flies) have the strongest flight ability, Trichoptera the 2nd strongest, 
    #Ephememeroptera are 3rd, and non insects are 4th...because they don't have wings.
#Groupings by habits include the swimmers (off limits for the assignment) as most mobile, sprawlers as 2nd (they move in search of food, but not quickly),
    #and the clingers come in last (they might move up and down on individual rocks).
PatchLatLon.csv <- read.csv("PatchLatLon.csv", header=T)
BugsByPatch.csv <- read.csv("BugsByPatch.csv", header=T)
HabitatbyPatch.csv <- read.csv("HabitatbyPatch.csv", header=T)
Clingers.csv <- read.csv("Clingers.csv", header=T)
Diptera.csv <- read.csv("Diptera.csv")

PatchLatLon.mat <- as.matrix(PatchLatLon.csv[,-1])
BugsByPatch.mat <- as.matrix(BugsByPatch.csv)
HabitatbyPatch.mat <- as.matrix(HabitatbyPatch.csv)
Clingers.mat <- as.matrix(Clingers.csv)
Diptera.mat <- as.matrix(Diptera.csv)

nb<-cell2nb(3,30,"queen")
nb1 <- droplinks(nb, 19, sym=TRUE)
nb2 <- droplinks(nb1, 22, sym=TRUE)
nb3 <- droplinks(nb2, 25, sym=TRUE)
nb4 <- droplinks(nb3, 30, sym=TRUE)
bin.mat <- aem.build.binary(nb4, PatchLatLon.mat, unit.angle = "degrees", rot.angle = 90, rm.same.y = TRUE, plot.connexions = TRUE)
plot(PatchLatLon.mat[,2]~PatchLatLon.mat[,3], xlim = rev(c(76.75,77)))
aem.ev <- aem(aem.build.binary=bin.mat)
aem.df <- aem.ev$vectors[c(-19,-22,-25,-30),]
aem.df
Space.rda <- rda(BugsByPatch.mat, as.data.frame(aem.df))
Space.r2a <- RsquareAdj(Space.rda)$adj.r.squared
aem.fwd <- forward.sel(BugsByPatch.mat,aem.df, adjR2thresh=Space.r2a)
aem.fwd$order
SpaceNoHab.rda <- rda(BugsByPatch.mat, as.data.frame(aem.df[,aem.fwd$order]), HabitatbyPatch.mat)
SpaceNoHab.rda 
anova(SpaceNoHab.rda, perm.max = 10000)
RsquareAdj(SpaceNoHab.rda)
HabNoSpace.rda <- rda(BugsByPatch.mat, HabitatbyPatch.mat, as.data.frame(aem.df[,aem.fwd$order]))
HabNoSpace.rda 
anova(HabNoSpace.rda, perm.max = 10000)
RsquareAdj(HabNoSpace.rda)

ClingSpace.rda <- rda(Clingers.mat, as.data.frame(aem.df))
ClingSpace.r2a <- RsquareAdj(ClingSpace.rda)$adj.r.squared
Clingaem.fwd <- forward.sel(Clingers.mat,as.data.frame(aem.df), adjR2thresh=Space.r2a)

ClingSpaceNoHab.rda <- rda(Clingers.mat, as.data.frame(aem.df[,Clingaem.fwd$order]), HabitatbyPatch.mat)
ClingSpaceNoHab.rda 
anova(ClingSpaceNoHab.rda, perm.max = 10000)
RsquareAdj(ClingSpaceNoHab.rda)
#Constrained 48% Conditional 26%

ClingHabNoSpace.rda <- rda(Clingers.mat, HabitatbyPatch.mat, as.data.frame(aem.df[,Clingaem.fwd$order]))
ClingHabNoSpace.rda 
anova(ClingHabNoSpace.rda, perm.max = 10000)
RsquareAdj(ClingHabNoSpace.rda)
#Constrained 4.4% Conditional 70%

DipSpace.rda <- rda(Diptera.mat, as.data.frame(aem.df))
DipSpace.r2a <- RsquareAdj(DipSpace.rda)$adj.r.squared
Dipaem.fwd <- forward.sel(Diptera.mat,as.data.frame(aem.df), adjR2thresh=Space.r2a)

DipSpaceNoHab.rda <- rda(Diptera.mat, as.data.frame(aem.df[,Dipaem.fwd$order]), HabitatbyPatch.mat)
DipSpaceNoHab.rda 
anova(DipSpaceNoHab.rda, perm.max = 10000)
RsquareAdj(DipSpaceNoHab.rda)
#Constrained 41%% Conditional 40%%

DipHabNoSpace.rda <- rda(Diptera.mat, HabitatbyPatch.mat, as.data.frame(aem.df[,Dipaem.fwd$order]))
DipHabNoSpace.rda 
anova(DipHabNoSpace.rda, perm.max = 10000)
RsquareAdj(DipHabNoSpace.rda)
#Constrained 4.8% Conditional 77%

#Part 2: What is your interpretation of the pattern for each group individually, and the two in comparison, based on their mobility? (5 points)
# The clingers did not have a large change in values. The Clinger with space no habitat constrained only increased by 2%. Then, the clingers 
# with habitat no space, there was a decrease in constrained by .5% and an increase in conditional by 3%. This is representative of the clingers caring
# more about the habitat than how much space they have, whihc makes sense since they just hang out on rocks in one particular location.
# The Diptera had a large change in conditional for space no habitat, increasing by 14%! And! For habitat no space there was a conditional 
# increase by 10% which is pretty significant. This is expected as they are the strongest fliers and will need a habitat to accommodate and the 
# space to fly around.

#Part 3: For each of your chosen groups of bugs, perform variable selection for the habitat data rather than the AEM data. Which habitat variables are significant for each? (10 points)
  # Definitions for the habitat column names:
    #Inorg = total suspended inorganic solids in the water column
    #Organ = total suspended organic solids in the water column
    #Chla = Chlorophyll a concentration from benthic rocks collected in-stream
    #BOM = total benthic organic matter in the sample
    #Depth = water depth
    #Flow	= water velocity where samples were collected
    #Fines = Percent of the substrate as "fines" i.e. small particles too small to measure
    #AveAr = The average size of rocks where each sample was collected
cling.vs <- rda(Clingers.csv ~., HabitatbyPatch.csv)
cling.vs
# The AveAr or average size of rocks has the smallest RDA value. Which is to be expected as clingers 
# rely on rocks for everything basically. Other that that, from the prior list, bottom to top is the order
# of importance to clingers.
dip.vs <- rda(Diptera.csv ~., HabitatbyPatch.csv)
dip.vs
# I got 0(??) for  three of the RDA, I dunno what that means but, i feel if it means those have inconclusive
# affects that would make sense since rocks, substrate an water velocity would not necessarily affect
# diptera, as it flies. The rest could be argued that they would have an effect. I do think water depth 
# could be explained as the most important as if the Diptera can only get food from surfaces of water
# a deeper depth would make it harder to get food.
#Part 4: How do you expect selecting both the spatial and the habitat variables would change the results of the RDAs from Part 1 above? (5 points)
  #(You do not need to redo the RDAs, unless you *want* to.)
# I think adding spatial variables would have little effect on the clingers RDA, since they don't need
# a lot of space. I do think it would increase the Diptera's strength relationship since they do 
# need space in order to fly around.

install.packages("png")
library(png)
setwd("C:/GitHub/Sherinskynl/code/Week 10")
Picture<-readPNG("omgpRanked.svg")
plot(1:10,ty="n")
rasterImage(Picture,3,3,7,7)
