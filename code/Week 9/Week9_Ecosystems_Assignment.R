# For this week it's time to start exploring your own ideas and questions in R.
  # There are at least five options in the dataset to create the following comparisons.
library(readxl)
setwd("C:/GitHUb/Sherinskynl/code/Week 9")
abiotic.tibble <- read_excel("Penaetal_2016_data.xlsx", sheet = "Abiotic factors")
abiotic <- as.data.frame(abiotic.tibble)
invertebrate.tibble <- read_excel("Penaetal_2016_data.xlsx", sheet = "Invertebrate_community")
invertebrate <- as.data.frame(invertebrate.tibble)
head(abiotic)
head(invertebrate)
# (Q1 - 12 pts) Use the dataset from the tutorial to complete one redundancy analysis (RDA) with variance partitioning on a different community (NOT the nematodes).
  # Explain the ecological importance of your significant predictor variables, or the importance if none are significant for your community.
abiotic.names <- paste(abiotic$Parcel)
abiotic$names <- abiotic.names
invertebrate.names <- paste(invertebrate$Parcel)
invertebrate$names <- invertebrate.names
abiotic.mean <- aggregate(x = abiotic, by = list(abiotic$names), FUN = "mean")
head(abiotic.mean)
invertebrate.mean <- aggregate(x = invertebrate, by = list(invertebrate$names), FUN = "mean")
head(invertebrate.mean)
abiotic.mean1 <- abiotic.mean[,-16]
head(abiotic.mean1)
abiotic.mean2 <- abiotic.mean1[,-2:-3]
head(abiotic.mean2)
abiotic.mean3 <- abiotic.mean2[,-3:-4]
head(abiotic.mean3)

invertebrate.mean1 <- invertebrate.mean[,-2:-3]
head(invertebrate.mean1)
invertebrate.mean2 <- invertebrate.mean1[,-71]
head(invertebrate.mean2)

library(vegan)
colnames(abiotic.mean3)
install.packages("plyr")
library(plyr)
ab.invert <- rbind.fill(abiotic.mean3, invertebrate.mean2)
ab.invert2 <- merge(abiotic.mean3, invertebrate.mean2, by = "Group.1")
ab.invert3 <- ab.invert2[,-1]
ord <- rda(ab.invert3)
ord
# There is no proportion in my RDA? So i dunno what that means. But personally i think that the abiotic factors have a
#bigger impact on the plant community than the invertebrate community. There is a secondary effect though with that
# if certainplants are growing because of those factors, only invertebrates that feed on those plants will thrive and grow
# in those environments.
#Your rda doesn't include any habitat variables to partition, e.g. rda(ab.invert3~totalN). That's why the proportion is missing.

# (Q2 - 12 pts) Then use the dataset from the tutorial to create a linear model related to your RDA. Try multiple predictors to find the best fit model.
  # Explain the ecological importance of the significant predictors, or lack of significant predictors.
library(fitdistrplus)
library(logspline)
fit.weibull <- fitdist(ab.invert3$pH, distr = "weibull")
fit.norm <- fitdist(ab.invert3$pH, distr = "norm")
fit.gamma <- fitdist(ab.invert3$pH, distr = "gamma")
fit.lnorm <- fitdist(ab.invert3$pH, distr = "lnorm")
fit.nbinom <- fitdist(ab.invert3$pH, distr = "nbinom")
fit.logis <- fitdist(ab.invert3$pH, distr = "logis")
fit.geom <- fitdist(ab.invert3$pH, distr = "geom")
gofstat(list(fit.weibull, fit.norm, fit.gamma, 
             fit.lnorm, fit.logis))
colnames(ab.invert3)
mod1 <- lm(pH ~Plot + totalN + Trichia_hispida + Opomyza_sp,ab.invert3)
anova(mod1)
AIC(mod1)
#Model is backward :(
#Think about this logically - would a species (i.e. trichia or opomyza) affect the pH or are the species more likely to be affected by pH?
#pick one species and use that as the y in this case, unless you have specific reasons for this model structure.
# (Q3 - 6 pts) Provide a 3-4 sentence synthesis of how these results relate to one another and the value of considering both together for interpreting biotic-abiotic interactions.
# The original RDA shows if there any relationship between the factors. Then the use of the linear models is to find if/ which factors 
# have the biggest impact on the data sets. Using both these together you can see if your data is worth working with using the RDA
# and the Linear model will help you further analyze your data comparing the abiotic to the biotic.
#Good general answer, but it does not use the results that you created!

par(bg=1)
plot(c(0,0), cex=0, xlim=c(-1,1), ylim=c(-1,1))
X <- runif(100,-1,1)
Y <- runif(100,0,1)
M <- rchisq(100,1)/20
points(Y~X, cex=M, pch=19, col=colors()[1])
points(0,-0.5, pch=19, col=colors()[498],cex=20)
for (i in 1:4){polygon(locator(3),col=7)}

# theoRetically, this will gRaph a oRange circle and if you click on thRee points it will cut out a tRiangle
# and then you are caRving a pumpkin! 
#THIS IS AMAZING! Well done, well done indeed!
