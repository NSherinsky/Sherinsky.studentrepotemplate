# For this week it's time to start exploring your own ideas and questions in R.
  # There are at least five options in the dataset to create the following comparisons.
library(readxl)
setwd("C:/GitHUb/Sherinskynl/code/Week 9")
abiotic.tibble <- read_excel("Penaetal_2016_data.xlsx", sheet = "Abiotic factors")
abiotic <- as.data.frame(abiotic.tibble)
invertebrate.tibble <- read_excel("Penaetal_2016_data.xlsx", sheet = "Invertebrate_community")
invertebrate <- as.data.frame(invertebrate.tibble)
# (Q1 - 12 pts) Use the dataset from the tutorial to complete one redundancy analysis (RDA) with variance partitioning on a different community (NOT the nematodes).
  # Explain the ecological importance of your significant predictor variables, or the importance if none are significant for your community.
abiotic.names <- paste(abiotic$Land_use, abiotic$Parcel)
abiotic$names <- abiotic.names
invertebrate.names <- paste(invertebrate$Landuse, invertebrate$Parcel)
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
invertebrate.mean2 <- invertebrate.mean1[,-71]
invertebrate.mean2 <- sapply(invertebrate.mean2, as.numeric )
library(vegan)
colnames(abiotic.mean3)
ab.invert <- merge(abiotic.mean3, invertebrate.mean2, by = "Group.1")
ord <- rda(ab.invert)
ord <- rda(invertebrate.mean2 ~ pH + totalN + Perc_ash + Kalium + Magnesium + Ca + Al + TotalP + OlsenP, abiotic.mean3)



# (Q2 - 12 pts) Then use the dataset from the tutorial to create a linear model related to your RDA. Try multiple predictors to find the best fit model.
  # Explain the ecological importance of the significant predictors, or lack of significant predictors.

# (Q3 - 6 pts) Provide a 3-4 sentence synthesis of how these results relate to one another and the value of considering both together for interpreting biotic-abiotic interactions.


