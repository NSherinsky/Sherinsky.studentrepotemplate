install.packages("devtools")
install.packages("mgcv")
install.packages("MuMIn")
library(devtools)
library(mgcv)
library(MuMIn)
install_github("USGS-R/dataRetrieval")

library(dataRetrieval)
?readNWISuv
#turbidity data code 63680

setwd("C:/GitHub/Sherinskynl/code/FinalProject")
macroinvert <- read.csv("EighteenmileCreek.csv")
eighteenturb <- readNWISuv("04219768","63680","2021-08-03","2021-08-04")
head(eighteenturb)
oakorc <- readNWISuv("0422016550","63680", "2021-08-04","2021-08-05")
head(oakorc)

eighteenturb2 <- eighteenturb[,-1]
eighteenturb3 <- eighteenturb2[,-4:-5]
head(eighteenturb3)
colnames(eighteenturb3) <- c("Water.Body","dateTime","turbidity")
head(eighteenturb3)

oakorc2 <- oakorc[,-1]
oakorc3 <- oakorc2[,-4:-5]
colnames(oakorc3) <- c("Water.Body","dateTime","turbidity")
head(oakorc3)

head(macroinvert)
macroinvert2 <-macroinvert[,-2]
head(macroinvert2)
macroinvert3 <-macroinvert2[,-3:-10]
head(macroinvert3)
macroinvert4 <- macroinvert3[,-4:-6]
head(macroinvert4)


eighteenturb3$Date <- format(as.POSIXct(eighteenturb3$dateTime,format='%Y-%m-%d %H:%M:%S'),format='%m/%d/%Y')
head(eighteenturb4)                       

?aggregate
macro.mean <- aggregate(macroinvert4$Diversity.Score ~ macroinvert4$Water.Body + macroinvert4$Date.Sampled, FUN = "mean")
head(macro.mean)

eight.mean <- aggregate(eighteenturb3$turbidity ~eighteenturb3$dateTime, FUN = "mean")
head(eight.mean)

#week 6 for gam


