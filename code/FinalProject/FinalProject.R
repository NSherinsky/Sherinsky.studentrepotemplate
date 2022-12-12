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
head(eighteenturb3)    

oakorc3$Date <- format(as.POSIXct(oakorc3$dateTime,format='%Y-%m-%d %H:%M:%S'),format='%m/%d/%Y')
head(oakorc3)

install.packages("stringr")
library(stringr)

eighteenturb3$Water.Body <- str_replace(eighteenturb3$Water.Body, "04219768", "Eighteenmile Creek")
head(eighteenturb3)

oakorc3$Water.Body <- str_replace(oakorc3$Water.Body, "0422016550", "Oak Orchard Creek")
head(oakorc3)


macro.mean <- aggregate(macroinvert4$Diversity.Score ~ macroinvert4$Water.Body + macroinvert4$Date.Sampled, FUN = "mean")
head(macro.mean)

eight.mean <- aggregate(eighteenturb3$turbidity ~eighteenturb3$Date + eighteenturb3$Water.Body, FUN = "mean")
head(eight.mean)

oak.mean <- aggregate(oakorc3$turbidity ~oakorc3$Date + oakorc3$Water.Body, FUN = "mean")
head(oak.mean)

?merge
colnames(oak.mean) <- c("Date","Water.Body","turbidity")

colnames(eight.mean) <- c("Date","Water.Body","turbidity")
 
colnames(macro.mean) <- c("Water.Body","Date","Diversity.score")

macro.mean$Date <- format(as.POSIXct(macro.mean$Date,format='%m/%d/%Y'),format='%m/%d/%Y')

Creeks <- rbind(oak.mean,eight.mean)


macro.creeks <- merge(macro.mean,Creeks, by = c("Date","Water.Body"))

library(mgcv)
install.packages("MASS")
library(MASS)
?mgcv
?gam

gam.diverse <- gam(Diversity.score~ turbidity + Water.Body, family = Gamma, random = list(ID=~1), data = macro.creeks)
summary(gam.diverse)

plot(gam.diverse$residuals, ylim = c(-.1,.1))
vis.gam(gam.diverse, view=c("turbidity","Water.Body"), theta = 45, color = "heat")
AIC(gam.diverse)

gam.mod2 <- gam(Diversity.score~ turbidity*Water.Body, family = Gamma, random = list(ID=~1), data = macro.creeks)
summary(gam.mod2)
AIC(gam.mod2)

