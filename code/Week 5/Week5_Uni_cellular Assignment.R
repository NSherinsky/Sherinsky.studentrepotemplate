# First, recreate Figure 4 from Herron et al. (2019). De novo origins of multicellularity in response to predation. Scientific Reports.
  # Search datadryad.org by the paper title and download the dataset. It will include .csv files and R scripts, organized by figure.
  # Save the script and change the working directory on lines 8 and 115 to match your GitHub repository. (6 points)
  # Export and save the plot you've created. (2 points)
  # Zoom into your plot to look at the distribution for different strains.

# Do all of the strains in the plot have the same distributions (yes/no)? (2 pt)
  #no
# Based on these observations of your strain distributions, why did the authors use a Kruskal-Wallis test rather than ANOVA to compare the strains? (2 pts)
  #The authors most likely used Kruskal-Wallis because the distributions of the strains are not the same. If they were similar they would have used ANOVA

# Use the fitdist() and gofstat() functions to compare the poisson, negative binomial, and logistic distributions for:
  # (1) - The number of cells of progeny (data$Num.Cells.Progeny)
  # (2) - The replication time (data$RepTime.sec)
      # 3 points each
    #HINT- "Num.Cells.Progeny" has defined breaks. To display results, use the formula with the "chisqbreaks" argument as follows:
      #gofstat(list(fit.1, fit.2, fit.3, etc), chisqbreaks=c(1,2,4,8,16,32,64))

library(fitdistrplus)
one.col <- data$Num.Cells.Progeny
fit.p.num <- fitdist(c(na.exclude(one.col)), distr = "pois")
fit.nb.num <- fitdist(c(na.exclude(one.col)), distr = "nbinom")
fit.lg.num <- fitdist(c(na.exclude(one.col)), distr = "logis")
gofstat(list(fit.lg.num, fit.nb.num, fit.p.num), chisqbreaks = c(1,2,4,8,16,32,64))

two.col <- data$RepTime.sec
fit.p.rep <- fitdist(c(na.exclude(two.col)), distr = "pois")
fit.nb.rep <- fitdist(c(na.exclude(two.col)), distr = "nbinom")
fit.lg.rep <- fitdist(c(na.exclude(two.col)), distr = "logis")
gofstat(list(fit.lg.rep, fit.nb.rep, fit.p.rep))

# Based on the AIC scores, which distribution is the best fit for: (4 pts)
  # (1) - The number of cells of progeny (data$Num.Cells.Progeny)?
  # (2) - The replication time (data$RepTime.sec)?
# The number of cells progeny has a better fit distribution because the AIC values are close to 8000 which is less 
# than the replication time (values around 50,000 and 900,000) 

# Plot a generic histogram for the replication time (data$RepTime.sec) (2 pt)
hist(two.col, main = "Replication Time")
# Based on the patterns of this histograms and Figure 4:
  #Give one hypothesis for an evolutionary process represented by the two tallest bars in your histogram. (6 pts)
  # Don't cheat by looking at the paper! 
    # This hypothesis does not need to be correct - it only needs to be ecologically rational based these two figures.
# The first large column shows the first big boom in population. This most likely raised the carrying capacity for the predators
# so the population decreased with predation. Less prey means less predators so once there were not as many predators
# the second tall column came as the population was able to boom again with out as high of predation risk.

x <- c(2,2,2,2,2,2,3,3,3,4,4,4,4,5)
y <- c(1,2,3,4,5,6,3,4,6,2,4,5,6,1)
plot(x,y, xlim = c(-1,7), col = 641, pch = 15, main = "omg pRanked")




