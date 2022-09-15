# (1) Approximately how many hours ahead of Sunbury was the peak flow in Lewisburg during the 2011 flood? (2 pt)
# 8 hours

# (2) Give one reason why information on the time between peak flow events up- and downstream could be valuable? (4 pts)
# Knowing the time between peak flows could help one to figure out the distance between lewisburg and sunbury, 
# which in turn would give you how fast the water in traveling.

# Package scavenger hunt! (12 pts each)

## (3) Using Google and ONLY packages from GitHub or CRAN:
    # Find a package that contains at least one function specifically designed to measure genetic drift.
    # Copy-paste into your script - and run - an example from the reference manual for a function within this package related to a measure of genetic drift. 
        # Depending on the function, either upload a plot of the result or use print() and copy/paste the console output into your script.
    # After running the function example, manipulate a parameter within the function to create a new result. 
        # Common options might be allele frequency, population size, fitness level, etc. 
        # Add the results of this manipulation to your script (if in the console) or upload the new plot.
install.packages("learnPopGen") 
library(learnPopGen)
drift.selection(p0=0.5, Ne=100, w=c(1,1,1), ngen=400, nrep=10, colors=NULL)
drift.selection(p0=0.4, Ne=33, w=c(1,1,1), ngen=300, nrep=10, colors=NULL)

          # By manipulating these parameters you can see how it impacts the results.
          # This type of manipulation is one example of how theoretical ecology and modelling are used to predict patterns in nature.



## (4) Using Google and ONLY packages from GitHub or CRAN:
    # Find a package that will generate standard diversity metrics for community ecology, specifically Simpson's Diversity Index.
    # Copy-paste into your script - and run - an example from the reference manual for a function to calculate Simpson's diversity. 
        # Depending on the example usage of the function, either upload a plot of the result or use print() and copy/paste the console output into your script.
install.packages("diverse")
library(diverse)
diversity(data = geese, type = "simpson", category_row = TRUE, dis = NULL,
          method = "euclidean", q = 0, alpha = 1, beta = 1, base = exp(1))
#     simpson.D simpson.I simpson.R
#1996 0.5534977 0.4465023  1.806692
#1997 0.5674638 0.4325362  1.762227
#1999 0.6150397 0.3849603  1.625911
#2000 0.6392648 0.3607352  1.564297
#2001 0.6622776 0.3377224  1.509941
#2002 0.6767693 0.3232307  1.477608
#2003 0.6838655 0.3161345  1.462276
#2004 0.7149405 0.2850595  1.398718
#2005 0.7160910 0.2839090  1.396471
#2006 0.7245616 0.2754384  1.380145
    # After running the function example, modify your script to generate another diversity metric that is NOT part of the example. 
        # If there are two diversity metrics in the example script, neither of these will count as the modified script.
        # Hint: If the function can "only" caluclate Simpson's diversity, the inverse of Simpson's diversity is another common metric. 
        # Add the results of this manipulation to your script (if in the console) or upload the new plot.
diversity(data = geese, type = "berger-parker", category_row = TRUE, dis = NULL,
          method = "euclidean", q = 0, alpha = 1, beta = 1, base = exp(1))    
#         berger.parker.D berger.parker.I
#1996       0.7124871        1.403534
#1997       0.7247953        1.379700
#1998       0.7451379        1.342033
#1999       0.7674295        1.303051
#2000       0.7860407        1.272199
#2001       0.8026238        1.245914
#2002       0.8127443        1.230399
#2003       0.8175550        1.223159
#2004       0.8382420        1.192973
#2005       0.8392823        1.191494
#2006       0.8446653        1.183901
          # Diversity metrics are frequently used in community ecology for reasons ranging from a quick comparison between sites to understanding community stability.
          # Their calculation can be very tedious by hand - and very fast with a package designed for the operation.
#enjoy :) Turn up your sound its kinda quiet
install.packages("beepr")
library(beepr)
beep(9)
#omg pRanked, i cant believe you fell for it 
