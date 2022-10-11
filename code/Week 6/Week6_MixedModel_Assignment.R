# Read in the "Toscano_Griffen_Data.csv" data from GitHub and load the three packages we used in the tutorial this week.
# The paper these data came from is uploaded to Canvas as "Toscano&Griffen_2014_JAE..."


# First create models with the same (y) and method (GLMM) as the published paper, using the GLMM function from the tutorial. 
  #Create two different models using the same 3 predictor (x) variables from the dataset. (4 points each) 
    # In one model only include additive effects.
    # In the other model include one interactive effect.
    # Use a binomial distribution and block as a random effect in both models to match the paper's analyses. Remember ?family to find distribution names.
setwd("C:/GitHub/Sherinskynl/code/Week 6")
read.csv("Toscano_Griffen_Data.csv")
df <- read.csv("Toscano_Griffen_Data.csv")
head(df)
glmm.mod.add <- glmmPQL(activity.level ~ temperature + toadfish.cue.treatment, family = binomial, random = ~ 1 | block, data = df)
summary(glmm.mod.add)
r.squaredGLMM(glmm.mod.add)
glmm.mod.inter <- glmmPQL(activity.level ~ claw.width, family = binomial, random = ~ 1 | block, data = df)
summary(glmm.mod.inter)
r.squaredGLMM(glmm.mod.inter)
# The authors used proportional consumption of prey as the (y) in their model, but did not include this in the dataset.
  # So we are going to create it - run the following line, assuming df= your data frame (feel free to change that):
df$prop.cons <- df$eaten/df$prey
glmm.coms.t <- glmmPQL(prop.cons ~ temperature + toadfish.cue.treatment, family = binomial, random = ~ 1 | block, data = df)
summary(glmm.coms.t)
glmm.coms <- glmmPQL(prop.cons ~ claw.width, family = binomial, random = ~ 1 | block, data = df)
summary(glmm.coms)
r.squaredGLMM(glmm.coms)
# (Q1) - The code in line 8 is performing two operations at once. What are they? (2 pts)
#  The code is signifying random effects in the data an specifying the family distribution as
# binomial

# (Q2) - Did the interactive effect change which variables predict proportional consumption? How, specifically, did the results change? (5 pts)
# Yes, when running the models with prop.cons as the y temperature has an unsignificant p value while toadfish.cue.treatmentYES has a significant p value
# while the claw.width has a p value of 0. Which is most likely an error? I am not sure.

# (Q3) - Plot the residuals of both models. Do you think either model is a good fit? Why or why not? (3 pts)
plot(glmm.coms.t$residuals)
plot(glmm.coms$residuals)
# the glmm.coms or the plot comparing proportional comsumption with claw width has a tighter pattern in the plot. 
# But both are spread out and fuzzy, they do not provide good fits.
# Re-run both models as generalized additive models instead (using gam). Then compare the AIC of both models. (4 points each)
gam.coms.t <- gam(prop.cons ~ temperature + toadfish.cue.treatment, family = binomial, random = list(block=~ 1), data = df)
gam.coms <- gam(prop.cons ~ claw.width, family = binomial, random = ~ 1 | block, data = df)

# (Q4) - Which model is a better fit? (2 pt)
AIC(gam.coms.t, gam.coms)


# (Q5) - Based on the residuals of your generalized additive models, how confident are you in these results? (2 pts)








