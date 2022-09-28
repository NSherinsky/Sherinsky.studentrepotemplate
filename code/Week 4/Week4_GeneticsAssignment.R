# Look at the plot and model results for our Dryad data in the tutorial. Part 1: Without knowing which points represent which groups, 
  # give one explanation for why these data might be difficult to draw spatial inferences about genes.(3 points)
#There is several spots where points overlap which can affect how we interpret those areas. Also there is
#a general shotgun dispersal on the left side with three out-liers on the right. This will also make it difficult to 
#get a spatial idea of the genes.
  # Part 2: Despite the drawbacks, give the result or interpretation that you feel most confident in (3 points), and EXPLAIN WHY (4 points).
#The Dots of similar color that are spread out (i.e Black)have genotypes and therefor phenotypes that are 
#more generally adapted. While colors like pink, that are closer together have more specilization. I feel this way because
#it is more likely for a organism with general traits to spread out and find place it can survive. While
#an organism with spcialized traits will only be able to stay in a few areas.

# For your scripting assignment we will use the "ge_data" data frame found in the "stability" package.
  # Install the "stability" package, load it into your R environment, and use the data() function to load the "ge_data". (2 points)
install.packages("stability")
library(stability)
data("ge_data")
head(ge_data)
data <- ge_data
head(data)
?ge_data
# Create two linear models for Yield Response: one related to the Environment and one to the Genotype. (2 points each)
  # 'Yield Response' in this dataset is a measure of phenotype expression.
  # Hint: Look at the help file for this dataset.
mod.env <- lm(data$Yield~ data$Env)
anova(mod.env)
mod.geno <- lm(data$Yield ~ data$Gen)
anova(mod.geno)
# Test the significance of both models and look at the model summary. (3 points each)
summary(mod.env)
summary(mod.geno)
plot(data$Yield ~ data$Gen)
plot(data$Yield ~ data$Env)
  # Which model is a better fit to explain the yield response, and WHY? (6 points)
  # Hint: Does one model seem more likely to be over-fitted?
#The model for Environment while it has less data points, most of them are significant, while in the 
#genotype model there is a lot of data but only a handful are significant in their p values.
#We are actually talking about the number of unique groups rather than the total number of samples. Your overall approach is on-point though. What about the r-squared?

# Which environment would be your very WORST choice for generating a strong yield response? (2 points)
#Sargodha would be the worst environment to produce a strong yield response since the p vaule was 0.7138
