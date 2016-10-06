# Here's a (simulated) experiment, with a single subject and 500 categorization trials.
all.data <- read.csv('experiment-data.csv')
source('memory-limited-exemplar-model.R')
rm(sample.data.set)
rm(sample.training.data)

# Use optim() to fit the model to this data.
# Note: In optim() you can tell it to display updates as it goes with:
# optim( ... , control=list(trace=4))

exemplar.memory.log.likelihood.for.fit <- function(parameters) {
  sensitivity <- parameters[1]
    if (sensitivity < 0) {return(NA)}
  decay.rate <- parameters[2]
    if(decay.rate < 0) {return(NA)}
    if(decay.rate > 1) {return(NA)}
  
  likelihood <- sapply(1:nrow(all.data), function(x){
    if(x == 1) {return(0.5)}
     if(all.data[x, ]$correct == T) 
    {return(exemplar.memory.limited(all.data[0:(x-1), ], all.data[x, ]$x, all.data[x, ]$y, all.data[x, ]$category, sensitivity, decay.rate))}
    
    if(all.data[x, ]$correct == F)
    {return(1-(exemplar.memory.limited(all.data[0:(x-1), ], all.data[x, ]$x, all.data[x, ]$y, all.data[x, ]$category, sensitivity, decay.rate)))}
    
  }) 
  
  return(sum(-log(likelihood)))
}

fit1 <- optim(c(0.5, 0.5), exemplar.memory.log.likelihood.for.fit, method = "Nelder-Mead", control=list(trace=4))
# sensitivity = 5.154, decay.rate = 0.627
# log likelihood = -187.5985

# Now try fitting a restricted version of the model, where we assume there is no decay.
# Fix the decay.rate parameter to 1, and use optim to fit the sensitivity parameter.
# Note that you will need to use method="Brent" in optim() instead of Nelder-Mead. 
# The brent method also requires an upper and lower boundary:
# optim( ..., upper=100, lower=0, method="Brent")

exemplar.memory.log.likelihood.brent <- function(parameters) {
  sensitivity <- parameters[1]
  if (sensitivity < 0) {return(NA)}
  decay.rate <- 1
  
  likelihood <- sapply(1:nrow(all.data), function(x){
    if(x == 1) {return(0.5)}
    if(all.data[x, ]$correct == T) 
    {return(exemplar.memory.limited(all.data[0:(x-1), ], all.data[x, ]$x, all.data[x, ]$y, all.data[x, ]$category, sensitivity, decay.rate))}
    
    if(all.data[x, ]$correct == F)
    {return(1-(exemplar.memory.limited(all.data[0:(x-1), ], all.data[x, ]$x, all.data[x, ]$y, all.data[x, ]$category, sensitivity, decay.rate)))}
    
  }) 
  
  return(sum(-log(likelihood)))
}

fit.brent <- optim(c(0.5), exemplar.memory.log.likelihood.brent, upper=100, lower=0, method = "Brent", control = list(trace=4))
# sensitivity = 3.86 
# log likelihood = -248.5161

# What's the log likelihood of both models? (see the $value in the result of optiom(),
# remember this is the negative log likeihood, so multiply by -1.

# What's the AIC and BIC for both models? Which model should we prefer?

# AIC: 2k - 2ln(L)
# BIC: k * ln(N) - 2ln(L)
# k = number of free parameters (2 in first model when decay rate isn't fixed, 1 in second model when decay rate is fixed)
# L = maximum likelihood
# N = sample size = 500
# AIC Model 1: 379.197
# AIC Model 2: 499.0322
# BIC Model 1: 387.6262
# BIC Model 2: 503.2468

# We should prefer model 2.

#### BONUS...
# If you complete this part I'll refund you a late day. You do not need to do this.

# Use parametric bootstrapping to estimate the uncertainty on the decay.rate parameter.
# Unfortunately the model takes too long to fit to generate a large bootstrapped sample in
# a reasonable time, so use a small sample size of 10-100 depending on how long you are
# willing to let your computer crunch the numbers.

# Steps for parametric bootstrapping:
# Use the best fitting parameters above to generate a new data set (in this case, that means
# a new set of values in the correct column for all.data).
# Fit the model to this new data, record the MLE for decay.rate.
# Repeat many times to get a distribution of decay.rate values.
# Usually you would then summarize with a 95% CI, but for our purposes you can just plot a
# histogram of the distribution.

