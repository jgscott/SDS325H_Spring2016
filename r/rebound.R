library(mosaic)

shocks = read.csv("../data/shocks.csv", header=TRUE)

# Plot the data
plot(cheap~expensive,data=shocks)
lm1 = lm(cheap~expensive,data=shocks)
abline(lm1)

# Slope and R^2 via bootstrapping
boot1 = do(1000)*{
	lm(cheap~expensive,data=resample(shocks))
}
confint(boot1)

# Looks like we're reasonably confident
# that beta1 could be 1 and that R^2 > 90% (though not a slam dunk)

# Prediction interval

# Values of x where we want to predict
# Here, just one x
xstar = data.frame(expensive=c(510, 550, 590))

# Bootstrapped prediction intervals
# Key idea: simulate future data points
NMC = 10000
boot_pred = do(NMC)*{
  
  # Step 1a: Fit a model to a bootstrapped sample
  lmboot = lm(cheap~expensive,data=resample(shocks))
  
  # Step 1b: Form the plug-in prediction from the bootstrapped model.
  yhat = predict(lmboot,newdata=xstar)
  
  # Resample the residuals from the bootstrapped model
  # This step bakes in residual uncertainty
  eps = sample(resid(lmboot), size=3, replace=TRUE)
  
  # Form and return the notional future data points
  ystar = yhat+eps
  ystar
}

# Look at the first few lines
head(boot_pred)

# Our sampling distribution for our prediction
hist(boot_pred$X1)
sd(boot_pred$X1)
sd(boot_pred$X2)
sd(boot_pred$X3)

# 95% prediction interval for every point
ci_bounds = apply(boot_pred, 2, quantile, probs=c(0.025, 0.975))

# View the bounds
ci_bounds

# Calculate the width of these intervals: do they exceed 36 units?