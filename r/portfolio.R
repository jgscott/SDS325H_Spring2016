library(mosaic)
library(fImport) # need to install this the first time you use it
library(mvtnorm)  # this too

# Import code for bivariate normal
# Put this line at the top of any script where you need to simulate
# from a bivariate normal distribution.
source("http://jgscott.github.io/teaching/r/mvnorm/rbvnorm.R")

# Import helper function
source("http://jgscott.github.io/teaching/r/mvnorm/computereturns.R")


# Import a few stocks
mystocks = c("ORCL", "JNJ", "WMT", "XOM", "MRK")
myprices = yahooSeries(mystocks, from='2007-04-05', to='2016-04-05')

# The first few rows
head(myprices)

# Compute the returns from the closing prices
myreturns = computereturns(myprices)
head(myreturns)

# Do these look normal?
ind = 2
hist(myreturns[,ind], 100, prob=TRUE)
curve(dnorm(x, mean(myreturns[,ind]), sd(myreturns[,ind])), add=TRUE)

# If they're not univariate normal, they can't be multivariate normal!
pairs(myreturns)

# How to proceed?

# One way: calculate moments of distribution
# for a one-period change in your portfolio
totalwealth = 10000
pweights = c(0.2, 0.2, 0.2, 0.2, 0.2)	# What percentage of your wealth will you put in each stock?

# Estimate the mean and covariance matrix
mu = colMeans(myreturns)
Sigma = cov(myreturns)

# Calculate mean and variance of linear combination
mu_port = pweights %*% mu
var_port = t(pweights) %*% Sigma %*% pweights
sd_port = sqrt(var_port)

# Chebyshev's inequality
mu_port - 2*sd_port  # no more than 25% chance
mu_port - 5*sd_port  # no more than 4% chance


# A second way: bootstrapping.
# That is: sample from the empirical joint dist'n rather a theoretical model.

# How much money do we have in each stock?
totalwealth = 10000
holdings = pweights * totalwealth

# Sample a random return from the empirical joint distribution
# This simulates a random day.
return.today = resample(myreturns, 1, orig.ids=FALSE)

# Update the value of your holdings
holdings = holdings*(1+return.today)

# Compute your new total wealth
totalwealth = sum(holdings)

# Now loop over four weeks
horizon = 20
totalwealth = 10000
weights = c(0.2, 0.2, 0.2, 0.2, 0.2)
holdings = weights * totalwealth
wealthtracker = rep(0, horizon) # Set up a placeholder to track total wealth
for(today in 1:horizon) {
	return.today = resample(myreturns, 1, orig.ids=FALSE)
	holdings = holdings*(1+return.today)
	totalwealth = sum(holdings)
	wealthtracker[today] = totalwealth
}
plot(wealthtracker, type='l')


# Now simulate many different possible trajectories
sim1 = do(1000)*{
	totalwealth = 10000
	weights = c(0.2, 0.2, 0.2, 0.2, 0.2)
	holdings = weights * totalwealth
	wealthtracker = rep(0, horizon) # Set up a placeholder to track total wealth
	for(today in 1:horizon) {
		return.today = resample(myreturns, 1, orig.ids=FALSE)
		holdings = holdings*(1+return.today)
		totalwealth = sum(holdings)
		wealthtracker[today] = totalwealth
	}
	wealthtracker
}

# Final wealth
hist(sim1[,20])
mean(sim1[,20])
sd(sim1[,20])

# Profit/loss
hist(sim1[,20] - 10000, breaks=50)



