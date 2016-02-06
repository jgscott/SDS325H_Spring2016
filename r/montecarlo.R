library(mosaic)

#### Four ways to do a Monte Carlo simulation in R


#####
# 1) The most intuitive way if you've never done this before.
# Uses "syntactic sugar" from the mosaic library
# Feel free to stop here at method 1!
#####


# Here is something we do once: generate a random uniform number
runif(1)

# Save the result in a new variable called x
x = runif(1)
x

# Here is a way to do that same thing 100 times
do(100)*{
	runif(1)
}

# Save the result in a new variable called MonteCarlo
MonteCarlo = do(1000)*{
	runif(1)
}
hist(MonteCarlo$result)


# We can do anything we want inside the braces.
# The last line is what gets aggregated.
MonteCarlo = do(1000)*{
	u = runif(1)
	x = cos(exp(2+u))
	x
}
hist(MonteCarlo$result)


#####
# 2) The most "R-native" way;
# no extra libraries involved
#####


# here is a for loop in R
for(i in 1:10) {
	print(i)
}

# for loops in R do not have a return value.
# Therefore we must pre-allocate a variable to store computations.

NMC = 1000
result = rep(0, NMC)
for(i in 1:NMC) {
	result[i] = i
}
result

# Slightly more interesting
result = rep(0, NMC)
for(i in 1:NMC) {
	result[i] = runif(1)
}
result

# The above is actually just a dumber version of this
# the runif function is C native and therefore much faster
result = runif(1000)



#####
# 3) My favorite pure-R way: for loops with return values
#####


# Install and load the foreach library
library(foreach)

# foreach loops have return values
# Here's the syntax
result = foreach(i = 1:1000) %do% {
	runif(1)
} 

# By default, foreach aggregates results in a list
# To aggregate in a vector, pass the following flag

result = foreach(i = 1:1000, .combine = 'c') %do% {
	runif(1)
} 
hist(result)

# You can also aggregate vectors
result = foreach(i = 1:1000, .combine = 'rbind') %do% {
	c(runif(1, min=0, max=1), rnorm(1))
} 
hist(result[,1])
hist(result[,2])


# Here's one reason this way is my favorite
library(parallel)
library(doMC)
registerDoMC(parallel::detectCores())  # function is in the parallel namespace

# Pretty automatic multi-core simulations
# check your processor load as this executes
result = foreach(i = 1:1000, .combine = 'c') %dopar% {
	runif(1)
} 
hist(result)


#####
# 4) Finally, if you know C++, here is an example of R's C++ API
# Compile your own mini C++ functions and call them directly from R
# This is much faster than a loop in any scripting language
# See http://www.rcpp.org for lots more detail
#####

library(Rcpp) # install this library first

cppFunction('
	NumericVector fastloop(int n) {
		NumericVector result(n, 0.0);
		for(int i=0; i<n; i++) {
			result[i] = R::runif(0, 10);
		}
		return result;
	}
')

mysim = fastloop(500000)
hist(mysim, breaks=200)

# compare timings

# 10000 simulated uniforms, the slow way
system.time({
	result <- foreach(i = 1:10000, .combine = 'c') %do% {
		runif(1)
	}
})

# 1 million simulated uniforms, each of two fast ways
system.time(mysim <- fastloop(1000000))
system.time(mysim <- runif(1000000))

