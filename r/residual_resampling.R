library(mosaic)

# Nonparametric bootstrap
heights = read.csv("../data/heights.csv")

plot(SHGT ~ MHGT, data=heights)

lmM = lm(SHGT ~ MHGT, data=heights)
abline(lmM)
lmF = lm(SHGT ~ FHGT, data=heights)

N = nrow(heights)

NMC = 500
betasave = matrix(0, NMC, 2)
for(i in 1:NMC) {
	keep = sample(1:N, N, replace=TRUE)
	lmstar = lm(SHGT ~ MHGT, data=heights[keep,])
	#plot(SHGT ~ MHGT, data=heights[keep,], xlim=c(50,75), ylim= c(60,80))
	#abline(lmstar)
	betasave[i,] = coef(lmstar)
}

hist(betasave[,1])
hist(betasave[,2])


########
# Residual-resampling bootstrap
########

chym = read.csv("../data/chymotrypsin.csv")
plot(Rate ~ Conc, data=chym)

# Michaelis-Menten kinetics
# see https://en.wikipedia.org/wiki/Michaelis–Menten_kinetics

# This function predictions the reaction rate as a function of:
# @x: concentration
# @vmax, km: M-M kinetic parameters
mmpredict = function(x, vmax, km) {
	vmax*x/{km+x}
}

# This defines a target function to be optimized
# to fit a Michaelis-Menten equation to data.
# @theta = (log vmax, log km), the system parameters to be optimized over
# @x: observed vector of concentrations
# @y: observed vector of reaction rates
target = function(theta, x, y) {
	vmax = exp(theta[1])
	km = exp(theta[2])
	ypred = mmpredict(x, vmax, km)
	sum(0.5*{y-ypred}^2)
}

# Optimize the function
mymax = optim(c(0,0), target, x = chym$Conc, y = chym$Rate, method='Nelder-Mead')
mymax  # see ?optim
theta = mymax$par

# Data and fitted curve
plot(Rate ~ Conc, data=chym)
curve(mmpredict(x, exp(theta[1]), exp(theta[2])), add=TRUE)

### Now bootstrap
yhat = mmpredict(chym$Conc, exp(theta[1]), exp(theta[2]))
eps = chym$Rate - yhat
N = nrow(chym)

NMC = 250
thetasave = matrix(0, NMC, 2)

for(i in 1:NMC) {
	estar = sample(eps, N, replace=TRUE)
	ystar = yhat + estar
	mymax = optim(c(0,0), target, x = chym$Conc, y = ystar)
	thetastar = mymax$par
	plot(chym$Conc, ystar,ylim=c(1,2.1))
	curve(mmpredict(x, exp(thetastar[1]), exp(thetastar[2])), add=TRUE)
	thetasave[i,] = thetastar
}

# Inspect the sampling distributions
hist(exp(thetasave[,1]))
hist(exp(thetasave[,2]))
plot(Rate ~ Conc, data=chym)
curve(mmpredict(x, exp(theta[1]), exp(theta[2])), add=TRUE)

apply(thetasave,2,sd)

