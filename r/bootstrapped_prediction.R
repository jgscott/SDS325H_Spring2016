library(mosaic)

newspapers = read.csv("../data/newspapers.csv", header=TRUE)

# Plot the data
plot(Sunday~Daily,data=newspapers)
lm1 = lm(Sunday~Daily,data=newspapers)
abline(lm1)


# Values of x where we want to predict
# Here, just one x
xstar = data.frame(Daily=1000)

# Bootstrapped prediction intervals
# Key idea: simulate future data points
NMC = 10000
boot_pred = do(NMC)*{
  
  # Step 1a: Fit a model to a bootstrapped sample
  lmsub = lm(Sunday~Daily,data=resample(newspapers))
  
  # Step 1b: Form the plug-in prediction from the bootstrapped model.
  yhat = predict(lmsub,newdata=xstar)
  
  # Resample the residuals from the bootstrapped model
  # This step bakes in residual uncertainty
  eps = sample(resid(lmsub), size=1, replace=TRUE)
  
  # Form and return the notional future data points
  ystar = yhat+eps
  ystar
}

# Look at the first few lines
head(boot_pred)

# Our sampling distribution for our prediction
hist(boot_pred$X1)
sd(boot_pred$X1)

# Boot-strapped prediction interval
confint(boot_pred$X1, level=0.95)

###
## Now for many future points at once
###

# Values of x where we want to predict
xstar = data.frame(Daily=seq(130,1210,by=10))
m = nrow(xstar)  # number of points to predict

# Bootstrapped prediction intervals
NMC = 10000
boot_pred = do(NMC)*{
	
  # Step 1a: Fit a model to a bootstrapped sample
  lmsub = lm(Sunday~Daily,data=resample(newspapers))
  
  # Step 1b: Form the plug-in prediction from the bootstrapped model.
  # This time, ystar is a vector corresponding to predictions
  # for all the values in xstar.
  yhat = predict(lmsub,newdata=xstar)
	
	# Resample the residuals from the bootstrapped model
	# This step bakes in residual uncertainty
	eps = sample(resid(lmsub), size=m, replace=TRUE)
	
	# Form and return the notional future data points
	ystar = yhat+eps
	ystar
}

head(boot_pred)

# Plug-in prediction
yhat = predict(lm1, newdata=xstar)

# Prediction standard errors
yse = apply(boot_pred, 2, sd)

# 95% prediction interval for every point
ci_bounds = apply(boot_pred, 2, quantile, probs=c(0.025, 0.975))

# View the bounds
ci_bounds

# View the bounds in columns
t(ci_bounds)

# Stack these columns next to the corresponding values of xstar for easy reference
cbind(xstar, t(ci_bounds))

# Compare the half-width of interval versus that of the naive prediction interval
# Notice the Monte Carlo variability (try with NMC = 100000)
plot(xstar$Daily, yse, pch=19, col='red')
abline(h=sd(lm1$residuals), col='blue', lty='dashed')



#######
##### Optional pretty plot
#######

par(mar=c(4,4,1,1))
plot(Sunday~Daily,data=newspapers,
	bty='n', axes=FALSE, xlab="Circulation on Monday to Friday (thousands of papers)", ylab="Circulation on Sunday (thousands of papers)", main='', cex.lab=0.9, cex=0.5, pch=19, col='blue',
	type='n')
axis(1,at = seq(150,1200,by=50), font=2, cex.axis=0.75, tick=FALSE)
axis(2,at = seq(200,1800,by=100), font=2, cex.axis=0.75, tick=FALSE, las=1)
lm1 = lm(Sunday~Daily,data=newspapers)


polygon(c(xstar$Daily, rev(xstar$Daily)),c(yhat+2*yse, rev(yhat-2*yse)), border=NA, col=grey(0.95))
polygon(c(xstar$Daily, rev(xstar$Daily)),c(yhat+1*yse, rev(yhat-1*yse)), border=NA, col=grey(0.85))
#points(Sunday~Daily,data=newspapers,pch=19, cex=0.3,col='blue')
pointLabel(newspapers$Daily, newspapers$Sunday, labels=as.character(1:N), font=2, offset=0.1, cex=0.7, col='blue')
#text(Sunday~Daily,data=newspapers,font=2,labels=1:N, pos=sample(1:4,N,replace=TRUE), cex=0.7,col='blue')
#abline(lm1)


### Another pretty plot

par(mar=c(4,4,0,1))
plot(xstar$Daily, yse, bty='n', axes=FALSE, xlab='', ylab='Half-width of prediction interval', main='', pch=19,
	col=rgb(150,0,0,50,maxColorValue=256), cex.lab=0.9)
axis(1,at = seq(150,1200,by=50), font=2, cex.axis=0.75, tick=FALSE)
axis(2,at = seq(100,120,by=2), font=2, cex.axis=0.75, tick=FALSE, las=1)
lines(lowess(xstar$Daily, yse, f=1/3))
abline(h=sd(lm1$residuals), col='blue', lty='dashed')
text(150,108.3,"Half-width of naive prediction interval", pos=4, font=2, cex=0.75)

