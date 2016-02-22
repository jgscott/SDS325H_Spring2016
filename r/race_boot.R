library(mosaic)
data(TenMileRace)

plot(net ~ age, data=TenMileRace)
plot(net ~ age,  data=TenMileRace,
	pch=19, col=rgb(0.5,0.5,0.5,0.2))

# Fit the model with no dummies or interactions and bootstrap
lm1 = lm(net ~ age, data=TenMileRace)
boot1 = do(1000)*{
	lm(net~age, data=resample(TenMileRace))
}
coef(lm1)

# Uncertainty about the age slope
hist(boot1$age)
sd(boot1$age)

# Bootstrapped confidence intervals
confint(boot1)


# Now a model with a dummy variable and interaction
lm2 = lm(net ~ age + sex + age:sex, data=TenMileRace)
# Looks like men get slower as they age at a faster rate than women (1.6 s/year difference)
coef(lm2)

# Quantify uncertainty
boot2 = do(1000)*{
	lm(net ~ age + sex + age:sex, data=resample(TenMileRace))
}

hist(boot2$age)
hist(boot2$sexM)

# Pay particular attention here:
# what is the strength of the evidence that 
# there is a differential rate of slowing for men/women?
hist(boot2$age.sexM)

