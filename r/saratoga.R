library(mosaic)
data(SaratogaHouses)

# scrub the houses with 0 lot size (because, huh?)
SaratogaHouses = subset(SaratogaHouses, lotSize > 0)


# price versus fireplaces
boxplot(price~fireplaces, data=SaratogaHouses)
lm0 = lm(price~fireplaces, data=SaratogaHouses)
coef(lm0)

# notice potential for confounding due to size of house
# (which also affects price)
boxplot(livingArea ~ fireplaces, data=SaratogaHouses)
boxplot(log(lotSize) ~ fireplaces, data=SaratogaHouses)


# Fit a multiple regression model incorporating these effects
# to estimate the partial relationship between price and #fireplaces
lm1 = lm(price ~ livingArea + log(lotSize) + fireplaces, data=SaratogaHouses)
coef(lm1)

# Bootstrap to get uncertainty estimates
boot1 = do(10000)*{
	lm(price ~ livingArea + log(lotSize) + fireplaces, data=resample(SaratogaHouses))
}
confint(boot1)

# residual plot versus fireplaces
boxplot(resid(lm1) ~ fireplaces, data=SaratogaHouses)

# Actual versus fitted values to check overall assumption of linearity
plot(price ~ fitted(lm1), data=SaratogaHouses)
abline(a=0,b=1)

# What about heating system?
boxplot(price ~ fuel, data=SaratogaHouses)
mean(price ~ fuel, data=SaratogaHouses)

# Incorporate these in a model
lm2 = lm(price ~ livingArea + log(lotSize) + fireplaces + fuel, data=SaratogaHouses)
coef(lm2)

# permutation test for the null hypothesis of no partial
# relationship between price and heating system
perm2 = do(10000)*{
	lm(price ~ livingArea + log(lotSize) + fireplaces + shuffle(fuel), data=SaratogaHouses)
}
head(perm2)
qdata(perm2$r.squared, p = 0.95)

# Show histogram
hist(perm2$r.squared, 40)

# Show critical value for alpha = 0.05 rejection region
abline(v=qdata(perm2$r.squared, p = 0.95), col='blue', lwd=2)

# Show observed test statisticL in rejection region
summary(lm2)  # notice R-squared
abline(v= 0.5182, col='red', lwd=2)


# What about an interaction between fireplaces and heating system?
lm3 = lm(price ~ livingArea + log(lotSize) + fireplaces + fuel + fireplaces:fuel, data=SaratogaHouses)
summary(lm3)  # notice R-squared

# Shuffle the interactions but not the main effects:
perm3 = do(10000)*{
	lm(price ~ livingArea + log(lotSize) + fireplaces + fuel + shuffle(fireplaces):shuffle(fuel), data=SaratogaHouses)
}

hist(perm3$r.squared, 40)

# Show critical value for alpha = 0.05 rejection region
abline(v=qdata(perm3$r.squared, p = 0.95), col='blue', lwd=2)

# Show actual test statistic: not in rejection region
abline(v=0.5185, col='red', lwd=2)

