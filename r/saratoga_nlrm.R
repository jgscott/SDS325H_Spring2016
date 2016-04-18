library(mosaic)
data(SaratogaHouses)

# scrub the houses with 0 lot size (because, huh?)
SaratogaHouses = subset(SaratogaHouses, lotSize > 0)

lm3 = lm(price ~ livingArea + log(lotSize) + fireplaces + fuel + fireplaces:fuel, data=SaratogaHouses)

# Standard errors and t statistics
summary(lm3)

# sequential F tests
anova(lm3)

# Compare p-value under F test with p value from permutation test
lm2 = lm(price ~ livingArea + log(lotSize) + fireplaces + fuel, data=SaratogaHouses)


# Shuffle the interactions but not the main effects:
perm3 = do(10000)*{
  lm(price ~ livingArea + log(lotSize) + fireplaces + fuel + shuffle(fireplaces):shuffle(fuel), data=SaratogaHouses)
}

hist(perm3$r.squared, 40)

# Show critical value for alpha = 0.05 rejection region
abline(v=qdata(perm3$r.squared, p = 0.95), col='blue', lwd=2)

# Show actual test statistic and calculate p value
abline(v=0.5185, col='red', lwd=2)
sum(perm3$r.squared > 0.5185)/10000


# show transformation of r2 into F statistic
fstat = (perm3$r.squared - rsquared(lm2))/(1-perm3$r.squared) * (nrow(SaratogaHouses) - 8)/(2)


hist(fstat, 50, prob=TRUE)
curve(df(x, nrow(SaratogaHouses) - 8, 2), add=TRUE)

