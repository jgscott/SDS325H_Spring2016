library(mosaic)
profs = read.csv("../data/profs.csv")

# Plots: ratings seem higher for men, for non-minorities,
# and for pretty people
plot(eval ~ beauty, data=profs)
boxplot(eval ~ minority, data=profs)
plot(eval ~ age, data=profs)
boxplot(eval ~ gender, data=profs)

# Ratings are lower for upper-division courses
# higher for single-credit courses
# higher for non-tenure-track teachers
# and higher for native English speakers
boxplot(eval ~ division, data=profs)
plot(eval ~ log(students), data=profs)
boxplot(eval ~ credits, data=profs)
boxplot(eval ~ tenure, data=profs)
boxplot(eval ~ native, data=profs)


# Let's build a model that incorporates all these effects
lm1 = lm(eval ~ native + tenure + credits + log(students) + gender + minority + beauty, data=profs)
anova(lm1)

# It looks like the beauty variable is the single largest contributor to the predictive abilities of the model

# We should check whether this finding is robust.
# What happens if we drop some of the more marginal variables?

# Class size looked like it had a small effect
lm2 = lm(eval ~ native + tenure + credits + gender + minority + beauty, data=profs)
anova(lm2)

# The tenure-track variable also has a small effect
# Try dropping it
lm3 = lm(eval ~ native + credits + gender + minority + beauty, data=profs)

perm2 = do(1000)*{
  lm(eval ~ native + tenure + credits + gender + minority + beauty + shuffle(tenure), data=profs)
}

hist(perm2$r.squared, 20)
abline(v = 0.1585, col='red', lwd=2) # r^2 from lm2


# What about the "beauty" effect?

perm3 = do(1000)*{
  lm(eval ~ native + tenure + credits + gender + minority + shuffle(beauty), data=profs)
}

