georgia2000 = read.csv('../data/georgia2000.csv')
names(georgia2000)

# First create an undercount variable as a percentage
# undercount divided by total number of ballots
georgia2000$undercount = 100*(georgia2000$ballots - georgia2000$votes)/georgia2000$ballots

# Create a variable for the Republican vote share
georgia2000$repshare = georgia2000$bush/georgia2000$ballots

# Exploratory analysis: plots help a lot
boxplot(undercount ~ equip, data=georgia2000)
boxplot(undercount ~ poor, data=georgia2000)
boxplot(undercount ~ urban, data=georgia2000)
boxplot(undercount ~ atlanta, data=georgia2000)
plot(undercount ~ perAA, data=georgia2000)
plot(undercount ~ repshare, data=georgia2000)


# One strategy is to start with a model and try to prune
lm1 = lm(undercount ~ poor + urban + atlanta + perAA + repshare + equip, data=georgia2000)
summary(lm1)
anova(lm1)
confint(lm1)

# Notice that perAA and repshare are highly correlated
plot(perAA ~ repshare, data=georgia2000)

# We will drop one of these variables
lm2 = lm(undercount ~ poor + urban + atlanta + repshare + equip, data=georgia2000)
summary(lm2)
anova(lm2)

# The effect of equipment seems present... what happens if we drop it?
lm3 = lm(undercount ~ poor + urban + atlanta + repshare, data=georgia2000)
summary(lm3)
anova(lm3)

# R-sqared goes down by 7%... is this "significant" or not?
# New idea: use a permutation test
# (not part of the homework!)

# Shuffle the equipment variable once
lm_perm = lm(undercount ~ poor + urban + atlanta + repshare + shuffle(equip), data=georgia2000)
summary(lm_perm)

# Now repeat
perm_test = do(10000)*{
  lm_perm = lm(undercount ~ poor + urban + atlanta + repshare + shuffle(equip), data=georgia2000)
  lm_perm
}

head(perm_test)
hist(perm_test$r.squared)
abline(v = 0.2729, col='red')
# Compare with R-squared from the model with the real equipment variable
summary(lm2)

# Compute alpha
sum(perm_test$r.squared > 0.25)


# Compute a p-value
sum(perm_test$r.squared > 0.2729)


### A bug in mosaic

undercount = ((georgia2000$ballots-georgia2000$votes)/georgia2000$ballots)*100

georgia2000$republicanlev = georgia2000$bush/georgia2000$ballots
percentAA = georgia2000$perAA *100

set.seed(12345)
boot1 = mosaic::do(1000)*{
	lm(undercount ~ poor + urban + atlanta + republicanlev + equip, data = resample(georgia2000))
}
hist(boot1$r.squared)

set.seed(12345)
boot2 = do(1000)*{
  lmboot = lm(undercount ~ poor + urban + atlanta + republicanlev +equip, data = resample(georgia2000))
	summary(lmboot)$r.squared
  }
hist(boot2$result)


confint(boot1)
hist(boot1$r.squared)
