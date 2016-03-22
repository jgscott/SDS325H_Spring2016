# Using R 3.2.3 (GUI 1.66) Mavericks build (7060)
# mosaic version 0.13.0

library(mosaic)

georgia2000 = read.csv('http://jgscott.github.io/STA371H_Spring2016/data/georgia2000.csv', header=TRUE)

# A bug in cases where resampling leads to NA's
undercount = ((georgia2000$ballots-georgia2000$votes)/georgia2000$ballots)*100
georgia2000$republicanlev = georgia2000$bush/georgia2000$ballots
percentAA = georgia2000$perAA *100

# This was an analysis tried by a student.
# It produces a spurious sampling distribution of model parameters
set.seed(12345)
boot1 = do(1000)*{
	lm(undercount ~ poor + urban + atlanta + republicanlev + equip, data = resample(georgia2000))
}

# The behavior is most obvious for R^2.
hist(boot1$r.squared)

# I conjecture this in caused in situations where optical voting machines
# are not included in a bootstrapped sample, leading to an NA for that equip coefficient.
# do() may not be aggregating this appropriately.
summary(georgia2000$equip)

# Notice how some row entries are shifted to the left by one column
# Thus the F statistic becomes part of the R^2 column, and so forth.
head(boot1, 20)

# Calculating r-squared from the summary() function does not produce the bug
set.seed(12345)
boot2 = do(1000)*{
  lmboot = lm(undercount ~ poor + urban + atlanta + republicanlev +equip, data = resample(georgia2000))
	summary(lmboot)$r.squared
}
hist(boot2$result)

