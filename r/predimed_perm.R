library(mosaic)

# Read in the data
predimed = read.csv("../data/predimed.csv", header=TRUE)

summary(predimed)

# Contingency table and a table of proportions
t1 = xtabs(~event + group, data=predimed)
t1
p1 = prop.table(t1, margin=2)
p1

# The mean rate of events across all participants
mu_grand = mean(predimed$event=="Yes")

# By group
mu_group = mean(predimed$event=="Yes" ~ group, data=predimed)

# My test statistic: the sum of absolute differences
# between group means and grand mean
delta = sum(abs(p1[2,] - mu_grand))

# The null hypothesis: no systematic differences between group means
# i.e. diet doesn't matter for rate of events

# Permutation test: shuffle the predictor (diet) and see what delta looks like
# i.e. calculate the sampling distribution of Delta under H_0
perm1 = do(1000)*{
	# shuffle the data
	t_shuff = xtabs(~event + shuffle(group), data=predimed)
	p_shuff = prop.table(t_shuff, margin=2)
	# calculate the test statistic for the shuffled data
	sum(abs(p_shuff[2,] - mu_grand))
}

# Inspect p(Delta | H_0)
hist(perm1$result)
# Calculate rejection region at alpha = 0.05 level
qdata(perm1$result, 0.95)

# My delta doesn't fall in the rejection region
abline(v=delta, col='red', lwd=2)



######
# Chi-squared test
# Be careful here!
# can't assume in-built functions are doing what you expect
######

# chi^2 = sum(  (observed - expected)^2/expected ) 
# where expected counts are under the null hypothesis

observed_counts = xtabs(~event + group, data=predimed)  # same as t1 above

# To calculate expected counts, we must recognize that people were allocated to 
# treatment arms by the experimenters.
# Thus expected counts must sum to the same numbers over columns as observed counts.
column_totals = colSums(observed_counts)

# under null hypothesis, everybody has the same probability of an event.
# This probability is given by mu_grand, above
expected_counts = rbind(column_totals*(1-mu), column_totals*mu)

# Double check allocations to each treatment arm
colSums(expected_counts)
colSums(observed_counts)

# Now calculate the distribution of the chi-squared statistic under permutation
perm2 = do(5000)*{
	# shuffle the data
	t_shuff = xtabs(~event + shuffle(group), data=predimed)
	sum( ((t_shuff - expected_counts)^2)/expected_counts )
}


# Inspect the sampling distribution
hist(perm2$result, 20)

# calculate critical value at alpha=0.05
qdata(perm2$result, 0.95)


# Check your observed test statistic
my_chisq = sum( ((observed_counts - expected_counts)^2)/expected_counts )
my_chisq
abline(v=my_chisq, col='red')


# p-value?
1-pdata(perm2$result, my_chisq)

# Compare with result from chisq.test
chisq.test(observed_counts)

# show a density histogram
# with chi-squared density superimposed
hist(perm2$result, 30, prob=TRUE)
curve(dchisq(x, 2), add=TRUE)
