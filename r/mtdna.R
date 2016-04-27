library(mosaic)
library(lattice)
library(lme4)

mtdna = read.csv('../data/mtdna.csv', header=TRUE)
summary(mtdna)

# Recast the numerical codes as factors
mtdna$animal = factor(mtdna$animal)
mtdna$litter = factor(mtdna$litter)
mtdna$tr = factor(mtdna$tr)
summary(mtdna)


# We'll look at log2(copy)
boxplot(log2(copy) ~ tissue, data=mtdna)

# Fit a model for tissue group
lm1 = lm(log2(copy) ~ tissue-1, data=mtdna)
summary(lm1)


# Notice the observations are not independent.
# We have ignored correlation due to litter
boxplot(resid(lm1) ~ litter, data=mtdna)


# Fit a mixed model that partially pools:
# litter-level effects
hlm1 = lmer(log2(copy) ~ tissue  + (1 | litter), data=mtdna)
summary(hlm1)
r1 = ranef(hlm1, condVar=TRUE)
dotplot(r1)

# We can also account for animal-level correlations
boxplot(log2(copy) ~ animal:litter, data=mtdna)

hlm2 = lmer(log2(copy) ~ tissue + (animal | litter), data=mtdna)
summary(hlm2)

r2 = ranef(hlm2, condVar=TRUE)
dotplot(r2)
