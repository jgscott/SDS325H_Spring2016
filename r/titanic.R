# Load in the relevant libraries
library(mosaic)

# Now load the TitanicSurvival.csv data set
# using the Import Dataset button in the Environment tab

# The variable names
names(TitanicSurvival)

# To view the data in RStudio,
# double click on it in the Environment tab,
# or just type the name of the data set.
TitanicSurvival

# Look at the first few lines
head(TitanicSurvival)

# Make tables that shows who survived, stratified by sex and cabin class
xtabs(~survived + sex, data=TitanicSurvival)
xtabs(~survived + passengerClass, data=TitanicSurvival)

# Now stratified by both variables
xtabs(~survived + sex + passengerClass, data=TitanicSurvival)

# Boxplot of age versus other variables
boxplot(age~survived, data=TitanicSurvival)
boxplot(age~survived + sex, data=TitanicSurvival)
boxplot(age~passengerClass + sex, data=TitanicSurvival)
boxplot(age~survived + passengerClass, data=TitanicSurvival)
