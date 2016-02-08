library(mosaic)
library(faraway)

### Load in the data on mammalian sleep patterns from the library faraway
data(mammalsleep)


# plot the data and fit a linear model on log log scale
plot(log(brain) ~ log(body), data=mammalsleep)
lm1 = lm(log(brain) ~ log(body), data=mammalsleep)
abline(lm1)


# Plug-in prediction
coef(lm1)
new_furry_critter = 10   # 10 kilograms

# make a prediction on the log scale
log_yhat = 2.1347887  + 0.7516859*log(new_furry_critter)


# This is better
new_animals = data.frame(body = 10)
log_yhat = predict(lm1, newdata = new_animals)


#####
# Point prediction on the log scale
#####

plot(log(brain) ~ log(body), data=mammalsleep)
abline(lm1)
points(log(new_furry_critter), log_yhat, col='red', pch=19)

#####
# Point prediction on the original scale
#####
yhat = exp(log_yhat)

# Visualize on the original scale: whole data set
beta_hat = coef(lm1)
plot(brain ~ body, data=mammalsleep)
curve(exp(beta_hat[1]) * x^(beta_hat[2]), add=TRUE)
points(new_furry_critter, yhat, col='red', pch=19)

# Visualize on the original scale: zoomed in
plot(brain ~ body, data=mammalsleep, xlim=c(0, 25), ylim=c(0, 200))
curve(exp(beta_hat[1]) * x^(beta_hat[2]), add=TRUE)
points(new_furry_critter, yhat, col='red', pch=19)

#####
# Interval prediction on the log scale
#####

# Extract the residual standard deviation
std_e = sd(resid(lm1))  

# form upper and lower bound_logs based on point prediction
alpha = 2
l_bound_log = log_yhat - alpha*std_e
u_bound_log = log_yhat + alpha*std_e

l_bound_log
u_bound_log

# Visualize the interval
plot(log(brain) ~ log(body), data=mammalsleep)
abline(lm1)

points(log(new_furry_critter), log_yhat, col='red', pch=19)
points(log(new_furry_critter), l_bound_log, col='blue', pch=19)
points(log(new_furry_critter), u_bound_log, col='blue', pch=19)

# We can visualize the whole family of intervals
plot(log(brain) ~ log(body), data=mammalsleep)
abline(lm1)
abline(a = beta_hat[1] - alpha*std_e, b = beta_hat[2], lty='dotted')
abline(a = beta_hat[1] + alpha*std_e, b = beta_hat[2], lty='dotted')

# Calculate the empirical coverage rate of the interval
# Easiest to do on a residual plot
plot(resid(lm1) ~ log(body), data=mammalsleep)
abline(h = -alpha*std_e, lty='dotted')
abline(h = alpha*std_e, lty='dotted')

# How many residuals exceed the alpha*std_e tolerance?
sum(abs(resid(lm1)) > alpha*std_e)
sum(abs(resid(lm1)) > alpha*std_e) / length(resid(lm1))

#####
# Interval prediction on the original scale
#####

# Key idea: just transform the lower and upper bounds back to original scale
l_bound = exp(l_bound_log)
u_bound = exp(u_bound_log)

# Visualize on the original scale: zoomed in
plot(brain ~ body, data=mammalsleep, xlim=c(0, 25), ylim=c(0, 200))
curve(exp(beta_hat[1]) * x^(beta_hat[2]), add=TRUE)
points(new_furry_critter, yhat, col='red', pch=19)
points(new_furry_critter, l_bound, col='blue', pch=19)
points(new_furry_critter, u_bound, col='blue', pch=19)
