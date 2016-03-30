currentwealth = 1000
rounds = 10000

frac = 0.02
runningwealth = rep(0, rounds) # a placeholder for the result

# Simulate 10000 rounds of betting
for(i in 1:rounds) {
  bet = frac*currentwealth
  coin = sample(c(1,-1), 1, prob = c(0.52, 0.48))
  currentwealth = currentwealth + coin*bet
  runningwealth[i] = currentwealth
}
plot(runningwealth)



#### Plot means and variances

delta = p - (1-p)

mux = function(x, c) (1 + delta*c)*x
varx = function(x, c) p*{(1-delta)*c*x}^2 + (1-p)*{(-1-delta)*c*x}^2
sdx = function(x, c) sqrt(varx(x,c))

w0 = 10000

c_grid = seq(0.005,0.5,by=0.005)
interest_grid = delta*c_grid
mu_grid = mux(w0, c_grid)
sig_grid = sdx(w0, c_grid)

plot(mu_grid, sig_grid/mu_grid)
plot(sig_grid)

