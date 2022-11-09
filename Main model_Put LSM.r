

library(MASS) #Help to store data for the functions below
BasisFunct <- function(X, k) 
{
  m = length(X)
  if(k == 1)
  {
    A = cbind(rep(1,m), (1-X))
  }
  else if(k == 2)
  {
    A = cbind(rep(1,m), (1-X), 1/2*(2-4*X+X^2))
  }
  else if(k == 3)
  {
    A = cbind(rep(1,m), (1-X), 1/2*(2-4*X+X^2), 1/6*(6-18*X+9*X^2-X^3))
  }
  else if(k == 4)
  {
    A = cbind(rep(1,m), (1-X), 1/2*(2-4*X+X^2), 1/6*(6-18*X+9*X^2-X^3),1/24*(24-96*X+72*X^2-16*X^3+X^4))
  }
  else if(k == 5)
  {
    A = cbind(rep(1,m), (1-X), 1/2*(2-4*X+X^2), 1/6*(6-18*X+9*X^2-X^3),1/24*(24-96*X+72*X^2-16*X^3+X^4),1/120*(120-600*X+600*X^2-200*X^3+25*X^4-X^5))
  }
  else
  {
    return("error")
  }
  return(A)
}
LSM = function(TT, r, sigma, K, S0, N, M, k )
{
  dt = TT/N #time steps
  t = seq(0, TT, dt) #Time vector
  
  z = rnorm(M/2, 0, 1) #Generate distribution for random walk
  w = (r - sigma^2/2)*TT + sigma*sqrt(TT) * cbind(z, -z) #Random walk
  S = S0 * exp(w) #Stock price with random walk or Geometric Brownian Motion
  
  P_0 = K - S #Options payoff
  P = P_0
  P[P_0 < 0] = 0 #If payoff less than zero, then zero
  
  res = rep(1, N) #Replicates values
  for (i in N:2 ) {
    z = rnorm(M/2, 0, 1)
    w = t[i] * w/t[i+1] + sigma*sqrt(dt * t[i] / t[i+1] ) * cbind(z, -z)
    S = S0 * exp(w)
    
    index = which(K - S > 0) #ITM path
    X = S[index] #ITM price
    Y = P[index] * exp(-r * dt) #Discounted payoff
    A = BasisFunct(X, k)
    beta = ginv(A) %*% Y #Regression
    
    C = A %*% beta #Estimated value of continuation
    E = K-X
    exP = index[C<E] #Path where it's better to exercise
    
    rest = setdiff(1:M, exP) #Rest of the path
    P[exP] = E[C<E]
    P[rest] = P[rest] * exp(-r * dt) #Discount payoff one step backward
    
    res[i] = mean(P * exp(-r * dt)) #Insert previous payoff and discount back one step
  }
  u = mean(P * exp(-r * dt)) #Output the option value
  return(u)
  
}
TT = 1 #Expiration time
r = 0.03 #Risk free rate
sigma = 0.15 #Volatility
K = 100 #Strike price
S0 = 100 #Asset price at period zero
N=10 #Number of time steps
M = 10^6 #Number of paths
k= 3 #Small k is the basis function
LSM(TT, r, sigma, K, S0, N, M, k )

