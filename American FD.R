

library(RQuantLib)

AmericanOption('put', underlying=100, strike=100,dividendYield=0,
               riskFreeRate=0.03, maturity=1, volatility=0.15)


AmericanOption('call', underlying=100, strike=100,dividendYield=0,
               riskFreeRate=0.03, maturity=1, volatility=0.15)