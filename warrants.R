
#calculate days in between
DATA = data.frame((c("8/2/2021",  "11/12/2021")))
names(DATA)[1] = "V1"
date = as.Date(DATA$V1, format="%m/%d/%Y")
print(date-date[1])

#Warrants pricing with BS
#鴻海兆豐09購08鴻海20210928美購

Call <- function(S, K, r, q, T, sigma, ratio) {
  d1  <-  (log(S/K) + ((r-q) + sigma^2/2)*T) / (sigma*sqrt(T))
  d2  <-  d1 - sigma*sqrt(T) S * pnorm(d1)  - K*exp(-r*T)*pnorm(d2)
}

S     <- 109.5
K     <- 148
r     <- 0.0076
q     <- 0
T     <- 0.5  #assume 5days/250-trading day convention     
ratio <- 0.1
sigma <- 0.59

d1  <-  (log(S/K) + ((r-q) + sigma^2/2)*T) / (sigma*sqrt(T))
d2  <-  d1 - sigma*sqrt(T)
p   <-  cbind(S, K, T, sigma,  p = -S*exp(-q*T) * pnorm(-d1) + K*exp(-r*T)*pnorm(-d2))  
c   <-  cbind(S, K, T, sigma,  c =  S*exp(-q*T) * pnorm(d1)  - K*exp(-r*T)*pnorm(d2))   
exerciseratio <- 1*ratio

realc<-c*ratio


#國泰金麥證0B購01國泰金20211115歐購

DATA = data.frame((c("5/21/2021",  "11/12/2021")))
names(DATA)[1] = "V1"
date = as.Date(DATA$V1, format="%m/%d/%Y")
print(date-date[1])


Call <- function(S, K, r, q, T, sigma, ratio) {
  d1  <-  (log(S/K) + ((r-q) + sigma^2/2)*T) / (sigma*sqrt(T))
  d2  <-  d1 - sigma*sqrt(T) S * pnorm(d1)  - K*exp(-r*T)*pnorm(d2)
}

S     <- 54.2
K     <- 55
r     <- 0.0076
q     <- 0
T     <- RD  #assume 5days/250-trading day convention     
ratio <- 0.001
sigma <- 0.39

d1  <-  (log(S/K) + ((r-q) + sigma^2/2)*T) / (sigma*sqrt(T))
d2  <-  d1 - sigma*sqrt(T)
p   <-  cbind(S, K, T, sigma,  p = -S*exp(-q*T) * pnorm(-d1) + K*exp(-r*T)*pnorm(-d2))  
c   <-  cbind(S, K, T, sigma,  c =  S*exp(-q*T) * pnorm(d1)  - K*exp(-r*T)*pnorm(d2))   
exerciseratio <- 1*ratio

realc<-c*ratio