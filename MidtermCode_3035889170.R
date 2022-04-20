install.packages(pastecs)
library(pastecs)
get_return = function(p_final){
  # A common used function in this file
  # This function is to calculate the total payoff of the Structured Note
  r = 0
  if(p_final>=1.04){
    r = 0.12
  }
  else if(p_final>=1){
    r = 0.05
  }
  else{
    r = p_final-1+0.05
  }
  r
}

# ************************************************ #
#Part 1 -- Draw the Payoff Graph
draw_payoff = function(N=120){
  #This function is to draw the payoff graph
  l = seq(0,1.6,1.6/(N-1))
  rs = array(data=NA,dim=1)
  for(i in 1:N){
    r = get_return(l[i])
    rs[i] = r
  }
  plot(l,rs)
}
draw_payoff() # exercise the draw_payoff function
# ************************************************ #

# ************************************************ #
# Part 2 -- Monte-Carol Simulation
# The following part is a simple monte-carol simulation
mod_stock_price = function(z){
  # This function is to model the stock prices with GBM
  S = 1
  rf = 0.005
  sigma = 0.15
  T = 1
  mu = 0.08
  S_end = S*exp((mu-0.5*sigma^2)*T+sigma*sqrt(T)*z)
}

N = 10^5
Rs_ELN = array(data=NA,dim=1)
Rs_Stock = array(data=NA,dim=1)
set.seed(202204)
zs = rnorm(N,0,1)

for(i in 1:N){
  z = zs[i]
  S_end = mod_stock_price(z)
  R_Stock = log(S_end)
  R_ELN = get_return(S_end)
  Rs_Stock[i] = R_Stock
  Rs_ELN[i] = R_ELN
}
# ************************************************ #

# ************************************************ #
# Part 3 Figure and Statistics
#The following part is to draw histgram and do descriptive analysis
hist(Rs_Stock)
hist(Rs_ELN)
stat.desc(Rs_Stock)
stat.desc(Rs_ELN)
# ************************************************ #

# End