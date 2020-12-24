rm(list=ls())
cat("\014")

source("EX1.R")

# Graphs Erdos-Renyi of size n = 1000 with different values of the probability p from 0 to 1 with a step of 0.05
# for (i in seq(0,1,by=0.05)){
#   rand_er <- erdos_renyi(20, i)
#   plot(rand_er)
#   Sys.sleep(1)
# }


# A graph small-world of size n = 1000 with p = 0:1 and m = 2

rand_ws <- watts_strogats(50, 0.5, 2)
plot(rand_ws)

