rm(list=ls())
cat("\014")

## 1 Implements a function that returns a matrix where we will have the vote of each person at each moment
source("EX1.R")

## 2 Represent the evolution of the voting rate to 1 for dierent graph
## You will take graphs of size n = 500 and the time t = 3000. In all simulations, the simulation will be e = 0.01
source("EX2.R")

n = 500
t = 300
e = 0.01



## 2A a graph Erdos-Renyi with p = 0.05
# erdos_ren <- generate_graph(1, n, 0.05)
# vote_matrix <- vote_evoution(erdos_ren, e, t, show_steps = FALSE)
# if(n < 100){
#   print(vote_matrix[,t])
# }
# 
# print("--- Initial Situation ---")
# print("Red [%]")
# Red = sum(vote_matrix[,1])/n
# print(Red * 100)
# print("Blue [%]")
# print((1 - Red) * 100)
# 
# print("--- Final Situation ---")
# print("Red [%]")
# Red = sum(vote_matrix[,t])/n
# print(Red * 100)
# print("Blue [%]")
# print((1 - Red) * 100)



## 2B a Watts-Strogatz graph with p = 0,01
# watts_strog <- generate_graph(2, n, 0.01)
# vote_matrix <- vote_evoution(watts_strog, e, t, show_steps = FALSE)
# if(n < 100){
#   print(vote_matrix[,t])
# }
# 
# print("--- Initial Situation ---")
# print("Red [%]")
# Red = sum(vote_matrix[,1])/n
# print(Red * 100)
# print("Blue [%]")
# print((1 - Red) * 100)
# 
# print("--- Final Situation ---")
# print("Red [%]")
# Red = sum(vote_matrix[,t])/n
# print(Red * 100)
# print("Blue [%]")
# print((1 - Red) * 100)



## 2C a graph scale free with k = 3 and m = 2 (see TP2).
# scale_free <- generate_graph(3, n, 0.01)
# vote_matrix <- vote_evoution(scale_free, e, t, show_steps = FALSE)
# if(n < 100){
#   print(vote_matrix[,t])
# }
# 
# print("--- Initial Situation ---")
# print("Red [%]")
# Red = sum(vote_matrix[,1])/n
# print(Red * 100)
# print("Blue [%]")
# print((1 - Red) * 100)
# 
# print("--- Final Situation ---")
# print("Red [%]")
# Red = sum(vote_matrix[,t])/n
# print(Red * 100)
# print("Blue [%]")
# print((1 - Red) * 100)