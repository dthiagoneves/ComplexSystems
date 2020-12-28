rm(list=ls())
cat("\014")

## 1 Implements a function that returns a matrix where we will have the vote of each person at each moment
source("EX1.R")
# RED = 1
# BLUE = 0



## 2 Represent the evolution of the voting rate to 1 for dierent graph
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
# print_result(vote_matrix, n, t)



## 2B a Watts-Strogatz graph with p = 0,01
# watts_strog <- generate_graph(2, n, 0.01)
# vote_matrix <- vote_evoution(watts_strog, e, t, show_steps = FALSE)
# if(n < 100){
#   print(vote_matrix[,t])
# }
# 
# print_result(vote_matrix, n, t)



## 2C a graph scale free with k = 3 and m = 2 (see TP2).
# scale_free <- generate_graph(3, n, 0.01)
# vote_matrix <- vote_evoution(scale_free, e, t, show_steps = FALSE)
# if(n < 100){
#   print(vote_matrix[,t])
# }
# 
# print_result(vote_matrix, n, t)


## 3 Now, you will take a graph scale free G of size n = 500. We are interested in the presidential elections.
source("EX3.R")
n = 500
t = 500
scale_free <- generate_graph(3, n, 0.01)
starter_unbiased <- rbinom(n, 1, 0.5)

# plot(scale_free, vertex.size = my_size, vertex.label.cex = 0.65 , vertex.label.color = "black", layout = layout_G)


## 3A Your job is to convince 10 people to vote for Mickey.
# RED = 1 Mickey
# BLUE = 0 Robert
cat("\n ===== Unbiased ===== \n")
vote_unbiased <- vote_biased(scale_free, e, t, starter_unbiased, visualise = FALSE)
print_result(vote_unbiased, n, t)

cat("\n ===== Degrees ===== \n")
starter_dgr <- modify_starter(scale_free, starter_unbiased, metric = 0)
vote_dgr <- vote_biased(scale_free, e, t, starter_dgr, visualise = FALSE)
print_result(vote_dgr, n, t)

cat("\n ===== Clustering Coefficient ===== \n")
starter_ccoef <- modify_starter(scale_free, starter_unbiased, metric = 1)
vote_ccoef <- vote_biased(scale_free, e, t, starter_ccoef, visualise = FALSE)
print_result(vote_ccoef, n, t)

cat("\n ===== Average Length of the Agents ===== \n")
starter_avgl <- modify_starter(scale_free, starter_unbiased, metric = 2)
vote_avgl <- vote_biased(scale_free, e, t, starter_avgl, visualise = FALSE)
print_result(vote_avgl, n, t)

cat("\n ===== Closeness Centrality ===== \n")
starter_ccen <- modify_starter(scale_free, starter_unbiased, metric = 3)
vote_ccen <- vote_biased(scale_free, e, t, starter_ccen, visualise = FALSE)
print_result(vote_ccen, n, t)


## Plot Initial Graphs
par(mfrow=c(2,2))
layout_G = layout_with_fr(scale_free)

V(scale_free)$color <- ifelse(starter_unbiased == 1, "red", "blue")
my_size <- 10 * (500 / n)
plot(scale_free, vertex.size = my_size, vertex.label=NA, layout = layout_G)
title("Unbiased")

V(scale_free)$color <- ifelse(starter_dgr == 1, "red", "blue")
my_size <- degree(scale_free, normalized = TRUE) * 250 * (500 / n)
plot(scale_free, vertex.size = my_size, vertex.label=NA, layout = layout_G)
title("Degrees")

V(scale_free)$color <- ifelse(starter_ccoef == 1, "red", "blue")
my_size <- transitivity(scale_free, type="local") * 25 * (500 / n)
plot(scale_free, vertex.size = my_size, vertex.label=NA, layout = layout_G)
title("Clustering Coefficient")

V(scale_free)$color <- ifelse(starter_ccen == 1, "red", "blue")
my_size <- closeness(scale_free, normalized = TRUE) * 25 * (500 / n)
plot(scale_free, vertex.size = my_size, vertex.label=NA, layout = layout_G)
title("Closeness Centrality")
