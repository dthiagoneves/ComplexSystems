rm(list=ls())
cat("\014")

## 2 Representations of graph and first metrics
source("EX2.R")


## 3 Breadth-first search algorithm (BFS) and applications
source("EX3.R")

## 4 With the package Igraph
source("EX4.R")

A <- list(c(2,3,4,5),c(3),c(4),c(5),c(2),c(5),c(6))

gg <- adjmatrix(A, is_directed = FALSE)
adj_gg <- as_adj_list(gg)
n <- length(adj_gg)

plot(gg)

# Degrees
print("List of Degrees")
dgrs = degree(gg)
print(dgrs)

dgrs_arr <- rep(0L, n)
dgrs_list <- degrees_list(adj_gg)
for(i in 1:n){
  dgrs_arr[i] <- dgrs_list[[i]]
}
print(dgrs_arr)

# print(degree_distribution(gg))

# Clustering Coefficients
print("-----------------------")
print("Clustering Coefficients")
clst_coeff = transitivity(gg, type="local")
print(clst_coeff)

clc_arr <- rep(0L, n)
clc_list <- clustering_coef(adj_gg)
for(i in 1:n){
  clc_arr[i] <- clc_list[[i]]
}

print(clc_arr)

# Closeness centrality
print("--------------------")
print("Closeness Centrality")
# clsness = closeness(gg, normalized = TRUE) # it normalized by (n-1) not (n) 
clsness = closeness(gg) * n
print(clsness)
print(closeness_centrality(adj_gg))

# Betweenness centrality
print("----------------------")
print("Betweenness Centrality")
btwnness = betweenness(gg, directed = FALSE)
print(btwnness)
print(betweenness_centrality(adj_gg))


# Represent the graphs where the color or the size of the nodes depend on these values.
library(RColorBrewer)
coul  <- brewer.pal(max(dgrs), "Set1")
my_color <- coul[as.numeric(as.factor(dgrs))]
my_size = round(clsness * (n-1) / (n) * 30, digits = 0)

# V(g)$color <- ifelse(V(g)$Q1_I1 == 1, "lightblue", "orange")

plot(gg, vertex.color=my_color, vertex.size = my_size)
legend("topleft", legend=levels(as.factor(dgrs))  , col = coul , bty = "n", pch=20 , pt.cex = 3, cex = 1.5, text.col=coul , horiz = FALSE, inset = c(0.1, 0.1))