rm(list=ls())
cat("\014")

A <- list(c(2,3,4,5),c(3),c(4),c(5),c(2))

## 2 Representations of graph and first metrics
source("EX2.R")


## 3 Breadth-first search algorithm (BFS) and applications
# source("EX3.R")

## 4 With the package Igraph
source("EX4.R")

gg <- adjmatrix(A, is_directed = FALSE)
adj_gg <- as_adj_list(gg)

plot(gg)

# Degrees
print("List of Degrees")
dgrs = degree(gg)
print(dgrs)
print(degrees_list(adj_gg))

# print(degree_distribution(gg))

# Clustering Coefficients
print("-----------------------")
print("Clustering Coefficients")
clst_coeff = transitivity(gg, type="local")
print(clst_coeff)
print(clustering_coef(adj_gg))

# Closeness centrality
print("--------------------")
print("Closeness centrality")
clsness = closeness(gg)
print(clsness)

# Betweenness centrality
print("----------------------")
print("Betweenness centrality")
btwnness = betweenness(gg)
print(btwnness)


# Represent the graphs where the color or the size of the nodes depend on these values.
library(RColorBrewer)
coul  <- brewer.pal(max(dgrs), "Set1")
my_color <- coul[as.numeric(as.factor(dgrs))]
my_size = round(btwnness * 30, digits = 0)

# V(g)$color <- ifelse(V(g)$Q1_I1 == 1, "lightblue", "orange")

plot(gg, vertex.color=my_color, vertex.size = my_size)
legend("bottomleft", legend=levels(as.factor(dgrs))  , col = coul , bty = "n", pch=20 , pt.cex = 3, cex = 1.5, text.col=coul , horiz = FALSE, inset = c(0.1, 0.1))