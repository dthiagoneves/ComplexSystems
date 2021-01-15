rm(list=ls())
cat("\014")

source("EX1.R")
source("EX2.R")

execute_flag <- 1

library(igraph)

# Graphs Erdos-Renyi of size n = 1000 with different values of the probability p from 0 to 1 with a step of 0.05
if (execute_flag == 1){
  # For multiple graphs:
  # index <- 1
  # clust_coeff_vector <- rep(0L, 22)
  # a_length <- rep(0L, 22)
  # for (i in seq(0,1,by=0.05)){
  #   rand_er <- erdos_renyi(100, i)
  #   #plot(rand_er)
  #   #rand_er <- get_hist(rand_er)
  #   clustering_coefficient <- clustering_coef(rand_er)
  #   cat("Clustering Coefficient (CC) for Erdos-Renyi = ", i, "\nCC = ")
  #   #print(clustering_coefficient)
  #   print(i)
  #   clust_coeff_vector[index] <- clustering_coefficient[50]
  #   index <- index + 1
  #   
  #   a_length[index] <- average_length(rand_er)
  #   
  # }
  # print(length(clust_coeff_vector))
  # print(length(1:index))
  # plot(1:index, clust_coeff_vector,
  #      main="Clustering Coefficient in function of p",
  #      ylab="Clustering Coefficient",
  #      xlab = "Link Probability")
  # 
  # plot(1:index, a_length,
  #      main="Average Length in function of p",
  #      ylab="Average Length",
  #      xlab = "Link Probability")
  
  
  i = 0.35
  rand_er <- erdos_renyi(200, i)
  plot(rand_er)
  
  clustering_coefficient <- clustering_coef(rand_er)
  cat("Clustering Coefficient (ClustC) for Erdos-Renyi = ", i, " ClustC = ")
  print(clustering_coefficient)
  
  cat("--------------------\nAverage Length: \n")
  print(average_l)
  cat("--------------------\n")
  
  GCC <- global_cluster_coeff(rand_er)
  cat("--------------------\nGlobal Clustering Coefficient (GCC)\nGCC = ")
  print(GCC)
  cat("\n")
  
  CC <- closeness_centrality(rand_er)
  cat("--------------------\nCloseness Centrality (CC)\nCC = ")
  print(CC)
  cat("\n")
  
  BC <- betweenness_centrality(rand_er)
  cat("--------------------\nBetweeness Centrality (BC)\nCC = ")
  print(BC)
  cat("\n")
  
  Hub_Node <- hub_identification(rand_er)
  cat("--------------------\nHub Node \t= ")
  print(Hub_Node)
  cat("Number of Edges\t= ")
  print(degree(rand_er)[Hub_Node])
  cat("Degrees")
  print(degree(rand_er))
  
  rand_er <- get_hist(rand_er)
}

# A graph small-world of size n = 1000 with p = 0.1 and m = 2
if(execute_flag == 2){
  rand_ws <- watts_strogats(200, 0.35, 2)
  plot(rand_ws)
  
  # rand_ws <- watts_strogats(1000, 0.1, 2)
  # plot(rand_ws)
  
  clustering_coefficient <- clustering_coef(rand_ws)
  cat("\nClustering Coefficient (ClustC) for Small-World \nClustC = ")
  print(clustering_coefficient)
  
  GCC <- global_cluster_coeff(rand_ws)
  cat("--------------------\nGlobal Clustering Coefficient (GCC)\nGCC = ")
  print(GCC)
  cat("\n")
  
  CC <- closeness_centrality(rand_ws)
  cat("--------------------\nCloseness Centrality (CC)\nCC = ")
  print(CC)
  cat("\n")
  
  BC <- betweenness_centrality(rand_ws)
  cat("--------------------\nBetweeness Centrality (BC)\nCC = ")
  print(BC)
  cat
  
  Hub_Node <- hub_identification(rand_ws)
  cat("--------------------\nHub Node \t= ")
  print(Hub_Node)
  cat("Number of Edges\t= ")
  print(degree(rand_ws)[Hub_Node])
  cat("Degrees")
  print(degree(rand_ws))
  
  rand_ws <- get_hist(rand_ws)
}

# Graph Scale-Free
if(execute_flag == 3){
  rand_sf <- scale_free(200,5,3)
  plot(rand_sf)
  
  # rand_sf <- scale_free(1000,3,2)
  # plot(rand_sf)
  
  clustering_coefficient <- clustering_coef(rand_sf)
  cat("Clustering Coefficient (ClustC) for Scale-Free \nClustC = ")
  print(clustering_coefficient)
  
  GCC <- global_cluster_coeff(rand_sf)
  cat("--------------------\nGlobal Clustering Coefficient (GCC)\nGCC = ")
  print(GCC)
  cat("\n")
  
  CC <- closeness_centrality(rand_sf)
  cat("--------------------\nCloseness Centrality (CC)\nCC = ")
  print(CC)
  cat("\n")
  
  
  BC <- betweenness_centrality(rand_sf)
  cat("--------------------\nBetweeness Centrality (BC)\nCC = ")
  print(BC)
  cat("\n")
  
  Hub_Node <- hub_identification(rand_sf)
  cat("--------------------\nHub Node \t= ")
  print(Hub_Node)
  cat("Number of Edges\t= ")
  print(degree(rand_sf)[Hub_Node])
  #cat("Degrees")
  #print(degree(rand_sf))
  
  rand_sf <- get_hist(rand_sf)
}