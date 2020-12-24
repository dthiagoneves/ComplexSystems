rm(list=ls())
cat("\014")

source("EX1.R")

find_maxdegree <- function(graphG, max_dgr = 100, n_elements = 10){
  dgr <- degree (g_scalefree)
  maxids <- list()
  
  i = 0
  while(i < n_elements){
    id <- which.max(dgr)
    if (dgr[id] < max_dgr){
      maxids[i+1] <- id
      print(dgr[id])
      dgr[id] <- -1
      i = i + 1
    }
  }
 return(maxids) 
}

create_starter <- function(graphG){
  adj_m <- as_adj_list(g_scalefree)
  dim_v <- length(adj_m)
  
  starter_sf <- rbinom(dim_v, 1, 0.5)
  
  maxdgr <- find_maxdegree(g_scalefree)
  
  
  for(i in maxdgr){
    starter_sf[i] <- 1
  }
  
  return(starter_sf)
}

g_scalefree <- sample_pa(250, m = 2, directed = FALSE)
V(g_scalefree)$size <- 5
V(g_scalefree)$label_size <- 0
plot(g_scalefree)

starter_sf <- create_starter(g_scalefree)

vote_matrix <- vote_evoution(g_scalefree, 0.01, 50, isInitialised = TRUE, starter = starter_sf) #1 red vs 0 blue
print(vote_matrix)