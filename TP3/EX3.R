library(igraph)

# Law of total probability,
f_Vp <- function(p, E){
  return((1-2*E)*p + E)
}



# Function that returns the index list of the n_max vertex with the higher property
find_n_max <- function(graphG, property, max_dgr = 100, n_max = 10){
  dgr <- degree(graphG)
  max_ids <- list()
  n <- length(dgr)
  
  min_dgr <- min(dgr)
  
  elms = 0
  sum_dgr = 0
  i = 0
  while((i < n) && (elms < n_max)){
    id <- which.max(property)
    
    # the sum of their degrees is less than 100
    if (dgr[id] + sum_dgr < max_dgr){
      max_ids[elms+1] <- id
      sum_dgr <- sum_dgr + dgr[id]
      elms <- elms + 1
    }
    
    property[id] <- -1
    i <- i + 1
  }
  
  return(max_ids) 
}



# Function that compute the average length of the agents
avg_lengths <- function(graphG){
  sp <- shortest.paths(graphG)
  n <- dim(sp)[1]
  avg <- rep(-1L, n)
  
  for(i in 1:n){
    avg[i] <- mean(sp[i,])
  }
  
  return(avg)
}



# Function that modifies the initial population
# Use the metrics (degrees, clustering coefficient, average length of the agents) of the graphs to identify these people you need to convince.
modify_starter <- function(graphG, starter_unbiased, metric = 0, biased = 10){
  starter_biased <- starter_unbiased
  
  if(metric == 0){
    property <- degree(graphG) # degree
  }
  else if(metric == 1){
    property <- transitivity(graphG, type="local") # clustering coefficient
  }
  else if(metric == 2){
    property <- avg_lengths(graphG) # average length of the agents
  }
  else if(metric == 3){
    property <- closeness(graphG, normalized = TRUE) # closeness centrality
  }
  else{
    return(starter_unbiased)
  }

  n_max <- find_n_max(graphG, property, n_max = biased)
  cat("# Number of Biased People: ")
  print(length(n_max))
  cat("# Biased People: ")
  print(unlist(n_max))
  cat("# People Metrics: ")
  print(property[unlist(n_max)])
  
  for(i in n_max){
    starter_biased[i] <- 1
  }
  
  return(starter_biased)
}



# Use the metrics (degrees, clustering coefficient, average length of the agents) of the graphs to identify these people you need to convince.
# Explain your choice and make a simulation with the vote of these people forced to 1.
vote_biased <- function(graphG, E, t, starter, visualise = FALSE){
  
  #obtenir la matrice d'adjacence graphG
  adj_m <- as_adj_list(graphG)
  dim_v <- length(adj_m)
  layout_G = layout_with_fr(graphG)
  
  
  #initialise la matrice
  vote_m <- matrix(0L, nrow = dim_v, ncol = t)
  vote_m[,1] <- starter
  
  
  #boucle pour calculer les votes chaque instant jusq'à t
  for(k in 1:(t-1)){
    for (i in 1:dim_v){
      p = 0
      Ni = length(adj_m[[i]])
      
      if (Ni > 0) {
        for(s in adj_m[[i]]){
          p = p + vote_m[s,k]
        }
        
        p = p / Ni
      }
      else {
        p = 0.5
      }
      
      
      if (runif(1) <= f_Vp(p,E)){
        vote_m[i,k+1] <- 1
      }
      else {
        vote_m[i,k+1] <- 0
      }
      
    }
  }
  
  return(vote_m)
}