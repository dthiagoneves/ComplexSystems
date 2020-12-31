library(RColorBrewer)
source("EX3.R")

# Now 20 percent of the population will still vote for Robert and another 20 percent will still vote for Mickey.
# Choose randomly these agents, they are named zealots.
assign_zealots <- function(graphG, n, percentage = 20){
  Nz <- round(n * percentage / 100)
  
  V(graphG)$class <- "N"
  zealots <- sample(1:n,2*Nz)
  
  for(i in 1:Nz){
    
    # Robert (vote 0)
    z <- zealots[i]
    V(graphG)[z]$class <- "Z"
    V(graphG)[z]$vote <- 0
    
    # Mickey (vote 1)
    z <- zealots[i + Nz]
    V(graphG)[z]$class <- "Z"
    V(graphG)[z]$vote <- 1
  }
  
  agents_ids <- which(V(graphG)$class == "N")
  
  V(graphG)[agents_ids]$vote <- rbinom(n - 2*Nz, 1, 0.5)
  
  return(graphG)
}



# Simulation of the votation, by keeping in mind people who are Zealots
vote_zealots <- function(population, E, t, visualise = FALSE ,layout_G = NULL){
  
  #obtenir la matrice d'adjacence graphG
  graphG <- population
  adj_m <- as_adj_list(graphG)
  n <- length(adj_m)
  
  
  #initialise la matrice
  vote_m <- matrix(0L, nrow = n, ncol = t)
  vote_m[,1] <- V(graphG)$vote
  
  #boucle pour calculer les votes chaque instant jusq'à t
  for(k in 1:(t-1)){
    for (i in 1:n){
      
      if(V(graphG)[i]$class == "Z"){
        vote_m[i,k+1] <- V(graphG)[i]$vote
      }
      else{
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
          V(graphG)[i]$vote <- 1
        }
        else {
          vote_m[i,k+1] <- 0
          V(graphG)[i]$vote <- 0
        }
      }
    }
  }
  
  if(visualise){
    plot_population(graphG, layout_G)
  }
  
  return(vote_m)
  
}



# In first, among the agents who are not zealots, identify the 10 agents you are going to convince
modify_agents <- function(population, metric = 0, biased = 10){
  
  if(metric == 0){
    property <- degree(population)# degree
  }
  else if(metric == 1){
    property <- transitivity(population, type="local") # clustering coefficient
  }
  else if(metric == 2){
    property <- closeness(population, normalized = TRUE) # closeness centrality
  }
  else{
    return(population)
  }
  
  # Discart the zealots
  zealots_ids <- which(V(population)$class == "Z")
  property[zealots_ids] <- -1

  # Discart the ones who already vote for Mickey
  mickey_ids <- which(V(population)$vote == "1")
  property[mickey_ids] <- -1
  
  max_ids <- finds_maxs(property, n_max = biased)
  adj_list <- as_adj_list(population)
  
  for(i in max_ids){
    V(population)[i]$vote <- 1
  }
  

  cat("# Number of Biased People: ")
  print(length(max_ids))
  cat("# Biased People: ")
  print(unlist(max_ids))
  cat("# People Metrics: ")
  print(property[unlist(max_ids)])
  
  for(i in max_ids){
    V(population)[i]$vote <- 1
  }
  
  return(population)
}



# Function that return the n_max agents' ids with the hishest property
finds_maxs <- function(property, n_max = 10){
  max_ids <- rep(0L, n_max)
  
  for(i in 1:n_max){
    id <- which.max(property)
    max_ids[i] <- id
    property[id] <- -1
  }
  
  return(max_ids) 
}



# With the zealots, you can now add and remove 20 edges to influence the vote in favor of Mickey.
modify_edges <- function(population, metric = 0, edges = 20, percentage = 0.5){
  
  if(metric == 0){
    property <- degree(population)# degree
  }
  else if(metric == 1){
    property <- transitivity(population, type="local") # clustering coefficient
  }
  else if(metric == 2){
    property <- closeness(population, normalized = TRUE) # closeness centrality
  }
  else{
    return(population)
  }
  
  n <- length(V(population))
  adj_list <- as_adj_list(population)
  
  # Discart the zealots
  zealots_ids <- which(V(population)$class == "Z")
  zealots_m_ids <- zealots_ids[V(population)[zealots_ids]$vote == "1"] # RED = 1 Mickey
  zealots_r_ids <- zealots_ids[V(population)[zealots_ids]$vote == "0"] # BLUE = 0 Robert
  property[zealots_ids] <- -1
  
  sort_ids <- sort(property, decreasing = TRUE, index.return = TRUE)$ix
  
  n <- length(V(population))
  z <- length(zealots_ids)
  adj_list <- as_adj_list(population)
  
  rm_edges <- list()
  add_edges <- list()
  
  ed_rm <- 0
  ed_add <- 0
  
  for(i in 1:(n-z)){
    
    id <- sort_ids[i]
    
    if((ed_rm + ed_add) >= edges || property[id] < 0){
      break
    }
    
    # Remove edges
    for(j in adj_list[[id]]){
      if(is.element(j,zealots_r_ids)){
        if(ed_rm < edges*(1-percentage)){
          population <- delete.edges(population, get.edge.ids(population, c(id,j), directed = FALSE))
          cat("Edge Removed (",ed_rm+1,") : " )
          print(c(id,j))
          
          ed_rm <- ed_rm + 1
        }
        else{
          break
        }
      }
    }
    
    # Add edges
    for(add in 1:(length(zealots_m_ids)/2)){
      if(ed_add < edges * percentage){
        j <- zealots_m_ids[add]
        
        if(!is.element(j,adj_list[[id]])){
          population <- add.edges(population, c(id,j))
          
          cat("Edge Added (", ed_add+1 , ") : " )
          print(c(id,j))
          
          ed_add <- ed_add + 1
        }
      }
      else{
        break
      }
    }
    
  }
  
  
  
  
  
  cat("# Number of modified edges: ")
  print(ed_rm + ed_add)
  
  return(population)
}



plot_population <- function(population, layout_P){
  my_color <- ifelse(V(population)$vote == 1, "red", "blue")
  my_shape <-  ifelse(V(population)$class == "N", "circle", "square")
  
  plot(population, vertex.shape = my_shape, vertex.color=my_color, layout = layout_P)
}