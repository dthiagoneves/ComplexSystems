library(igraph)

f_Vp <- function(p, E){
  return( (1-2*E)*p )
}

vcolor <- c("red","blue")



vote_evoution <- function(graphG, E, t, Tvis = 10, isInitialised = FALSE, starter = NULL){
  
  #obtenir la matrice d'adjacence graphG
  adj_m <- as_adj_list(graphG)
  dim_v <- length(adj_m)
  
  
  #initialise la matrice avec Beroulli'law
  vote_m <- matrix(0L, nrow = dim_v, ncol = t)
  
  if (isInitialised && !(is.null(starter))){
    vote_m[,1] <- starter
  }
  else {
    vote_m[,1] <- rbinom(dim_v, 1, 0.5) # bernoulli distribution in r
  }
  
  # set_vertex_attr("vote", value = vote_m[,1])
  # plot(graphG)
  # Sys.sleep(30 / t)
  
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
      
      
      if (runif(1) < f_Vp(p,E)){
        vote_m[i,k+1] <- 1
        V(graphG)[i]$color <- "red"
        
      }
      else {
        vote_m[i,k+1] <- 0
        V(graphG)[i]$color <- "blue"
      }
      
    }
    
    # set_vertex_attr(graphG,"vote", value = vote_m[,1])
    plot(graphG)
    Sys.sleep(Tvis / t)
    
    
  }
  
  return(vote_m)
}