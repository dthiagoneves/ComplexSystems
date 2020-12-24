library(igraph)

# Implement a function to simulate the spread of the epidemic.
# The input variables are the graph G which represent social connections, the number of infected people
# n0 at the initial time, the probability p that a person infects one of their neighbours in the graph every
# day, nd the number of days a person is infectious and the time t.


epidemic_spread <- function(G, n0, p, nd, t, tc, nplots){
  
  V(G)$class <- "S"
  V(G)$color <- "white"
  V(G)$time <- -1
  V(G)$infected <- 0
  
  #obtenir la matrice d'adjacence graphG
  adj_m <- as_adj_list(G)
  dim_v <- length(adj_m)
  
  for(i in 1:n0){
    repeat{
      n = runif(1, 1, dim_v)
      
      if(all(V(G)[n]$class == "S")){
          break
      }
    }
    
    V(G)[n]$class <- "I"
    V(G)[n]$color <- "red"
    V(G)[n]$time <- 0
    
  }
  
  layout_G = layout_with_fr(G)
  
  plot(G, vertex.size = 4, vertex.label.cex = 0.5, layout = layout_G)

  for(k in 1:t){
    Infected <- V(G)[V(G)$class == "I"]

    for(i in Infected){
      if (V(G)[i]$time < nd){
        
        V(G)[i]$time <- V(G)[i]$time + 1
        
        for(m in adj_m[i]){
          if(all(V(G)[m]$class == "S")){
            print(V(G)[m]$class == "S")
            if(runif(1) <= p){
              
              V(G)[m]$class <- "I"
              V(G)[m]$color <- "red"
              V(G)[m]$time <- 0
              print(V(G)[m]$class)
              V(G)[i]$infected <- V(G)[i]$infected + 1
            }
          }
        }
      }
      else {
        V(G)[i]$class <- "R"
        V(G)[i]$color <- "green"
      }
    }
    
    if (length(Infected) < 1){
      plot(G, vertex.size = 4, vertex.label.cex = 0.5, layout = layout_G)
      print("No one is infected!")
      print(k)
      break
    }
    
    
    if(k%%(round(t/nplots,0)) == 0){
      plot(G, vertex.size = 4, vertex.label.cex = 0.5, layout = layout_G)
    }
    print(k)
    if(dim_v < 500){
      Sys.sleep(tc / t)
    }
  } # end for (k in 1:t)
  
  R0 <- mean(V(G)$infected)
  #print("")
  return(G)
}
