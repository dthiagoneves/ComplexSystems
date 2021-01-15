library(igraph)

# Implement a function to simulate the spread of the epidemic.
# The input variables are the graph G which represent social connections, the number of infected people
# n0 at the initial time, the probability p that a person infects one of their neighbours in the graph every
# day, nd the number of days a person is infectious and the time t.


epidemic_spread <- function(G, n0, p, nd, t, tc, nplots){
  
  # to ensure that we wil restore people's connections
  V(G)$onetime <- "T"
  ancient_adj_list <- as_adj_list(G)
  #etst
  # necessary for the lockdown dynamic
  V(G)$lockdown <- "F"
  
  # testing dynamics
  V(G)$tested <- "X"
  # X = No
  # N = Negative
  # P = Positive
  
  # application of tests and isolation
  V(G)$symp <- "N"
  # N = negative
  # A = Asymptomatic
  # S = Symptomatic
  
  V(G)$class <- "S"
  V(G)$color <- "white"
  V(G)$time <- -1
  V(G)$infected <- 0
  
  #obtenir la matrice d'adjacence graphG
  adj_m <- as_adj_list(G)
  dim_v <- length(adj_m)
  
  for(i in 1:n0){
    repeat{
      n = round(runif(1, 1, dim_v),0)
      if(V(G)[n]$class == "S"){
        #print(V(G)[n]$class == "S")
        break
      }
    }
    
    V(G)[n]$class <- "I"
    V(G)[n]$color <- "red"
    V(G)[n]$time <- 0
    
    if(runif(1) < 0.2){ # p of being Asymptomatic
      V(G)$symp <- "A"
    }
    
  }
  
  layout_G = layout_with_fr(G)
  
  plot(G, vertex.size = 4, vertex.label.cex = 0.5, layout = layout_G)
  
  Infected <- V(G)[V(G)$class == "I"]
  max_infected_aux <- length(Infected)
  
  
  
  
  # Estimation of parameters for the SIR Model
  
  beta <- 1 # infection rate
  gama <- 1 # recovery rate
  
  
  isolation_days <- 10
  scenario <- 0
  # -------------------------------
  # Begin time simulation
  for(k in 1:t){
    Infected <- V(G)[V(G)$class == "I"]
    
    G <- test_population(G, scenario)
    
    # application of intervention measures
    threshold <- 0.5
    G <- lockdown_after_50(G, threshold, ancient_adj_list)
    # G <- lockdown_after_50(G)
    
    
    if(length(Infected) > max_infected_aux){max_infected <- length(Infected)}
    else{max_infected <- max_infected_aux}
    
    for(i in Infected){
      if( ( (V(G)[i]$tested == "X") || (V(G)[i]$tested == "N") ) || (scenario==0) ){
        
        if (V(G)[i]$time < nd){
          
          if(V(G)[i]$time >= 5){
            
            if(V(G)[i]$symp == "N"){
              V(G)[i]$symp <- "S"
            }
          }
          
          V(G)[i]$time <- V(G)[i]$time + 1
          
          for(m in adj_m[[i]]){
            
            if(V(G)[m]$class == "S"){
              infection_chance = runif(1)
              if(infection_chance < p){
                
                #cat("Infection Chance = ", infection_chance, "\n")
                V(G)[m]$class <- "I"
                V(G)[m]$color <- "red"
                V(G)[m]$time <- 0
                if(runif(1) < 0.2){ # p of being Asymptomatic
                  V(G)$symp <- "A"
                }
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
      else{
        V(G)[i]$time <- V(G)[i]$time + 1
        
        if (V(G)[i]$time < nd){
          
          if(V(G)[i]$time >= 5){
            if(V(G)[i]$symp == "N"){
              V(G)[i]$symp <- "S"
            }
          }
        }
        else {
          V(G)[i]$class <- "R"
          V(G)[i]$color <- "green"
        }
      }
    }
    if (length(Infected) < 1){
      plot(G, vertex.size = 4, vertex.label.cex = 0.5, layout = layout_G)
      cat("--------------------------\n")
      print("No one is infected!")
      cat("Pandemic Final Day ", k, "\n")
      cat("Pandemic Peak = ", max_infected, " infected\n")
      break
    }
    
    # for large simulations
    if(k%%(round(t/nplots,0)) == 0){
      plot(G, vertex.size = 4, vertex.label.cex = 0.5, layout = layout_G)
    }
    # for small simulations
    # plot(G, vertex.size = 4, vertex.label.cex = 0.5, layout = layout_G)
    
    
    cat("--------------------------\n")
    cat("N of Infected = ", max_infected, "\n")
    cat("Pandemic Day ", k, "\n")
    if(dim_v < 500){
      Sys.sleep(tc / t)
    }
  } # end for (k in 1:t)
  # End of time simulation
  # -------------------------------
  
  
  R0 <- mean(V(G)$infected)
  cat("--------------------------\n")
  cat("R0 = ", R0, "\n")
  return(G)
  }




