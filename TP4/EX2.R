rm(list=ls())
cat("\014")

source("EX1.R")


generate_graph <- function(selection, number_of_nodes, probability){
  if(selection == 1){
    # erdos-renyi
    g_erdos <- erdos.renyi.game(number_of_nodes, probability,
                                type = c("gnp"), directed = FALSE, loops = TRUE)
    return(g_erdos)
  }
  else if (selection == 2){
    # watts-strogatz
    neighborhood_siz = round(runif(1,0,round(number_of_nodes/10, 0)),0)
    g_watts <- sample_smallworld(1, number_of_nodes, neighborhood_siz,
                                 probability)
    return(g_watts)
  }
  else {
    g_scalefree <- sample_pa(number_of_nodes, m = 2, directed = FALSE)
    return(g_scalefree)
  }
}


# ---------------------------------
# Interventional Measures Functions

lockdown_after_50 <- function(population, threshold = 0.5, ancient_adj_list){
  N_pop <- length(V(population))
  half_N_pop <- round(N_pop*threshold,0)
  
  N_infected <- length(which(V(population)$class == "I", TRUE))
  
  if( all(V(population)$lockdown == "F" )){lock_flag <- FALSE}
  else if( all(V(population)$lockdown == "T" )){lock_flag <- TRUE}
  
  adj_list <- as_adj_list(population)
  # l_edges <- as_edgelist(population)
  
  # if(all(V(population)$onetime == "T")){
  #   cat("cu")
  #   ancient_adj_list <- as_adj_list(population)
  #   V(population)$onetime <- "F"
  # }
  
  
  if((N_infected > half_N_pop) && (lock_flag == FALSE)){
    
    V(population)$lockdown <- "T"
    
    cat("\nDegree Before Lockdown: ")
    print(degree(population))
    
    # cut 80% of edges
    dgr <- sum(degree(population))
    new_dgr <- dgr*0.2
    new_dgr <- round(new_dgr,0)
    dgr_aux <- dgr
    
    
    for(i in 1:N_pop){
      adj_list <- as_adj_list(population)
      #dim_edges <- dim(l_edges)[1]
      #cat("i =",i, "\n")
      for(e in adj_list[[i]]){
        #cat("e =",e, "\n")
        if( (dgr_aux > new_dgr) && (lock_flag == FALSE) ){
          # condition is never met
          population <- delete.edges(population, get.edge.ids(population, c(i,e), directed =FALSE))
          # population <- delete.edges(population, get.edge.ids(population, c(id,j), directed = FALSE))
          dgr_aux <- sum(degree(population))
        }
        else {break}
      }
    }
    
    
    if(lock_flag == FALSE){
      cat("\nDegree After Lockdown: ")
      print(degree(population))
    }
    
    lock_flag <- TRUE
    V(population)$lockdown <- "T"
  }
  else if( (N_infected < 10) && (lock_flag == TRUE) ){
    # restablish ancient edges
    
    
    # remove all edges
    dgr_aux <- sum(degree(population))
    for(i in 1:N_pop){
      adj_list <- as_adj_list(population)
      #dim_edges <- dim(l_edges)[1]
      #cat("i =",i, "\n")
      for(e in adj_list[[i]]){
        #cat("e =",e, "\n")
        if( (dgr_aux > 0) ){
          # condition is never met
          population <- delete.edges(population, get.edge.ids(population, c(i,e), directed =FALSE))
          dgr_aux <- sum(degree(population))
        }
        else {break}
      }
    }
    
    # add old edges
    for(i in 1:N_pop){
      for(e in ancient_adj_list[[i]]){
        updated_adj_list <- as_adj_list(population)
        if(!is.element(e, updated_adj_list[[i]])){
          population <- add.edges(population, c(i,e))
        }
        # population <- add.edges(population, c(i,e))
      }
    }
    
    V(population)$lockdown <- "F"
    cat("\nDegree With No More Lockdown: ")
    print(degree(population))
  }
  
  dgr_pop <- degree(population)
  
  return(population)
}


test_population <- function(population, scenario){
  p_false_negative = runif(1)
  N_pop <- length(V(population))
  testing_capacity <- round(N_pop/10,0)
  tested_people <- 0
  
  if(scenario == 1){
    scenario_flag_1 <- TRUE
    scenario_flag_2 <- FALSE
  }
  else if(scenario == 2){
    scenario_flag_2 <- TRUE
    scenario_flag_1 <- FALSE
  }
  else{
    #cat("Tests Disabled")
    return(population)
  }
  
  
  # scenario 1
  while(tested_people < testing_capacity && scenario_flag_1){
    error_log <- 0
    person_tested <- 1
    repeat{
      error_log <- error_log + 1
      
      if( V(population)[person_tested]$symp == "S" ){break}
      if( person_tested == N_pop ){break}
      person_tested <- person_tested + 1
    }
    
    if( person_tested == N_pop + 1 ){break}
    
    if( V(population)[person_tested]$class == "I" ){
      # possibility to add a false positive exam
      if( (V(population)[person_tested]$time > 0) && (V(population)[person_tested]$time < 5) ){
        if(runif(1) > 0.60){ V(population)[person_tested]$tested <- "P" }
        else{  V(population)[person_tested]$tested <- "N" }
      }
      else if((V(population)[person_tested]$time > 0) && (V(population)[person_tested]$time >= 5)){
        if(runif(1) > 0.20){ V(population)[person_tested]$tested <- "P" }
        else{  V(population)[person_tested]$tested <- "N" }
      }
    }
    else{ V(population)[person_tested]$tested <- "N" }
    tested_people <- tested_people + 1
  }
  
  
  
  # scenario 2
  while(tested_people < testing_capacity && scenario_flag_2){
    repeat{
      person_tested = runif(1,1,N_pop)
      if( V(population)[person_tested]$tested == "X" || V(population)[person_tested]$tested == "N" ){
        break
      }
    }
    
    if( V(population)[person_tested]$class == "I" ){
      if( (V(population)[person_tested]$time > 0) && (V(population)[person_tested]$time < 5) ){
        if(runif(1) > 0.60){ V(population)[person_tested]$tested <- "P" }
        else{  V(population)[person_tested]$tested <- "N" }
      }
      else if((V(population)[person_tested]$time > 0) && (V(population)[person_tested]$time >= 5)){
        if(runif(1) > 0.20){ V(population)[person_tested]$tested <- "P" }
        else{  V(population)[person_tested]$tested <- "N" }
      }
    }
    else{ V(population)[person_tested]$tested <- "N" }
    tested_people <- tested_people + 1
  }
  
  return(population)
}


# isolate_positive <- function(population, isolation_days = 10, id){
#   # quarantine == no more infections
#   
# }