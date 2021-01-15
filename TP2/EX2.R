rm(list=ls())
cat("\014")

source("EX1.R")

library(RColorBrewer)

# Historigram Representation

get_hist <- function(g){
  dgr <- degree(g)
  Nv <- length(V(g))
  
  max_degree <- max(dgr)
  
  dgr_distr_list <- rep(0L, max_degree+1)
  
  for(e in dgr){
    dgr_distr_list[e+1] <- dgr_distr_list[e+1] + 1
  }
  
  dgr_distr_list <- dgr_distr_list/Nv
  
  plot(0:max_degree, dgr_distr_list,
       main="Histogram of the Degree Distribution",
       ylab="Degree Distribution",
       xlab = "Degree",
       type = "h", lwd = 50/Nv)
  
  return(g)
}


# Average Length

average_length <- function(g){
  Adj_list <- as_adj_list(g)
  distance_matrix <- BFS_naive(Adj_list)
  N_graph <- length(V(g))
  l <- ( 2/(N_graph*(N_graph - 1)) )*sum(distance_matrix)
  return(l)
}



# BFS Naive algorithm for calculating the Average Length

BFS_naive <- function(A_init){
  #Check if A_init is a matrix or a list
  if(class(A_init) == "matrix"){
    A_list <- matrix2list(A_init)
  }
  else{
    A_list <- A_init
  }
  
  # Number of vertex
  n <- length(A_list)
  
  
  D <- matrix(1L, nrow=n, ncol=n) * -1
  
  for(s in 1:n){
    
    # 1 Create a vector Ds of n integers and an integer d
    D[s,s] <- 0
    
    for(d in 0:n){
      
      
      # 2 Find all nodes i such as Ds(i) = d
      list_node <- list()
      k <- 1
      for (i in 1:n){
        if (D[s,i] == d){
          list_node[k] <- i
          k <- k + 1
        }
      }
      
      # if there isn't, go to 5
      if(length(list_node) < 1){
        break
      }
      
      # 3 For all nodes i such as Ds(i) = d, if j is a neighbor of i and Ds(j) = -1 then Ds(j) = d + 1
      for (i in list_node){
        for(j in A_list[[i]]){
          
          if(D[s,j] <= -1){
            D[s,j] <- d + 1
          }
          
        }
      }
      
    }
  }
  
  return(D)
}

# BFS Stack for Calculating the Closeness Centrality

BFS_stack <- function(A_init){
  #Check if A_init is a matrix or a list
  if(class(A_init) == "matrix"){
    A_list <- matrix2list(A_init)
  }
  else{
    A_list <- A_init
  }
  
  # Number of vertex
  n <- length(A_list)
  
  # 1 Place s at the position 1 of the stack, we initialize read = 1, and write = 2
  # Create a vector Ds of n integers and an integer d
  D <- matrix(1L, nrow=n, ncol=n) * -1
  d <- 0
  
  for(s in 1:n){
    D[s,s] <- 0
    
    stack <- list()
    stack[1] <- s
    
    read <- 1
    write <- 2
    
    # 2 if read = write, then go to 5, else read the next node i of the stack, d = Ds(i)
    while(read != write){
      i <- stack[[read]]
      
      d <- D[s,i]
      
      # 3 For every neighbor j of i, if Ds(j) = -1 then Ds(j) = d + 1 and j is placed in the top of the stack write = write + 1
      for(j in A_list[[i]]){
        
        if(D[s,j] <= -1){
          
          D[s,j] <- d + 1
          
          stack[write] <- j
          write <- write + 1
        }
      }
      
      # 4 read = read + 1 and go to 2
      read <- read + 1
      
    }
  }
  
  return(D)
}



# Clustering Coefficient (from TP1)

clustering_coef <- function(g) {
  # A_int is adjacent list
  A_init <- as_adj_list(g)
  if(class(A_init) == "list"){
    A_m <- list2matrix(A_init)
  }
  else{
    A_m <- A_init
  }
  
  
  NROW <- nrow(A_m)
  
  CC <- list()
  
  for (i in 1:NROW) {
    ENi <- 0
    ki <- 0
    for(j in 1:NROW){
      ki <- ki + A_m[i,j]
      for(p in 1:NROW){
        ENi <- ENi + (A_m[i,j]*A_m[j,p]*A_m[p,i])
      }
    }
    if (ki > 1){
      CC[i] <- ENi / (ki * (ki-1))
    }
    else{
      CC[i] <- 0
    }
  }
  
  return(CC)
}

# Global Clustering Coefficient

global_cluster_coeff <- function(g){
  adj_list <- as_adj_list(g)
  Nv <- length(V(g))
  
  for(l in 1:Nv){
    V(g)[l]$class <- "N"
  }
  
  
  count_3 <- 0
  id <- 1
  for(e in adj_list){
    if(length(e)>1){
      for(i in (length(e)-1)){
        if( is.element(e[i],adj_list[[(i+1)]] ) ){
          
            # counts the number of triangles
            count_3 <- count_3 + 1
            
            
            V(g)[id]$class <- "T"
            V(g)[i]$class <- "T"
            V(g)[i+1]$class <- "T"
        }
      }
    }
    id <- id + 1
  }
  
  
  
  # counts the number of connected triangles
  count_connected <- 0
  id <- 1
  for(e in adj_list){
    node_class <- V(g)[id]$class
    if( node_class=="T" ){
      for(i in (length(e)-1)){
        
        # counts the number of triangles
        if( V(g)[i]$class == "T" ){
          count_connected <- count_connected + 1
        }
      }
    }
    id <- id + 1
  }
  
  GCC <- 3*count_3/count_connected
  cat("Count triangles: ")
  print(count_3)
  cat("Count Connected triangles: ")
  print(count_connected)
  return(GCC)
}


# Closeness Centrality

closeness_centrality <- function(g){
  A_init <- as_adj_list(g)
  D <- BFS_stack(A_init)
  n <- dim(D)[1]
  cc <- rep(0L, n)
  
  for(s in 1:n){
    cc[s] <- n / sum(D[s,])
  }
  
  return(cc)
}




# Betweeness Centrality

betweenness_centrality <- function(g){
  A_init <- as_adj_list(g)
  #Check if A_init is a matrix or a list
  if(class(A_init) == "matrix"){
    A_list <- matrix2list(A_init)
  }
  else{
    A_list <- A_init
  }
  
  # Number of vertex
  n <- length(A_list)
  
  # Init betweeness centrality
  Cb <- rep(0L, n)
  
  # 1 Place s at the position 1 of the stack, we initialize read = 1, and write = 2
  # Create a vector Ds of n integers and an integer d
  D <- matrix(1L, nrow=n, ncol=n) * -1
  d <- 0
  
  for(s in 1:n){
    D[s,s] <- 0
    
    stack <- list()
    stack[1] <- s
    
    read <- 1
    write <- 2
    
    #Initialization add
    sigma <- rep(0L, n)
    sigma[s] <- 1
    
    paths <- list()
    for(i in 1:n){
      paths[[i]] <- list()
    }
    
    
    
    # 2 if read = write, then go to 5, else read the next node i of the stack, d = Ds(i)
    while(read != write){
      i <- stack[[read]]
      
      d <- D[s,i]
      
      # 3 For every neighbor j of i, if Ds(j) = -1 then Ds(j) = d + 1 and j is placed in the top of the stack write = write + 1
      for(j in A_list[[i]]){
        
        if(D[s,j] <= -1){
          
          D[s,j] <- d + 1
          
          stack[write] <- j
          write <- write + 1
        }
        
        if(D[s,j] == d + 1){
          sigma[j] <- sigma[j] + sigma[i]
          paths[[j]] <- c(paths[[j]], i)
        }
      }
      
      # 4 read = read + 1 and go to 2
      read <- read + 1
      
    }
    
    m <- length(stack)
    delta <- rep(0L, n)
    
    for(k in m:1){
      j <- stack[[k]]
      
      for(i in paths[[j]]){
        
        if(sigma[i] != 0){
          delta[i] <- delta[i] + sigma[i] / sigma[j] * (1 + delta[j])
        }
        
        
      }
      
      if(j != s){
        Cb[j] <- Cb[j] + delta[j]
      }
    }
  }
  
  return(Cb / 2)
}


# Hub identification

hub_identification <- function(g){
  dgr <- degree(g)
  
  Hub_Node_degree <- max(dgr)
  Hub_Node <- which(dgr == Hub_Node_degree)
  
  return(Hub_Node)
  
}


# Auxiliary function from TP1

list2matrix <- function(A_l){
  NROW <- length(A_l)
  A_m <- matrix(0L, nrow = NROW, ncol = NROW)
  
  for(i in 1:NROW){
    for(j in A_l[[i]]){
      A_m[i,j] <- 1
    }
  }
  return(A_m)
}