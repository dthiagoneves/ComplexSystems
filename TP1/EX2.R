# Define a function that gets the adjacency list from the adjacency matrix
matrix2list <- function(A_mat){
  NROW <- nrow(A_mat)
  A_list <- list();
  
  for(i in 1:NROW){
    A_list[[i]] <- list()
    k <- 1
    for(j in 1:NROW){
      if (A_mat[i,j] > 0){
        A_list[[i]][[k]] <- j
        k = k + 1
      }
    }
  }
  
  return (A_list)
}


# Define a function that gets the adjacency matrix from the adjacency list
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


# Define a functions that from the adjacency matrix or adjacency list return the list of degrees
degrees_list <- function(A_init){
  if(class(A_init) == "matrix"){
    A_l <- matrix2list(A_init)
  }
  else{
    A_l <- A_init
  }
  
  k <- list()
  
  for(i in 1:length(A_l)){
    k[i] <- length(A_l[[i]])
  }
  
  return(k)
}


# Define a functions that from the adjacency matrix or adjacency list return the degree distribution
degree_distr <- function(A_init){
  k <- degree(A_init)
  k_max <- max(unlist(k))
  
  
  distr <- replicate(k_max+1, 0)
  for(i in 1:k_max+1){
    distr[i] <- 0
  }
  
  for(i in k){
    distr[i+1] <- distr[i+1] + 1
  }
  
  return(distr)
}


# Implement a function that returns the list of cluster coefficients from the adjacency matrix
clustering_coef <- function(A_init) {
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
      ki = ki + A_m[i,j]
      for(p in 1:NROW){
        ENi = ENi + (A_m[i,j]*A_m[j,p]*A_m[p,i])
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