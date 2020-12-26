source("EX2.R")

# Implement a function BFS for which the input variable is the adjacency matrix and output variable is the matrix D
# Without using a stack
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

# Implement a function BFS for which the input variable is the adjacency matrix and output variable is the matrix D
# With using a stack
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

# Implement a function diameter for which the input variable is a node s and output value is the diameter of the component of s
diameter <- function(A_init, s){
  D <- BFS_stack(A_init)
  d <- max(D[s,])
  return(d)
}

# Implement a function closeness_centrality for which the input variable is the adjacency matrix
# and the output variable is the vector of closeness centrality cc(i) of nodes
closeness_centrality <- function(A_init){
  D <- BFS_stack(A_init)
  n <- dim(D)[1]
  cc <- rep(0L, n)
  
  for(s in 1:n){
    cc[s] <- n / sum(D[s,])
  }
  
  return(cc)
}


# Return the directed graph with the paths that reaches the vertex s from all the other ones 
shortest_paths_tree <- function(A_init, s){
  #Check if A_init is a matrix or a list
  if(class(A_init) == "matrix"){
    A_list <- matrix2list(A_init)
  }
  else{
    A_list <- A_init
  }
  
  n <- length(A_list)
  
  #Initialization of the adj list
  paths <- list()
  for(i in 1:n){
    paths[[i]] <- list()
  }
  
  # 1 Place s at the position 1 of the stack, we initialize read = 1, and write = 2
  # Create a vector Ds of n integers and an integer d
  Ds <- rep(-1L, n)
  d <- 0
  Ds[s] <- 0
  
  stack <- list()
  stack[1] <- s
  
  read <- 1
  write <- 2
  
  # 2 if read = write, then go to 5, else read the next node i of the stack, d = Ds(i)
  while(read != write){
    i <- stack[[read]]
    
    d <- Ds[i]
    
    # 3 For every neighbor j of i, if Ds(j) = -1 then Ds(j) = d + 1 and j is placed in the top of the stack write = write + 1
    for(j in A_list[[i]]){
      
      if(Ds[j] <= -1){
        
        Ds[j] <- d + 1
        
        # every time a neighbor j from i is an unseen node, a predecessor pointer j <- i is created
        paths[[j]] <- c(paths[[j]], i)
        
        stack[write] <- j
        write <- write + 1
      }
      # extra step: when examining neighbors of node i (at a distance d from
      # s), if some node j has already assigned distance d + 1, a new pointer
      # j -> i is created
      else if(Ds[j] == d + 1) {
        paths[[j]] <- c(paths[[j]], i)
      }
      
      
    }
    
    # 4 read = read + 1 and go to 2
    read <- read + 1
    
  }
  
  return(paths)
}

# Recursive function that compute the number of path of a tree from a leaf to the root
read_node <- function(tree, leaf, root){
  delta = 0
  
  if(length(tree[[leaf]]) > 0 ){
    for(j in tree[[leaf]]){
      if (j == root){
        delta[1] <- delta[1] + 1
      }
      else {
        delta <- delta + read_node(tree, j, root, k)
      }
    }
  }

  return (delta)
}



# Implement a function betweenness_centrality for which the input variable is the adjacency matrix and
# the output variable is the vector of betweenness centralities delta(i)
# Using Ulrik Brandes algorithm
betweenness_centrality <- function(A_init){
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