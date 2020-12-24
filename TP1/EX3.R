source("EX2.R")

#Implement a function BFS for which the input variable is the adjacency matrix and output variable is the matrix D
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
        JJ <- lengths(A_list[i])
        for(j in 1:JJ){
          if(D[s,A_list[[i]][j] ] <= -1){
            D[s,A_list[[i]][j] ] <- d + 1
          }
        }
      }
    }
  }

  return(D)
}