library(igraph)

erdos_renyi <- function(n, p){
  # Creation des nodes
  g <- graph.empty(n, directed = FALSE)
  
  for(i in 1:(n-1)){
    for (j in (i+1):n){
      if(runif(1) < p){
        g <- add.edges(g, c(i,j))
      }
    }
  }
  
  return(g)
}

watts_strogats <- function(n, p, m){
  # Creation des nodes
  g <- graph.empty(n, directed = FALSE)
  
  # Step 1 : generate a ring graph
  for(i in 1:(n-1)){
    g <- add.edges(g, c(i,i+1))
  }
  g <- add.edges(g, c(n,1))
  
  # Step 2 : add the vertices
  # that connect the nodes with the 2m nearest neighbours. in this case, m = 3, for exemple for the node 5, it is
  # already connected to nodes 4 and 6, we add the links with 2; 3; 7 and 8.
  g_tmp <- g
  for(i in 1:(n-1)){
    dd <- distances(g_tmp, v = i)
    for (j in (i+1):n){
      if (dd[1,j] <= m && dd[1,j] > 1){
        g <- add.edges(g, c(i,j))
      }
    }
  }
  
  # Step 3 : for each existing edge, it
  # is removed with a probability p
  l_edges <- as_edgelist(g)
  dim_edges <- dim(l_edges)[1]

  for(e in 1:dim_edges){
    if(runif(1) < p){
      g <- delete.edges(g, get.edge.ids(g, l_edges[e,]))
    }
  }

  
  # Step 4 : for the most distant nodes, we add a link with the probability p.
  g_tmp <- g
  for(i in 1:(n-1)){
    dd <- distances(g_tmp, v = i)
    for (j in (i+1):n){
      if (dd[1,j] > m && runif(1) < p){
        g <- add.edges(g, c(i,j))
      }
    }
  }
  
  
  return(g)
}



scale_free <- function(n, k, m){
  # Step 1 : generate a complete graph of size k
  g <- graph.empty(k, directed = FALSE)
  
  for(i in 1:(k-1)){
    for (j in (i+1):k){
      g <- add.edges(g, c(i,j))
    }
  }
  
  # Step 2 : add a vertex and connect it to "m" existing nodes
  g <- add.vertices(g, 1)
  Nv <- length(V(g))
  p <- 1/k
  flag_added_edge <- 0
  flag_avoid_double_edge <- 0
  for(i in 1:m){
    for(j in 1:Nv){
      if((runif(1) < p) && (flag_added_edge==0) && (flag_avoid_double_edge!=j)){
        g <- add.edges(g, c(Nv,j))
        flag_added_edge <- 1
        flag_avoid_double_edge <- j
      }
    }
    flag_added_edge <- 0
  }
  
  # Step 3 : repeat step 2 until number of nodes = n
  
  for(l in 2:(n-k)){
    g <- add.vertices(g, 1)
    Nv <- length(V(g))
    dgr <- degree(g)
    p <- dgr[l]/sum(dgr)
    adj_list <- as_adj_list(g)
    
    flag_added_edge <- 0
    flag_avoid_double_edge <- 0
    for(i in 1:m){
      for(j in (k+1):Nv){
        if(!is.element(j, adj_list[j])){
          if((runif(1) < p) && (Nv != j)){
            g <- add.edges(g, c(Nv,j))
          }
        }
      }
      flag_added_edge <- 0
    }
  }
  
  return(g)
}

