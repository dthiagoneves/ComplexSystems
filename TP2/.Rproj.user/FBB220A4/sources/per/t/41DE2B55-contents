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