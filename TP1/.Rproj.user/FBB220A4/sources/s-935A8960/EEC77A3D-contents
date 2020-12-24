library(igraph)
source("EX2.R")

adjmatrix <- function(A_init, is_directed = FALSE)
{
  if(class(A_init) == "matrix"){
    A_l <- matrix2list(A_init)
  }
  else{
    A_l <- A_init
  }
  
  len <-length(A_l)
  
  g <- graph.empty(len, directed = is_directed)
  
  
  for (i in 1:len){
    for (j in A_l[[i]]){
      g <- add.edges(g, c(i,j))
    }
  }
  
  return(g)
}