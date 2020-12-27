# Funtion to generate a specific random graph
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

