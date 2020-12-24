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

# You will take graphs of size n = 500 and the time t = 3000. In all simulations, the simulation will be t = 0.01

# a graph Erdos-Renyi with p = 0.05
# erdos_ren <- generate_graph(1, 50, 0.05)
# plot(erdos_ren)
# 
# vote_matrix <- vote_evoution(erdos_ren, 0.01, 50, 10)
# print(vote_matrix)

# a Watts-Strogatz graph with p = 0,01
# watts_strog <- generate_graph(2, 100, 0.01)
# plot(watts_strog)
# 
# vote_matrix <- vote_evoution(watts_strog, 0.01, 50, 10)
# print(vote_matrix)


# a graph scale free with k = 3 and m = 2 (see TP2).
scale_free <- generate_graph(3, 100, 0.01)
plot(scale_free)

vote_matrix <- vote_evoution(scale_free, 0.01, 50, 10)
print(vote_matrix)

