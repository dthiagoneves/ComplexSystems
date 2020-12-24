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

# Represent the evolution of the number of Suceptible, Infected and Recovered people as a function of
# time, when the contact network graph for the epidemic outbreak is an Erdos-Renyi random graph (with
# a probability p0 = 0.01 for a vertice between any two nodes chosen uniformly at random), a population
# of N = 2000 individuals, a disease transmission probability p = 0.01, a number n0 = 10 of initially
# infected people, a contamination period of nd = 10 days and simulation with a duration of t = 1000
# days.
p <- 0.05
N = 2000

population <- generate_graph(1, N, p)

population <- epidemic_spread(population, 10, 1, 10, 1000, 60, 2)


# plot(population)


