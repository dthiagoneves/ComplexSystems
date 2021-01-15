rm(list=ls())
cat("\014")

source("EX1.R")
source("EX2.R")


# Represent the evolution of the number of Suceptible, Infected and Recovered people as a function of
# time, when the contact network graph for the epidemic outbreak is an Erdos-Renyi random graph (with
# a probability p0 = 0.01 for a vertice between any two nodes chosen uniformly at random), a population
# of N = 2000 individuals, a disease transmission probability p = 0.01, a number n0 = 10 of initially
# infected people, a contamination period of nd = 10 days and simulation with a duration of t = 1000
# days.


population_size = 200
p <- 0.08 #probability
population <- generate_graph(3, population_size, p)


# Simulation
p <- 0.08
n0 <- 10
nd <- 10
t = 200

tc = 1 # sleep time constant (higher = slower simulation)
nplots = 5
population <- epidemic_spread(population, n0, p, nd, t, tc, nplots)


# plot(population)