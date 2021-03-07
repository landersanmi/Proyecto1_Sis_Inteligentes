# =======================================================================
# Group Name: AmaIA
# Students: Lander San Millan, Amaia Acha
# =======================================================================

# Clear environment and console
rm(list=ls())
cat("\014")
graphics.off()
# Set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# Install required packages
# library(gridExtra)
# library(grid)
# library(ggplot2)
# library(lattice)

# Include algorithm functions
source("../algorithms/blind/expand-node.R")
source("../algorithms/blind/breadth-first-search.R")
source("../algorithms/blind/depth-first-search.R")
source("../algorithms/blind/depth-limited-search.R")
source("../algorithms/blind/iterative-deepening-search.R")
source("../algorithms/informed/greedy-best-first-search.R")
source("../algorithms/informed/uniform-cost-search.R")

# Include functions for data analysis and result plot
source("../algorithms/results-analysis/analyze-results.R")
source("../algorithms/results-analysis/plot-results.R")

# ADD YOUR CODE HERE TO INITIALIZE YOUR PROBLEM AND INCLUDE THE DEFINITION FILE
source("../problem/problem1-AmaIA.R")

problem <- initialize.problem("../data/assembler-robot-txt/assembler-robot-1.txt", c(), 1)
movements_bfs_ts <- c()
movements_bfs_gs <- c()
movements_dfs_ts <- c()
movements_dfs_gs <- c()
movements_dls6_ts <- c()
movements_dls6_gs <- c()
movements_dls10_ts <- c()
movements_dls10_gs <- c()
movements_ids_ts <- c()
movements_ids_gs <- c()

for (i in 1: problem$assembly_cuantity) {
  problemTemp_bfs_ts <- initialize.problem("../data/assembler-robot-txt/assembler-robot-1.txt", movements_bfs_ts, i)
  bfs_ts <- breadth.first.search(problemTemp_bfs_ts)
  movements_dfs_ts <- append(movements_dfs_ts, dfs_ts[[3]]$actions)
  print(movements_bfs_ts)
  
  problemTemp_bfs_gs <- initialize.problem("../data/assembler-robot-txt/assembler-robot-1.txt", movements_bfs_gs, i)
  bfs_gs <- breadth.first.search(problemTemp_bfs_gs, graph_search = T)
  movements_bfs_gs <- append(movements_bfs_gs, bfs_gs[[3]]$actions)
  print(movements_bfs_gs)
  
  problemTemp_dfs_ts <- initialize.problem("../data/assembler-robot-txt/assembler-robot-1.txt", movements_dfs_ts, i)
  dfs_ts <- depth.first.search(problemTemp_dfs_ts, max_iterations = 2500, count_print = 1000)
  print(dfs_ts)
  movements_dfs_ts <- append(movements_dfs_ts, dfs_ts[[3]]$actions)
  print(movements_dfs_ts)
  
  problemTemp_dfs_gs <- initialize.problem("../data/assembler-robot-txt/assembler-robot-1.txt", movements_dfs_gs, i)
  dfs_gs <- depth.first.search(problemTemp_dfs_gs, max_iterations = 2500, count_print = 1000, graph_search = TRUE)
  movements_dfs_gs <- append(movements_dfs_gs, dfs_gs[[3]]$actions)
  print(movements_dfs_gs)
  
  problemTemp_dls6_ts <- initialize.problem("../data/assembler-robot-txt/assembler-robot-1.txt", movements_dls6_ts, i)
  dls6_ts <- depth.limited.search(problemTemp_dls6_ts, depth_limit = 6, max_iterations = 4000, count_print = 1000)
  movements_dls6_ts <- append(movements_dls6_ts, dls6_ts[[3]]$actions)
  print(movements_dls6_ts)
  
  problemTemp_dls6_gs <- initialize.problem("../data/assembler-robot-txt/assembler-robot-1.txt", movements_dls6_gs, i)
  dls6_gs <- depth.limited.search(problemTemp_dls6_gs, depth_limit = 6, max_iterations = 4000, count_print = 1000, graph_search = TRUE)
  movements_dls6_gs <- append(movements_dls6_gs, dls6_gs[[3]]$actions)
  print(movements_dls6_gs)
  
  problemTemp_dls10_ts <- initialize.problem("../data/assembler-robot-txt/assembler-robot-1.txt", movements_dls10_ts, i)
  dls10_ts <- depth.limited.search(problemTemp_dls10_ts, depth_limit = 10, max_iterations = 3500, count_print = 1000)
  movements_dls10_ts <- append(movements_dls10_ts, dls10_ts[[3]]$actions)
  print(movements_dls10_ts)
  
  problemTemp_dls10_gs <- initialize.problem("../data/assembler-robot-txt/assembler-robot-1.txt", movements_dls10_gs, i)
  dls10_gs <- depth.limited.search(problemTemp_dls10_gs, depth_limit = 10, max_iterations = 2500, count_print = 1000, graph_search = TRUE)
  movements_dls10_gs <- append(movements_dls10_gs, dls10_gs[[3]]$actions)
  print(movements_dls10_gs)
  
  problemTemp_ids_ts <- initialize.problem("../data/assembler-robot-txt/assembler-robot-1.txt", movements_ids_ts, i)
  ids_ts <- iterative.deepening.search(problemTemp_ids_ts, max_iterations = 2500, count_print = 1000)
  movements_ids_ts <- append(movements_ids_ts, ids_ts[[3]]$actions)
  print(movements_ids_ts)
  
  problemTemp_ids_gs <- initialize.problem("../data/assembler-robot-txt/assembler-robot-1.txt", movements_ids_gs, i)
  ids_gs <- iterative.deepening.search(problemTemp_ids_gs, max_iterations = 2500, count_print = 1000, graph_search = TRUE)
  movements_ids_gs <- append(movements_ids_gs, ids_gs[[3]]$actions)
  print(movements_ids_gs)
}

analyze.results(list(bfs_ts, bfs_gs, dfs_ts, dfs_gs, dls6_ts, dls6_gs, dls10_ts, dls10_gs, ids_ts, ids_gs), problemTemp)



problem <- initialize.problem("../data/assembler-robot-txt/assembler-robot-2.txt")

bfs_ts <- breadth.first.search(problem)   
bfs_gs <- breadth.first.search(problem, graph_search = T)
dfs_ts <- depth.first.search(problem, max_iterations = 2500, count_print = 1000)
dfs_gs <- depth.first.search(problem, max_iterations = 2500, count_print = 1000, graph_search = TRUE)
dls6_ts <- depth.limited.search(problem, depth_limit = 6, max_iterations = 4000, count_print = 1000)
dls6_gs <- depth.limited.search(problem, depth_limit = 6, max_iterations = 4000, count_print = 1000, graph_search = TRUE)
dls10_ts <- depth.limited.search(problem, depth_limit = 10, max_iterations = 3500, count_print = 1000)
dls10_gs <- depth.limited.search(problem, depth_limit = 10, max_iterations = 2500, count_print = 1000, graph_search = TRUE)
ids_ts <- iterative.deepening.search(problem, max_iterations = 2500, count_print = 1000)
ids_gs <- iterative.deepening.search(problem, max_iterations = 2500, count_print = 1000, graph_search = TRUE)

analyze.results(list(bfs_ts, bfs_gs, dfs_ts, dfs_gs, dls6_ts, dls6_gs, dls10_ts, dls10_gs, ids_ts, ids_gs), problem)

problem <- initialize.problem("../data/assembler-robot-txt/assembler-robot-3.txt")

bfs_ts <- breadth.first.search(problem)   
bfs_gs <- breadth.first.search(problem, graph_search = T, max_iterations = 100000)
dfs_ts <- depth.first.search(problem, max_iterations = 10000, count_print = 1000)
dfs_gs <- depth.first.search(problem, max_iterations = 2500, count_print = 1000, graph_search = TRUE)
dls6_ts <- depth.limited.search(problem, depth_limit = 6, max_iterations = 4000, count_print = 1000)
dls6_gs <- depth.limited.search(problem, depth_limit = 6, max_iterations = 4000, count_print = 1000, graph_search = TRUE)
dls10_ts <- depth.limited.search(problem, depth_limit = 10, max_iterations = 3500, count_print = 1000)
dls10_gs <- depth.limited.search(problem, depth_limit = 10, max_iterations = 2500, count_print = 1000, graph_search = TRUE)
ids_ts <- iterative.deepening.search(problem, max_iterations = 2500, count_print = 1000)
ids_gs <- iterative.deepening.search(problem, max_iterations = 2500, count_print = 1000, graph_search = TRUE)

analyze.results(list(bfs_ts, bfs_gs, dfs_ts, dfs_gs, dls6_ts, dls6_gs, dls10_ts, dls10_gs, ids_ts, ids_gs), problem)

problem <- initialize.problem("../data/assembler-robot-txt/assembler-robot-4.txt")

bfs_ts <- breadth.first.search(problem)   
bfs_gs <- breadth.first.search(problem, graph_search = T)
dfs_ts <- depth.first.search(problem, max_iterations = 2500, count_print = 1000)
dfs_gs <- depth.first.search(problem, max_iterations = 2500, count_print = 1000, graph_search = TRUE)
dls6_ts <- depth.limited.search(problem, depth_limit = 6, max_iterations = 4000, count_print = 1000)
dls6_gs <- depth.limited.search(problem, depth_limit = 6, max_iterations = 4000, count_print = 1000, graph_search = TRUE)
dls10_ts <- depth.limited.search(problem, depth_limit = 10, max_iterations = 3500, count_print = 1000)
dls10_gs <- depth.limited.search(problem, depth_limit = 10, max_iterations = 2500, count_print = 1000, graph_search = TRUE)
ids_ts <- iterative.deepening.search(problem, max_iterations = 2500, count_print = 1000)
ids_gs <- iterative.deepening.search(problem, max_iterations = 2500, count_print = 1000, graph_search = TRUE)

analyze.results(list(bfs_ts, bfs_gs, dfs_ts, dfs_gs, dls6_ts, dls6_gs, dls10_ts, dls10_gs, ids_ts, ids_gs), problem)