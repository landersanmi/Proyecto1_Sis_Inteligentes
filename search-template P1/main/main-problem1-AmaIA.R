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


filename <- "../data/assembler-robot-txt/assembler-robot-1.txt"


problem <- initialize.problem(filename, c(), 1)
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
movements_gbfs_ts <- c()
movements_gbfs_gs <- c()
movements_ucs_ts <- c()
movements_ucs_gs <- c()
bfs_ts <- list()
bfs_gs <- list()
dfs_ts <- list()
dfs_gs <- list()
dls6_ts <- list()
dls6_gs <- list()
dls10_ts <- list()
dls10_gs <- list()
ids_ts <- list()
ids_gs <- list()
gbfs_ts <- list()
gbfs_gs <- list()
ucs_ts <- list()
ucs_gs <- list()

for (i in 1: problem$assembly_cuantity) {
    
  problemTemp_bfs_ts <- initialize.problem(filename, movements_bfs_ts, i)
  bfs_ts_Temp <- breadth.first.search(problemTemp_bfs_ts, max_iterations = 10000)
  if(!(length(bfs_ts) == 0)){
    bfs_ts$runtime <- bfs_ts$runtime + bfs_ts_Temp$runtime
    bfs_ts$state_final$actions <- append(bfs_ts$state_final$actions, bfs_ts_Temp$state_final$actions)
    bfs_ts$state_final$cost <- bfs_ts$state_final$cost + bfs_ts_Temp$state_final$cost
  }else{
    bfs_ts <- bfs_ts_Temp
  }
  movements_bfs_ts <- append(movements_bfs_ts, bfs_ts_Temp$state_final$actions)
  print("MOVIMIENTOS EN BFS_TS")
  print(i)
  print(movements_bfs_ts)
  ####################################################################################################################
  problemTemp_bfs_gs <- initialize.problem(filename, movements_bfs_gs, i)
  bfs_gs_Temp <- breadth.first.search(problemTemp_bfs_gs, graph_search = T, max_iterations = 10000)
  if(!(length(bfs_gs) == 0)){
    bfs_gs$runtime <- bfs_gs$runtime + bfs_gs_Temp$runtime
    bfs_gs$state_final$actions <- append(bfs_gs$state_final$actions, bfs_gs_Temp$state_final$actions)
    bfs_gs$state_final$cost <- bfs_gs$state_final$cost + bfs_gs_Temp$state_final$cost
  }else{
    bfs_gs <- bfs_gs_Temp
  }
  movements_bfs_gs <- append(movements_bfs_gs, bfs_gs_Temp$state_final$actions)
  print("MOVIMIENTOS EN BFS_GS")
  print(i)
  print(movements_bfs_gs)
  ###################################################################################################################
    problemTemp_dfs_ts <- initialize.problem(filename, movements_dfs_ts, i)
    dfs_ts_Temp <- depth.first.search(problemTemp_dfs_ts, max_iterations = 5000, count_print = 1000)
    if(!(length(dfs_ts) == 0)){
      dfs_ts$runtime <- dfs_ts$runtime + dfs_ts_Temp$runtime
      dfs_ts$state_final$actions <- append(dfs_ts$state_final$actions, dfs_ts_Temp$state_final$actions)
      dfs_ts$state_final$cost <- dfs_ts$state_final$cost + dfs_ts_Temp$state_final$cost
    }else{
      dfs_ts <- dfs_ts_Temp
    }
    movements_dfs_ts <- append(movements_dfs_ts, dfs_ts_Temp$state_final$actions)
    print(movements_dfs_ts)
    ######################################################################################################################
  problemTemp_dfs_gs <- initialize.problem(filename, movements_dfs_gs, i)
  dfs_gs_Temp <- depth.first.search(problemTemp_dfs_gs, max_iterations = 5000, count_print = 1000, graph_search = TRUE)
  if(!(length(dfs_gs) == 0)){
    dfs_gs$runtime <- dfs_gs$runtime + dfs_gs_Temp$runtime
    dfs_gs$state_final$actions <- append(dfs_gs$state_final$actions, dfs_gs_Temp$state_final$actions)
    dfs_gs$state_final$cost <- dfs_gs$state_final$cost + dfs_gs_Temp$state_final$cost
  }else{
    dfs_gs <- dfs_gs_Temp
  }
  movements_dfs_gs <- append(movements_dfs_gs, dfs_gs_Temp$state_final$actions)
  print(movements_dfs_gs)
  ######################################################################################################################
  problemTemp_dls6_ts <- initialize.problem(filename, movements_dls6_ts, i)
  dls6_ts_Temp <- depth.limited.search(problemTemp_dls6_ts, depth_limit = 11, max_iterations = 40000, count_print = 1000)
  if(!(length(dls6_ts) == 0)){
    dls6_ts$runtime <- dls6_ts$runtime + dls6_ts_Temp$runtime
    dls6_ts$state_final$actions <- append(dls6_ts$state_final$actions, dls6_ts_Temp$state_final$actions)
    dls6_ts$state_final$cost <- dls6_ts$state_final$cost + dls6_ts_Temp$state_final$cost
  }else{
    dls6_ts <- dls6_ts_Temp
  }
  movements_dls6_ts <- append(movements_dls6_ts, dls6_ts_Temp$state_final$actions)
  print("MOVIMIENTOS EN DLS6")
  print(movements_dls6_ts)
    #######################################################################################################################
  problemTemp_dls6_gs <- initialize.problem(filename, movements_dls6_gs, i)
  dls6_gs_Temp <- depth.limited.search(problemTemp_dls6_gs, depth_limit = 11, max_iterations = 40000, count_print = 1000, graph_search = TRUE)
  if(!(length(dls6_gs) == 0)){
    dls6_gs$runtime <- dls6_gs$runtime + dls6_gs_Temp$runtime
    dls6_gs$state_final$actions <- append(dls6_gs$state_final$actions, dls6_gs_Temp$state_final$actions)
    dls6_gs$state_final$cost <- dls6_gs$state_final$cost + dls6_gs_Temp$state_final$cost
  }else{
    dls6_gs <- dls6_gs_Temp
  }
  movements_dls6_gs <- append(movements_dls6_gs, dls6_gs_Temp$state_final$actions)
  print("MOVIMIENTOS EN DLS6-GS")
  print(movements_dls6_gs)
  ######################################################################################################################
  problemTemp_dls10_ts <- initialize.problem(filename, movements_dls10_ts, i)
  dls10_ts_Temp <- depth.limited.search(problemTemp_dls10_ts, depth_limit =15, max_iterations = 10000, count_print = 1000)
  if(!(length(dls10_ts) == 0)){
    dls10_ts$runtime <- dls10_ts$runtime + dls10_ts_Temp$runtime
    dls10_ts$state_final$actions <- append(dls10_ts$state_final$actions, dls10_ts_Temp$state_final$actions)
    dls10_ts$state_final$cost <- dls10_ts$state_final$cost + dls10_ts_Temp$state_final$cost
  }else{
    dls10_ts <- dls10_ts_Temp
  }
  movements_dls10_ts <- append(movements_dls10_ts, dls10_ts_Temp$state_final$actions)
  print("MOVIMIENTOS EN DLS10")
  print(movements_dls10_ts)
  #######################################################################################################################
  problemTemp_dls10_gs <- initialize.problem(filename, movements_dls10_gs, i)
  dls10_gs_Temp <- depth.limited.search(problemTemp_dls10_gs, depth_limit = 15, max_iterations = 10000, count_print = 1000, graph_search = TRUE)
  if(!(length(dls10_gs) == 0)){
    dls10_gs$runtime <- dls10_gs$runtime + dls10_gs_Temp$runtime
    dls10_gs$state_final$actions <- append(dls10_gs$state_final$actions, dls10_gs_Temp$state_final$actions)
    dls10_gs$state_final$cost <- dls10_gs$state_final$cost + dls10_gs_Temp$state_final$cost
  }else{
    dls10_gs <- dls10_gs_Temp
  }
  movements_dls10_gs <- append(movements_dls10_gs, dls10_gs_Temp$state_final$actions)
  print("MOVIMIENTOS EN DLS10-GS")
  print(movements_dls10_gs)
  #######################################################################################################################
  problemTemp_ids_ts <- initialize.problem(filename, movements_ids_ts, i)
  ids_ts_Temp <- iterative.deepening.search(problemTemp_ids_ts, max_iterations = 40000, count_print = 1000)
  if(!(length(ids_ts) == 0)){
    ids_ts$runtime <- ids_ts$runtime + ids_ts_Temp$runtime
    ids_ts$state_final$actions <- append(ids_ts$state_final$actions, ids_ts_Temp$state_final$actions)
    ids_ts$state_final$cost <- ids_ts$state_final$cost + ids_ts_Temp$state_final$cost
  }else{
    ids_ts <- ids_ts_Temp
  }
  movements_ids_ts <- append(movements_ids_ts, ids_ts_Temp$state_final$actions)
  print("MOVIMIENTOS EN IDS_TS")
  print(movements_ids_ts)
  #########################################################################################################################
  problemTemp_ids_gs <- initialize.problem(filename, movements_ids_gs, i)
  ids_gs_Temp <- iterative.deepening.search(problemTemp_ids_gs, max_iterations = 40000, count_print = 1000, graph_search = TRUE)
  if(!(length(ids_gs) == 0)){
    ids_gs$runtime <- ids_gs$runtime + ids_gs_Temp$runtime
    ids_gs$state_final$actions <- append(ids_gs$state_final$actions, ids_gs_Temp$state_final$actions)
    ids_gs$state_final$cost <- ids_gs$state_final$cost + ids_gs_Temp$state_final$cost
  }else{
    ids_gs <- ids_gs_Temp
  }
  movements_ids_gs <- append(movements_ids_gs, ids_gs_Temp$state_final$actions)
  print("MOVIMIENTOS EN IDS_GS")
  print(movements_ids_gs)
  # ###########################################################################################################################
  problemTemp_gbfs_ts <- initialize.problem(filename, movements_gbfs_ts, i)
  gbfs_ts_Temp <- greedy.best.first.search(problemTemp_gbfs_ts, max_iterations = 100000, count_print = 1000)
  if(!(length(gbfs_ts) == 0)){
    gbfs_ts$runtime <- gbfs_ts$runtime + gbfs_ts_Temp$runtime
    gbfs_ts$state_final$actions <- append(gbfs_ts$state_final$actions, gbfs_ts_Temp$state_final$actions)
    gbfs_ts$state_final$cost <- gbfs_ts$state_final$cost + gbfs_ts_Temp$state_final$cost
  }else{
    gbfs_ts <- gbfs_ts_Temp
  }
  movements_gbfs_ts <- append(movements_gbfs_ts, gbfs_ts_Temp$state_final$actions)
  print("MOVIMIENTOS EN GBFS_TS")
  print(movements_gbfs_ts)
  ############################################################################################################################
  problemTemp_gbfs_gs <- initialize.problem(filename, movements_gbfs_gs, i)
  gbfs_gs_Temp <- greedy.best.first.search(problemTemp_gbfs_gs, max_iterations = 100000, count_print = 1000, graph_search = TRUE)
  if(!(length(gbfs_gs) == 0)){
    gbfs_gs$runtime <- gbfs_gs$runtime + gbfs_gs_Temp$runtime
    gbfs_gs$state_final$actions <- append(gbfs_gs$state_final$actions, gbfs_gs_Temp$state_final$actions)
    gbfs_gs$state_final$cost <- gbfs_gs$state_final$cost + gbfs_gs_Temp$state_final$cost
  }else{
    gbfs_gs <- gbfs_gs_Temp
  }
  movements_gbfs_gs <- append(movements_gbfs_gs, gbfs_gs_Temp$state_final$actions)
  print("MOVIMIENTOS EN GBFS_GS")
  print(movements_gbfs_gs)
  #############################################################################################################################
  problemTemp_ucs_ts <- initialize.problem(filename, movements_ucs_ts, i)
  ucs_ts_Temp <- uniform.cost.search(problemTemp_ucs_ts, max_iterations = 10000, count_print = 1000)
  if(!(length(ucs_ts) == 0)){
    ucs_ts$runtime <- ucs_ts$runtime + ucs_ts_Temp$runtime
    ucs_ts$state_final$actions <- append(ucs_ts$state_final$actions, ucs_ts_Temp$state_final$actions)
    ucs_ts$state_final$cost <- ucs_ts$state_final$cost + ucs_ts_Temp$state_final$cost
  }else{
    ucs_ts <- ucs_ts_Temp
  }
  movements_ucs_ts <- append(movements_ucs_ts, ucs_ts_Temp$state_final$actions)
  print("MOVIMIENTOS EN UCS_TS")
  print(movements_ucs_ts)
  ##############################################################################################################################
  problemTemp_ucs_gs <- initialize.problem(filename, movements_ucs_gs, i)
  ucs_gs_Temp <- uniform.cost.search(problemTemp_ucs_gs, max_iterations = 10000, count_print = 1000, graph_search = TRUE)
  if(!(length(ucs_gs) == 0)){
    ucs_gs$runtime <- ucs_gs$runtime + ucs_gs_Temp$runtime
    ucs_gs$state_final$actions <- append(ucs_gs$state_final$actions, ucs_gs_Temp$state_final$actions)
    ucs_gs$state_final$cost <- ucs_gs$state_final$cost + ucs_gs_Temp$state_final$cost
  }else{
    ucs_gs <- ucs_gs_Temp
  }
  movements_ucs_gs <- append(movements_ucs_gs, ucs_gs_Temp$state_final$actions)
  print("MOVIMIENTOS EN UCS_GS")
  print(movements_ucs_gs)
}

analyze.results(list(bfs_ts, bfs_gs, dfs_ts, dfs_gs, dls6_ts, dls6_gs, dls10_ts, dls10_gs, ids_ts, ids_gs, gbfs_ts, gbfs_gs, ucs_ts, ucs_gs), problemTemp_bfs_ts)
analyze.results(list(  gbfs_ts, gbfs_gs), problemTemp_bfs_ts)

