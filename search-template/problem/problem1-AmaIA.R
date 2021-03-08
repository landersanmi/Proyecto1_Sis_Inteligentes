# =======================================================================
# Group Name: AmaIA
# Students: Lander San Millan, Amaia Acha
# =======================================================================
# You must implement the different functions according to your problem.
# You cannot modify the function headers because they are used by the 
# search algorithms. If you modify any headers the algorithms may not work.
# =======================================================================

# This function must return a list with the information needed to solve the problem.
# (Depending on the problem, it should receive or not parameters)
initialize.problem <- function(file, movements, target) {
  problem <- list() # Default value is an empty list.
  
  # This attributes are compulsory
  problem$name <- paste0("problem1 - [", file, "]")
  
  ######################## READING DATA FROM FILE ###############################
  conn <- file(file, "r")
  contador <- 0
  while ( TRUE ) {
    line = readLines(conn, n = 1)
    
    if (contador == 0){
      size <- strsplit(line, ",")
      problem$columns <- strtoi(size[[1]][1])
      problem$rows  <- strtoi(size[[1]][2])
    
    }else if(contador == 1){
      mountPoint <- strsplit(line, ",")
      mountY <- strtoi(mountPoint[[1]][1])
      mountX <- strtoi(mountPoint[[1]][2])
      
    }else if(contador == 2){
      assemblyPoints <- strsplit(line, ";")
      assemblyX <- c()
      assemblyY <- c()
      counter <- 1
      for (point in assemblyPoints){
        coordenadas <- strsplit(point, ",")
        for(coordenada in coordenadas){
          assemblyX <- append(assemblyX, strtoi(coordenada[[1]][1]))
          assemblyY <- append(assemblyY, strtoi(coordenada[[2]][1]))
        }
      }
      
    }else if(contador == 3){
      forbiddenPoints <- strsplit(line, ";")
      forbiddenX <- c()
      forbiddenY <- c()
      for (point in forbiddenPoints){
        coordenadas <- strsplit(point, ",")
        for(coordenada in coordenadas){
          forbiddenX <- append(forbiddenX, strtoi(coordenada[[1]][1]))
          forbiddenY <- append(forbiddenY, strtoi(coordenada[[2]][1]))
        }
      }
      
    }else if ( length(line) == 0 ) {
      break
    }
    contador <- contador +1
  }
  
  close(conn)
  ##############################################################################
  initialState <- list( matrix(0, nrow = problem$rows, ncol = problem$columns))
  names(initialState) <- c("mat")
  initialState <- as.data.frame(t(initialState))

  initialState$mat[[1]][mountX, mountY] <- -1
  
  for (i in 1:length(assemblyY)){
    initialState$mat[[1]][assemblyY[i], assemblyX[i]] <- i
  }
  for (i in 1:length(forbiddenY)){
    initialState$mat[[1]][forbiddenY[i], forbiddenX[i]] <- -4
  }
  for (move in movements) {
    initialState <- effect(initialState, move, problem)
  }
  
  print("INITIAL STATE APLICADO MOVIMIENTOS")
  print(initialState[[1]])
  problem$state_initial     <- initialState
  #problem$state_final       <- <INSERT CODE HERE>
  problem$actions_possible  <- data.frame(direction = c("Up", "Down", "Left", "Right"), stringsAsFactors = FALSE)
  problem$assembly_cuantity <- length(assemblyX)
  problem$target <- target
  
  return(problem)
}

# Analyzes if an action can be applied in the received state.
is.applicable <- function (state, action, problem) {
  
  stateMat <- state[[1]][1]
  stateMat <- stateMat[[1]]
  if(length(which(as.numeric(unlist(stateMat)) == -2 )) == 0){
    where <- which(stateMat == -1, arr.ind = TRUE)
    row <- where[1]
    col <- where[2]
  }else{
    where <- which(stateMat == -2, arr.ind = TRUE)
    row <- where[1]
    col <- where[2]
  }
  
  is_correct_number <- TRUE
  if (action == "Up") {
    if (row !=1 && stateMat[row-1, col] >= 1){
      is_correct_number <- stateMat[row-1, col] == problem$target
    }
    return(row != 1 && stateMat[row-1, col] != -4 && stateMat[row-1, col] != -3 && stateMat[row-1, col] != -1 && is_correct_number )
  }
  
  if (action == "Down") {
    if (row != problem$rows && stateMat[row+1, col] > 0){
      is_correct_number <- stateMat[row+1, col] == problem$target
    }
    return(row != problem$rows && stateMat[row+1, col] != -4 && stateMat[row+1, col] != -3 && stateMat[row+1, col] != -1 && is_correct_number)
  }
  
  if (action == "Left") {
    if (col !=1 && stateMat[row, col-1] > 0){
      is_correct_number <- stateMat[row, col-1] == problem$target
    }
    return(col != 1 && stateMat[row, col-1] != -4 && stateMat[row, col-1] != -3 && stateMat[row, col-1] != -1 && is_correct_number)
  }
  
  if (action == "Right") {
    if (col != problem$columns && stateMat[row, col+1] > 0){
      is_correct_number <- stateMat[row, col+1] == problem$target
    }
    return(col != problem$columns && stateMat[row, col+1] != -4 && stateMat[row, col+1] != -3 && stateMat[row, col+1] != -1 && is_correct_number)
  }
  
  return(FALSE)
}

# Returns the state resulting on applying the action over the state
effect <- function (state, action, problem) {
 
  stateMat <- state
  
  if(length(which(as.numeric(unlist(stateMat)) == -2, arr.ind = TRUE)) == 0){
    where <- which(stateMat$mat[[1]] == -1, arr.ind = TRUE)
    row <- where[1]
    col <- where[2]
  
  }else{
    where <- which(stateMat$mat[[1]] == -2, arr.ind = TRUE)
    row <- where[1]
    col <- where[2]
  }
  
  mat <- stateMat$mat[[1]]
  result <- state
    
  if (action == "Up") {
    result$mat[[1]][row-1, col] <- -2
    result$mat[[1]][row, col] <- -3
    return(result)
  }
  
  if (action == "Down") {
    result$mat[[1]][row+1, col] <- -2
    result$mat[[1]][row, col] <- -3
    return(result)
  }
  
  if (action == "Left") {
    result$mat[[1]][row, col-1] <- -2
    result$mat[[1]][row, col] <- -3
    return(result)
  }
  
  if (action == "Right") {
    result$mat[[1]][row, col+1] <- -2
    result$mat[[1]][row, col] <- -3
    return(result)
  }

  return(result)
}

# Analyzes if a state is final or not
is.final.state <- function (state, final_satate, problem) {
  stateMat <- state[[1]][1]
  stateMat <- stateMat[[1]]
  target <- problem$target
  return(length(which(as.numeric(unlist(stateMat)) == target )) == 0)
}

# Transforms a state into a string
to.string = function (state, problem) {
  print(state)
}

# Returns the cost of applying an action over a state
get.cost <- function (action, state, problem) {
  
  # <INSERT YOUR CODE HERE TO RETURN THE COST OF APPLYING THE ACTION ON THE STATE> 
  
  return(1) # Default value is 1.
}

# Heuristic function used by Informed Search Algorithms
get.evaluation <- function(state, problem) {
  
  # <INSERT YOUR CODE HERE TO RETURN THE RESULT OF THE EVALUATION FUNCTION>
  
  return(1) # Default value is 1.
}

