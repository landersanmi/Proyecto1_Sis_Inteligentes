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
      print("Esto es X")
      print(size[[1]][1])
      print("Esto es Y")
      print(size[[1]][2])
      problem$rows <- strtoi(size[[1]][1])
      problem$columns  <- strtoi(size[[1]][2])
    
    }else if(contador == 1){
      mountPoint <- strsplit(line, ",")
      mountX <- strtoi(mountPoint[[1]][1])
      mountY <- strtoi(mountPoint[[1]][2])
      
    }else if(contador == 2){
      assemblyPoints <- strsplit(line, ";")
      assemblyX <- c()
      assemblyY <- c()
      counter <- 1
      for (point in assemblyPoints){
        coordenadas <- strsplit(point, ",")
        print(coordenadas)
        for(coordenada in coordenadas){
          assemblyX <- append(assemblyX, strtoi(coordenada[[1]][1]))
          assemblyY <- append(assemblyY, strtoi(coordenada[[2]][1]))
          print(assemblyX)
          print(assemblyY)
        }
      }
      
    }else if(contador == 3){
      forbiddenPoints <- strsplit(line, ";")
      forbiddenX <- c()
      forbiddenY <- c()
      for (point in forbiddenPoints){
        coordenadas <- strsplit(point, ",")
        print(coordenadas)
        for(coordenada in coordenadas){
          forbiddenX <- append(forbiddenX, strtoi(coordenada[[1]][1]))
          forbiddenY <- append(forbiddenY, strtoi(coordenada[[2]][1]))
          print(forbiddenX)
          print(forbiddenY)
        }
      }
      
    }else if ( length(line) == 0 ) {
      break
    }
    print(line)
    contador <- contador +1
  }
  
  close(conn)
  ##############################################################################
  #print(problem$rows)
  #print(problem$columns)
  initialState <- list( matrix(0, nrow = problem$rows, ncol = problem$columns), c(0), c() )
  names(initialState) <- c("mat", "order", "lastActions")
  initialState <- as.data.frame(t(initialState))
  #initialState <- as.data.frame(t(initialState))
  #print("MOUNT X -->")
  #print(mountX)
  #print("MOUNT Y -->")
  #print(mountY)

  initialState$mat[[1]][mountX, mountY] <- -1
  
  for (i in 1:length(assemblyX)){
    initialState$mat[[1]][assemblyX[i], assemblyY[i]] <- i
  }
  for (i in 1:length(forbiddenX)){
    initialState$mat[[1]][forbiddenX[i], forbiddenY[i]] <- -4
  }
  
  for (move in movements) {
    initialState <- effect(initialState, move, problem)
    print(move)
  }
  print("INITIAL STATE APLICADO MOVIMIENTOS")
  print(initialState[[1]])
  problem$state_initial     <- initialState
  #print(initialState[1])
  #problem$state_final       <- <INSERT CODE HERE>
  problem$actions_possible  <- data.frame(direction = c("Up", "Down", "Left", "Right"), stringsAsFactors = FALSE)
  
  # You can add additional attributes
  problem$assembly_cuantity <- length(assemblyX)
  problem$target <- target

  
  return(problem)
}


# Analyzes if an action can be applied in the received state.
is.applicable <- function (state, action, problem) {
  #print("STATE RECIBIDO -->")
  #print(state)
  stateMat <- state[[1]][1]
  stateMat <- stateMat[[1]]
  print("STATEMAT RECIBIDO -->")
  print(stateMat)
  #print("Cantidad de -2 -->")
  #print(length(which(stateMat == -2, arr.ind = TRUE)))
  if(length(which(as.numeric(unlist(stateMat)) == -2 )) == 0){
    #print("1º ITERATIONS")
    where <- which(stateMat == -1, arr.ind = TRUE)
    row <- where[1]
    col <- where[2]
  }else{
    #print("STATEMAT RECIBIDO -->")
    #print(stateMat)
    #print("NEXT ITERATIONS")
    where <- which(stateMat == -2, arr.ind = TRUE)
    row <- where[1]
    col <- where[2]
  }
  
  # any_hole_rows <- FALSE
  # any_hole_columns <- FALSE
  # for (i in 1:problem$columns){
  #   #print("SE VA A ANALIZAR LA SIGUIENTE COLUMNA")
  #   #print(stateMat[, i])
  #   #print(stateMat[, i] >= 1)
  #   if (stateMat[, i] >= 1){
  #     any_hole_columns <- TRUE
  #     #print("AGUJERO ENCONTRADO")
  #   }
  #   if(!any_hole_columns){
  #     #print("COLUMNA ATASCADA -->")
  #     #print(i)
  #     for (j in 1:problem$columns-i){
  #       if (stateMat[, j+i] > 0){
  #         for (k in 1:i-1) {
  #           if (stateMat[, k] == -2){
  #             #print("EL ESTADO SE DESTRUYE")
  #             return(FALSE)
  #           }
  #         }   
  #       }
  #     }
  #   }
  # }
  # for (l in 1:problem$rows){
  #   #print("SE VA A ANALIZAR LA SIGUIENTE FILA")
  #   #print(stateMat[l, ])
  #   #print(stateMat[l, ] >= 1)
  #   if (stateMat[l, ] >= 1){
  #     any_hole_rows <- TRUE
  #     #print("AGUJERO ENCONTRADO")
  #   }
  #   if(!any_hole_rows){
  #     #print("FILA ATASCADA -->")
  #     #print(l)
  #     for (m in 1:problem$rows-l){
  #       if (stateMat[m+l, ] > 0){
  #         for (n in 1:l-1) {
  #           if (stateMat[n, ] == -2){
  #             #print("EL ESTADO SE DESTRUYE")
  #             return(FALSE)
  #           }
  #         }   
  #       }
  #     }
  #   }
  # }

  
  
  
  is_correct_number <- TRUE
  is_util_movement <- TRUE
  locked_numbers <- FALSE
  if (action == "Up") {
    #print("Up")
    #print(stateMat)
    #print(row != 1 && stateMat[row-1, col] != -4 && stateMat[row-1, col] != -3 && stateMat[row-1, col] != -1)
    if (row !=1 && stateMat[row-1, col] >= 1){
      is_correct_number <- stateMat[row-1, col] == problem$target
    }
    if(state[[3]][1] == "Down" && (state[[3]][2] == "Left" || state[[3]][2] == "Right")){
      is_util_movement <- FALSE
    }

    return(row != 1 && stateMat[row-1, col] != -4 && stateMat[row-1, col] != -3 && stateMat[row-1, col] != -1 && is_correct_number && is_util_movement)
  }
  
  if (action == "Down") {
    #print("Down")
    #print(row != problem$rows && stateMat[row+1, col] != -4 && stateMat[row+1, col] != -3 && stateMat[row+1, col] != -1)
    if (row != problem$rows && stateMat[row+1, col] > 0){
      is_correct_number <- stateMat[row+1, col] == problem$target
    }
    if(state[[3]][1] == "Up" && (state[[3]][2] == "Left" || state[[3]][2] == "Right")){
      is_util_movement <- FALSE
    }
    return(row != problem$rows && stateMat[row+1, col] != -4 && stateMat[row+1, col] != -3 && stateMat[row+1, col] != -1 && is_correct_number && is_util_movement)
  }
  
  if (action == "Left") {
    #print("Left")
    #print(col != 1 && stateMat[row, col-1] >= 0)
    if (col !=1 && stateMat[row, col-1] > 0){
      is_correct_number <- stateMat[row, col-1] == problem$target
    }
    if(state[[3]][1] == "Right" && (state[[3]][2] == "Left" || state[[3]][2] == "Right")){
      is_util_movement <- FALSE
    }
    return(col != 1 && stateMat[row, col-1] != -4 && stateMat[row, col-1] != -3 && stateMat[row, col-1] != -1 && is_correct_number && is_util_movement)
  }
  
  if (action == "Right") {
    #print("Right")
    #print(col != problem$columns && stateMat[row, col+1] != -4 && stateMat[row, col+1] != -3 && stateMat[row, col+1] != -1)
    if (col != problem$columns && stateMat[row, col+1] > 0){
      is_correct_number <- stateMat[row, col+1] == problem$target
    }
    if(state[[3]][1] == "Left" && (state[[3]][2] == "Left" || state[[3]][2] == "Right")){
      is_util_movement <- FALSE
    }
    return(col != problem$columns && stateMat[row, col+1] != -4 && stateMat[row, col+1] != -3 && stateMat[row, col+1] != -1 && is_correct_number && is_util_movement)
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
    if(mat[row-1, col] > 0){
      result[[2]]$order <- append(result[[2]]$order, mat[row-1, col])
    }
    result$mat[[1]][row-1, col] <- -2
    result$mat[[1]][row, col] <- -3
    if (is.null(result[[3]]$lastActions)){
      result[[3]]$lastActions[1] <- "Up"
    }else if (length(result[[2]]$lastActions) == 1 ){
      result[[3]]$lastActions[2] <- "Up"
    }else{
      result[[3]]$lastActions[1] <- result[[3]]$lastActions[2]
      result[[3]]$lastActions[2] <- "Up"
    }
    print(result$mat)
    return(result)
  }
  
  if (action == "Down") {
    if(mat[row+1, col] > 0){
      result[[2]]$order <- append(result[[2]]$order, mat[row+1, col])
    }
    result$mat[[1]][row+1, col] <- -2
    result$mat[[1]][row, col] <- -3
    if (is.null(result[[3]]$lastActions)){
      result[[3]]$lastActions[1] <- "Down"
    }else if (length(result[[2]]$lastActions) == 1 ){
      result[[3]]$lastActions[2] <- "Down"
    }else{
      result[[3]]$lastActions[1] <- result[[3]]$lastActions[2]
      result[[3]]$lastActions[2] <- "Down"
    }
    print(result$mat)
    return(result)
  }
  
  if (action == "Left") {
    if(mat[row, col-1] > 0){
      result[[2]]$order <- append(result[[2]]$order, mat[row, col-1])
    }
    result$mat[[1]][row, col-1] <- -2
    result$mat[[1]][row, col] <- -3
    if (is.null(result[[3]]$lastActions)){
      result[[3]]$lastActions[1] <- "Left"
    }else if (length(result[[2]]$lastActions) == 1 ){
      result[[3]]$lastActions[2] <- "Left"
    }else{
      result[[3]]$lastActions[1] <- result[[3]]$lastActions[2]
      result[[3]]$lastActions[2] <- "Left"
    }
    print(result$mat)
    return(result)
  }
  
  if (action == "Right") {
    if(mat[row, col+1] > 0){
      result[[2]]$order <- append(result[[2]]$order, mat[row, col+1])
    }
    result$mat[[1]][row, col+1] <- -2
    result$mat[[1]][row, col] <- -3
    if (is.null(result[[3]]$lastActions)){
      result[[3]]$lastActions[1] <- "Right"
    }else if (length(result[[2]]$lastActions) == 1 ){
      result[[3]]$lastActions[2] <- "Right"
    }else{
      result[[3]]$lastActions[1] <- result[[3]]$lastActions[2]
      result[[3]]$lastActions[2] <- "Right"
    }
    print(result$mat)
    return(result)
  }

  return(result)
}

# Analyzes if a state is final or not
is.final.state <- function (state, final_satate, problem) {
  result <- FALSE # Default value is FALSE.
  
  stateMat <- state[[1]][1]
  stateMat <- stateMat[[1]]
  target <- problem$target
  return(length(which(as.numeric(unlist(stateMat)) == target )) == 0)
  
  # piecesOrdered <- TRUE
  # #print("ENTRANDO EN BUCLE CON...")
  # #print(state[[2]]$order)
  # for(i in 1:length(state[[2]]$order)){
  #   if(i-1 != state[[2]]$order[i]){
  #     piecesOrdered <- FALSE
  #   }
  # }
  # #print("COMPROBANDO STATE")
  # stateMat <- state[[1]]
  # #print(stateMat)
  # result <- ( length(which(as.numeric(unlist(stateMat)) > 0, arr.ind = TRUE)) == 0 && piecesOrdered)
  # #print("Es eñ resultado final?")
  # #print(result)
  # 
  # return(result)
}

# Transforms a state into a string
to.string = function (state, problem) {
  #print(state)
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

