#####     Author: Adrianne D. Solis      #####
#####     Diet Problem Solver: Simplex   #####

# [1] Import Data

food_data <- read_excel("foodtable.xlsx")   # nutritional data and prices per food item
#View(food_data)

# food names
foodNames = food_data$Foods

# convert food names and respective row numbers (Vectors) into lists
foodNamesList = as.list(foodNames)
foodValuesList = as.list(1:64)

# serving sizes per food items
foodServingSizes = cbind(food_data$Foods, food_data$`Serving Size`)
colnames(foodServingSizes) = c("Food Item","Serving Size")

# minimum and maximum nutrients needed
min_nutrient = c(1900,0,0,0,0,25,50,5000,50,800,10)
max_nutrient = c(2250, 300, 65, 2400, 300, 100, 100, 50000, 20000, 1600, 30)
maxServing = 10 # maximum servings

# [2] Formulate objective function & constraints

## sample only; depends on input by user
# foodVector = 1:20                            # vector containing indices of selected food items

createDualProblemMatrix <- function(foodVector, food_data, maxServing, min_nutrient, max_nutrient) {
  n = length(foodVector)
  cost_vector = c(food_data$`Price/Serving`[  foodVector  ],1)

  # coefficients of the objective function
  objectiveCoeff = cost_vector                 # vector containing price/cost per food serving
  objectiveCoeff = as.matrix(objectiveCoeff)

  # Setting up the dual problem tableau

  # [2.a] Constraint set 1: nutrientj1*x1 + ... + nutrientjn*xn > min_Nutrientj

  food_nutrients_mtx = food_data[  foodVector, 4:14  ]; food_nutrients_mtx = as.matrix(food_nutrients_mtx)
  min_nutrients_mtx = rbind(food_nutrients_mtx, min_nutrient)

  # [2.b] Constraint set 2: nutrientj1*x1 + ... + nutrientjn*xn < max_Nutrientj
  #                         -> -(nutrientj1*x1 + ... + nutrientjn*xn) > -(max_Nutrientj)

  max_nutrients_mtx = rbind(-food_nutrients_mtx, -max_nutrient)

  # [2.c] Constraint set 3: xi < max_serving       for all i = 1,...,n
  #                         -> -xi > -max_serving

  max_serving_matrix = matrix(0, nrow=n, ncol=n); diag(max_serving_matrix) = -1
  max_serving_matrix = rbind(max_serving_matrix, rep(-maxServing,n)) # 10 as max food serving
  max_serving_matrix = as.matrix(max_serving_matrix)

  # [2.c] Dual Problem Matrix
  dual_mtx = cbind(min_nutrients_mtx, max_nutrients_mtx, max_serving_matrix, objectiveCoeff)
  colnames(dual_mtx) = NULL; rownames(dual_mtx) = NULL

  lis = list( cost_vector = cost_vector, dual_mtx = dual_mtx )
  return(lis)
}

# [3] Set up tableau

tableau <- function(dual_mtx) {
  # matrix for slack variables
  idenMtx = matrix(0, nrow=nrow(dual_mtx), ncol=nrow(dual_mtx)); diag(idenMtx) = 1;

  tableau = cbind(dual_mtx[,1:(ncol(dual_mtx)-1)], idenMtx, dual_mtx[,ncol(dual_mtx)]) # form the tableau
  tableau[nrow(tableau),ncol(tableau)] = 0
  tableau[nrow(tableau),-(ncol(tableau)-1)] = (-1)*(tableau[nrow(tableau),-(ncol(tableau)-1)]) # negate coefficients in last row
  return(tableau)
   # tableau      # initial tableau
}

# [4] Function definitions for simplex solver

Simplex <- function(tableau) {

  iterations = data.frame()
  row = rep("",ncol(tableau)-1)
  cnt = 0
  solution = c()

  # assign number of rows and columns to variables
  nrow = nrow(tableau)
  ncol = ncol(tableau)

  while (all( tableau[nrow,-ncol] >= 0) == FALSE) { # check if there are negative numbers in the last row (Except solution column)

    lastRow = tableau[nrow,]

    # [1] select pivot column
    pc = which.min(tableau[nrow,])

    # [2] select pivot row
    # find test ratios
    min = -1
    for (i in 1:(nrow-1)) {
      if (tableau[i,pc] > 0) {   # skip negative elements in pivot colummn
        if (min == -1 || tableau[i,ncol]/tableau[i,pc] < min) {
          min = tableau[i,ncol]/tableau[i,pc]
          pr = i
        }
      }
    }

    # indicates that pivot row was not found (elements in pivot column are <= 0)
    if (min == -1) {
      return(list(feasible=0, finalTableau=tableau,Z=tableau[nrow,ncol], iterations = iterations, count = cnt))
    }

    # [3] select pivot element
    pe <- tableau[pr,pc]

    # [4] normalize pivot row
    tableau[pr,] = (tableau[pr,])/pe

    # [5] "eliminate" elements
    for (i in 1:nrow) {

      if (i == pr) {next}  # skip the pivot element
      tv = tableau[i,pc]*(tableau[pr,])     # calculate temporary vector ( multiplier * pivot row ) wherein the multiplier is the element we want to be 0
      tableau[i,] = tableau[i,] - tv        # update matrix row i

    }

    # basic solution per iteration
    for (i in 1:(ncol-1)) {  # iterate through columns and check for solutions
      hasSolution = FALSE
      for (j in 1:nrow) {
        if (tableau[j,i] != 0) { # check for nonzero value in column
          if (hasSolution == FALSE) {
            hasSolution = TRUE
            solRow = j
          } else if (hasSolution == TRUE) { # more than one nonzero value in the column, solution for the variable = 0
            hasSolution = FALSE
            break
          }
        }
      }

      if (hasSolution == TRUE) { # add solution to solution vector
        solution = c(solution, tableau[solRow,ncol]/tableau[solRow,i])
      } else {
        solution = c(solution, 0)
      }
    }

    cnt = cnt + 1
    iterations = rbind(iterations, c(paste("Iteration", cnt),row), c("",row), tableau, c("Basic solution:", row), c(solution), c("",row))

  }

  # set up the basic solution matrix
  basicSol = matrix(tableau[nrow,-(ncol-1)], nrow=1)
  colnames(basicSol) = colnames(tableau[,-ncol])

  lis = list(feasible=1, finalTableau=tableau, basicSolution=basicSol, Z=tableau[nrow,ncol], iterations = iterations, count = cnt)
  return(lis)
}

# [5] Obtain data frame with Food items, Serving sizes, Cost

solveDiet <- function(foodVector) {

  # create tableau
  dual = createDualProblemMatrix(foodVector, food_data, maxServing, min_nutrient, max_nutrient)
  cost_vector = dual$cost_vector
  dual_mtx = dual$dual_mtx
  tableau = tableau(dual_mtx)
  n = length(foodVector)

  # solve using simplex method
  simplexSol = Simplex(tableau)

  # setup column names
  colNames = c( colNames_cons = c( paste('S',1:(22+n),sep="") ),
                colNames_vars = c( paste('X',1:n, sep="") ), 'Z', 'Solution')
  colnames(tableau) = colNames

  if (simplexSol$feasible == 0) {
    colnames(simplexSol$iterations) = colNames;   colnames(tableau) = colNames
    lis = list(feasible=0, initialTableau = tableau, finalTableau = simplexSol$finalTableau,
               basicSolution = simplexSol$basicSolution, optimumCost = simplexSol$Z, iterations = simplexSol$iterations, count = simplexSol$count)
    return(lis)
  }


  # format final table of serving sizes and cost per food item
  servingSizes = simplexSol$basicSolution[(22+n+1):(22+n+1+n-1)]

  foodCost = servingSizes*cost_vector[-length(cost_vector)]

  foodVector_final = which(servingSizes>0)

  finalValues = cbind(servingSizes, foodCost); finalValues = finalValues[ finalValues[,1]>0, ]
  finalValues = data.frame(   food_data$Foods[foodVector[foodVector_final]], finalValues, food_data$`Serving Size`[foodVector[foodVector_final]])

  colnames(finalValues) = c("Food", "Number of Servings", "Cost ($)", "Size PER 1 Serving")
  finalTableau = simplexSol$finalTableau

  colnames(finalTableau) = colNames; colnames(simplexSol$iterations) = colNames; colnames(simplexSol$basicSolution) = colNames[-(n-1)]

  lis = list(feasible=1, finalValues = finalValues, initialTableau = tableau, finalTableau = finalTableau,
             basicSolution = simplexSol$basicSolution, optimumCost = simplexSol$Z, iterations = simplexSol$iterations, count = simplexSol$count)

  return(lis)

}




