# The function creates a special object that stores the matrix and cahce of its inverse. 
# The argument of the function is a squared (i.e. the number of rows has to be equal to the number of columns) matrix.
#
# Samples of usage:
#
# 1. Initialize the object by an empty matrix:
# t <- makeCacheMatrix()
# 2. Initialize the object by a non-square matrix:
# t <- makeCacheMatrix(matrix(c(0, 1, 1, 1), nrow = 1, ncol = 4)) # Raises an error: "The number of columns of the matrix must be the same as the number of rows!". t is still null.
# 3. Initialize the object by a proper matrix:
# t <- makeCacheMatrix(matrix(c(0, 1, 1, 1), nrow = 2, ncol = 2))
# 4. Set a new matrix value to the object:
# t$set(matrix(c(1, 1, 1, 0), nrow = 2, ncol = 2))
# 5. Get the previously saved matrix:
# t$get()
# 6. Get the cached inversed matrix
# t$getInverse()
# 7. Cache a new inversed matrix:
# t$setInverse(matrix(c(1, 1, 1, 1), nrow = 2, ncol = 2))

makeCacheMatrix <- function(x = matrix()) {
  # Check if the matrix has equal number of rows and columns (if not, then the matrix can't be solved). 
  if (dim(x)[1] != dim(x)[2]) {
    stop ("The number of columns of the matrix must be the same as the number of rows!")
  }
  
  inverseMatrixValue <- NULL
  
  set <- function (matrixValue = matrix()) {
    if (dim(matrixValue)[1] != dim(matrixValue)[2]) {
      stop ("The number of columns of the matrix must be the same as the number of rows!")
    }
    
    x <<- matrixValue # Addressing variables outside the default environment...
    inverseMatrixValue <<- NULL
  }
  
  get <- function() x
  setInverse <- function(inverseMatrix) inverseMatrixValue <<- inverseMatrix
  getInverse <- function() inverseMatrixValue
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


# Returns the inverse matrix for the matrix stored in the object. If the inverse has already been calculated, then the returns the inverse from the cache without calculation.
# In the opposite case, calculates the inverse matrix and stores it in the cache.
#
# Samples of usage:
# 
# t <- makeCacheMatrix(matrix(sample(c(0.1, 0.05, 0, -0.05, -0.1), 1000000, TRUE), 1000, 1000)) # Create a new object.
# inv1 <- cacheSolve(t) # Calculate and return the inverse matrix.
# inv2 <- cacheSolve(t) # Return the cached value, avoiding calculations. 
# identical(inv1, inv2) # Results are identical.
#
# If the initial matrix is changed...
# t$set(matrix(sample(c(0.1, 0.05, 0, -0.05, -0.1), 1000000, TRUE), 1000, 1000))
# inv3 <- cacheSolve(t) # The function doesn't use the cache, because the matrix was changed and the results is different:
# identical(inv2, inv3) # Returns FALSE, as expected.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  s <- x$getInverse()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  
  tryCatch(
    {s <- solve(data, ...)}, 
    error = function (e) 
      {stop (paste("Can't inverse the matrix. Error message is: ", e))}
    )
  
  x$setInverse(s)
  s
}

