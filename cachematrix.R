## In order to run this code please install matrixcalc
## install.packages('matrixcalc')

## Create a cache matrix
library('matrixcalc')

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <-function() x
  
  setMatrix <- function(matrix) m <<- matrix
  
  getMatrix <- function() m
  
  list(set = set, get = get, setMatrix = setMatrix, getMatrix = getMatrix)
}

## This function returns a inverse of a matrix if its possible. Case not 
## possible returns the message "It's not possible to invert a singular matrix"

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  data <- x$get()
  
  if(!is.singular.matrix(data)) {
    inverse <- x$getMatrix()
    if(!is.null(inverse)) {
      message("getting cached data")
      return(inverse)
    }
    
    inverse <- solve(data, ...)
    x$setMatrix(inverse)
    inverse
  } else {
    message("It's not possible to invert a singular matrix")
  }
}

