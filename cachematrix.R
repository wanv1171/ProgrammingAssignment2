## The makeCacheMatrix function will return a list of four functions:
##     1. set the value of the matrix
##     2. get the value of the matrix
##     3. set the value of the inverse of the matrix
##     4. set the value of the inverse of the matrix
## functions do

## This will create a list to the four functions mentioned above
## The two checks in the function will check for invalid matrix inputs

makeCacheMatrix <- function(x = matrix()) {
  if (!is.matrix(x)) {
    message("This is not a valid matrix")
    return()
  }
  
  im <- NULL
  
  set <- function(y) {
    if (!is.matrix(x)) {
        message("This is not a valid matrix")
        return()
    }
    
    x <<- y
    im <<- NULL
  }
  
  get <- function() return(x)
  setInverse <- function(inverseMatrix) im <<- inverseMatrix
  getInverse <- function() return(im)
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This will return the inverse of the matrix 'x'

cacheSolve <- function(x, ...) {
  im <- x$getInverse()
  
  if (!is.null(im)) {
    message("Returning cached inversed matrix")
    return(im)
  }
  matrixData <- x$get()
  im <- solve(matrixData)
  x$setInverse(im)
  return(im)
}
