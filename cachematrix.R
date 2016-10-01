## +---------------------------------------------------------------------------------------------------------+
## |                                                                                                         |
## |                              cachematrix.R - Programming Assignment 2                                   |
## |                                                                                                         |
## +---------------------------------------------------------------------------------------------------------+

## --------------------------------[makeCacheMatrix] function realization ------------------------------------

makeCacheMatrix <- function(x = matrix()) {   ## Creates a special object, so called "matrix" object, that is 
                                              ## really a list and that can cache its inverse
  i <- NULL                                                             ## Inverse matrix reseting
  set <- function(y) {                                                  ## Setting the matrix
    x <<- y
    i <<- NULL
  }
  get <- function() x                                                   ## Getting the matrix
  setinv <- function(solve) i <<- solve                                 ## Setting the inverse matrix
  getinv <- function() i                                                ## Getting the inverse matrix
  list(set = set, get = get, setinv = setinv, getinv = getinv)          ## Returning the list of functions
}

## -------[cacheSolve] function realization ------------------------------------------------------------------

cacheSolve <- function(x, ...) {              ## Computes the inverse of the special "matrix" object
                                              ## or retrieves the inverse from the cache
  i <- x$getinv()                                                       ## Receiving inverse matrix
  if(!is.null(i)) {                                                     ## If it already has been calculated
    message("Inverse matrix already exists! Getting cached data.")
    return(i)                                                           ## Returning result
  }
  data <- x$get()                                                       ## Else if it hasn't
  i <- solve(data, ...)                                                 ## Getting the inverse matrix
  x$setinv(i)                                                           ## Setting the inverse matrix
  i                                                                     ## Returning result
}
