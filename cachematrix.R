## +-------------------------------------------------------------------+
## |                                                                   |
## |             cachematrix.R - Programming Assignment 2              |
## |                                                                   |
## +-------------------------------------------------------------------+

## -------[makeCacheMatrix] function realization -----------------------
##        Creates a special object, so called "matrix" object,
##        that is really a list and that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(solve) i <<- solve
  getinv <- function() i
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## -------[cacheSolve] function realization ----------------------------
##        Computes the inverse of the special "matrix" object
##        or retrieves the inverse from the cache

cacheSolve <- function(x, ...) {
  i <- x$getinv()
  if(!is.null(i)) {
    message("Inverse matrix already exists! Getting cached data.")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}
