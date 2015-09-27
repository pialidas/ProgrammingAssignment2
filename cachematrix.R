## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv_mat <- NULL
  set <- function(y) {
    x <<- y
    inv_mat <<- NULL
  }
  get <- function() x
  setInverted<- function(inverse) inv_mat <<-inverse
  getInverted <- function() inv_mat
  list(set = set, get = get,
       setInverted = setInverted,
       getInverted = getInverted)
}


## Write a short comment describing this function
## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already 
## been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ## Try to get cached inverse matrix
  inv_x <- x$getInverted()
  if (!is.null(inv_x)) {
    message("Found cached inverted matrix")
  } 
  else{
    # Didn't find the inverse matrix, need to compute  //
    inv_x <- solve(x$get())
    # cache the inverted matrix
    x$setInverted(inv_x)
  }
  return(inv_x)
}
