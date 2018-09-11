## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## makeChaceMatrix creates a list containing functions to 
## 1. Set inverse of matrix
## 2. Get inverse of matrix
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setInverse <- function(inv) inverse <<- inv
  getInverse <- function() inverse
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function

## cacheSolve returns a matrix that is the inverse of 'x'
## returned by makeCacheMatrix.
## If it has already been calculated, cacheSolve should
## retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  if (!is.null(inverse)) {
    message("getting cached inverse matrix")
    return(inverse)
  }
  data <- x$getMatrix()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}