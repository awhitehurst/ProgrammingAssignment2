## 
## makeCacheMatrix
##
## returns a set of functions that allow the manipulation
## of a matrix and its inverse. These functions work together
## to provide caching of the computed inverse, in that once
## the matrix has been set and its inverse solved, subsequent
## calls to getInverse or cacheSolve will return the previously
## computed inverse rather than recomputing it.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(m2){m<<-m2;inv<<-NULL}
  get <- function() m
  setInverse <- function(inv2) inv <<- inv2
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Given a cacheMatrix of the type returned by "makeCacheMatrix",
## cacheSolve computes the inverse of the matrix. This must be
## called once after the initial creation of the cacheMatrix, and
## once after using the "set" function. Subsequent calls to cacheSolve
## will return the cached solution, rather than recomputing it.

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)) {
    return(inv)
  }
  inv <- solve(x$get(), ...)
  x$setInverse(inv)
  inv
}
