## Created by Zach Horrall 12/25/2015 for R Programming Assignment 2

## This script creates two functions: makeCacheMatrix and cacheSolve.
## These two functions combine to allow R to calculate the inverse of a given matrix
## and cache the result for later retrieval. 

## makeCacheMatrix sets the value of the matrix,
## allows cacheSolve to get the value of the matrix, set the value of the inverse,
## and allows cacheSolve to get the value of the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve will return the inverse of the matrix stored with makeCacheMatrix.
## If the inverse has been calculated previously, then the cached value will be returned
## with the message 'getting cached data', otherwise the inverse will be calculated and cached.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  ## If the inverse is cached, return cached value
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  ## If the inverse is not cached, calculate inverse, cache, and return result
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}