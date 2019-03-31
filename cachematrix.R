## These two function computes for an inverse of a square matrix (a square matrix is always invertible)
## The first function caches the matrix, then the second function solves for the inverse of the cached matrix


## For the function to solve for the inverse, I used the inv() function from the matlib package. 
## You can install the matlib package or just change the inv() function under cacheSolve to solve() which does the same thing.

library(matlib)
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function solves for the inverse of the cached matrix

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cache of inverse data")
    return(m)
  }
  data <- x$get()
  m <- inv(data, ...)
  x$setinverse(m)
  m
}
