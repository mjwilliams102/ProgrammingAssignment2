## Functions to compute the inverse of a matrix and cache the result such 
## that the inverted matrix only has to be computed once

## First function makeCacheMatrix makes a special "matrix" that is really
## a list containing a function to set and get the value of the matrix, and 
## to set and get the value of the matrix's inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Second function cacheSolve calculates the inverse of the matrix created
## in the first function, but first checks to see whether it is already 
## stored in cache. If so, it simply returns the cached inverse matrix. If
## not, the inverse matrix is calculated, returned, and cached

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if (!is.null(m)){
    message("Inverse already calculated. Retrieving from cache.")
    return(m)
  } else {
    newMatrix <- x$get()
    m <- solve(newMatrix,...)
    x$setinverse(m)
    m
  }
}
