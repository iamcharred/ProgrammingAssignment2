## This R source code contains two functions
## - makeCacheMatrix creates a special "matrix" object that can cache its inverse.
## - cacheSolve computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then the cachesolve should retrieve the inverse from 
## the cache.

## makeCacheMatrix creates a special "matrix" object that can cache its inverse.
## takes an input of matrix x that is assumed to be always invertible
## returns a list of functions: set, get, setinverse, getinverse

makeCacheMatrix <- function(m = matrix()) {
     i <- NULL  ## set the inverse to NULL
     set <- function(y) { ## the set function set the value of the matrix
          m <<- y
          i <<- NULL
     }
     get <- function() m ## the get function returns the value of the matrix
     setinverse <- function(solve) i <<- solve ## the setinverse function solves 
     ## the inverse of the matrix
     getinverse <- function() i ## the getinverse function returns the inverse
     ## of the matrix
     list(set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)
}


## - cacheSolve computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then the cachesolve should retrieve the inverse from 
## the cache.
## takes an input of
## returns the inverse of a cache-able matrix

cacheSolve <- function(m, ...) {
        ## Return a matrix that is the inverse of 'm'
     i <- m$getinverse()
     if(!is.null(i)) {
          message("getting cached data")
          return(i)
     }
     data <- m$get()
     i <- solve(data, ...)
     m$setinverse(i)
     i
}
