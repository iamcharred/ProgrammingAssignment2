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
     
     ## the set function set the value of the matrix
     set <- function(y) { 
          m <<- y
          i <<- NULL
     }
     
     ## the get function returns the value of the matrix
     get <- function() m 
     
     ## the setinverse function solves the inverse of the matrix
     setinverse <- function(solve) i <<- solve  
     
     ## the getinverse function returns the inverse of the matrix
     getinverse <- function() i 
     
     list(set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse) ## return the list of functions
}


## - cacheSolve computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then the cachesolve should retrieve the inverse from 
## the cache.
## takes an input of
## returns the inverse of a cache-able matrix

cacheSolve <- function(m, ...) {
        
     i <- m$getinverse() ## try to see if m has cached inverse
     if(!is.null(i)) { ## if inverse i exists return i and exit cacheSolve fun
          message("getting cached data")
          return(i)
     }
     data <- m$get() ## if inverse i doesn't exist, store matrix m to data
     i <- solve(data, ...) ## compute inverse of data to i via solve function
     m$setinverse(i) ## store the inverse i of data to m, will use the <<- operator
     i ## return the inverse of 'm'
}
