## These two functions together demonstrate how to cache a value and then
## check for its existance in another function.

## This function creates a special "matrix" object that can cache the value 
## of a matrix for later retrieval. 

makeCacheMatrix <- function(x = matrix()) {
     m <- NULL
     set <- function(y) {
          x <<- y
          m <<- NULL
     }
     get <- function() x
     setinv <- function(inv) m <<- inv
     getinv <- function() m
     list(set = set, get = get,
          setinv = setinv,
          getinv = getinv)         
}


## This function computes the inverse of a matrix after checking to see if it 
## is in cache.

cacheSolve <- function(x, ...) {
     ## Check cache for x
     m <- x$getinv()
     if(!is.null(m)) {
          message("getting cached data")
          return(m)
     }
     ## Return a matrix that is the inverse of x
     data <- x$get()
     m <- solve(data, ...)
     x$setinv(m)
     m          
}