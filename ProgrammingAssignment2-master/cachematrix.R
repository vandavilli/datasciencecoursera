## Put comments here that give an overall description of what your
## functions do

## This function creates a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
          inv <- NULL
          set <- function(y) {
              x <<- y
              inv <<- NULL
          }
          get <- function() x
          setmatinv <- function(inverse) inv <<- inverse
          getmatinv <- function() inv
          list(set = set, 
               get = get, 
               setmatinv = setmatinv, 
               getmatinv = getmatinv)
}


## This function computes the inverse of the matrix that was created in the makeCacheMatrix
## If the inverse is already calculated the cacheSolve returns the inverse from the cache.

cacheSolve <- function(x, ...) {
        inv <- x$getmatinv()
        if (!is.null(inv)) {
              message("getting the matrix inverse from cached data")
              return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setmatinv(inv)
        inv
}