## My functions makeCacheMatrix and cacheSolve can be used to
## calculate the inverse of a set matrix and cache the result so that
## the computation, which can be costly, does not have to be repeated
## if the inverse of the set matrix is needed more than once during
## a task.

## The following function creates a matrix object whose inverse can 
## be cached. It is essentially a list of functions that are used 
## separately to "set" and retrieve ("get") a matrix, and to set 
## ("setinv") and retrieve ("getinv") the inverse of the set matrix.

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y = matrix()) {
            x <<- y
            inv <<- NULL
      }
      get <- function() x
      setinv <- function(solve) inv <<- solve
      getinv <- function() inv
      list(set = set, get = get, 
           setinv = setinv, 
           getinv = getinv)
}


## The function below calculates the inverse of the matrix object set 
## by the above function. If the inverse of the object has already 
## been calculated, it is instead retrieved from the environment  
## where it was cached.

cacheSolve <- function(x, ...) {
      inv <- x$getinv()
      if(!is.null(inv)) {
            message("getting cached data")
            return (inv)
      }
      data <- x$get()
      inv <- solve(data, ...)
      x$setinv(inv)
      inv
}
