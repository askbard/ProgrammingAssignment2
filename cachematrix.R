# This script edited by @askbard ##finalVersion

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
        x <<- y
        m <<- NULL
      }
      get <- function() x
      setInverse <- function(inverse) m <<- inverse
      getInverse <- function() m
      list(set = set, get = get,setInverse = setInverse,getInverse = getInverse)
}

# The cacheSolve function returns the inverse of the matrix. Initially, it checks if
# the inverse has already been computed. If yes, it gets the result and skips the
# computation. Otherwise, it computes the inverse, sets the value in the cache via
# setinverse function.

cacheSolve <- function(x, ...) {
      m <- x$getInverse()
      if(!is.null(m)) {
        message("getting cached data")
        return(m)
      }
      data <- x$get()
      m <- solve(data)
      x$setInverse(m)
      m
}
