##These functions use R's lexical scoping
##Since matrix inversing is a time-demanding operation,
##We'll use caching for this operation

## This function cache's the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setinv <- function(solve) m <<- inv
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
  }
}


##This function computes the inverse of the matrix
##If the inverse has been calculates, it retrieves the value

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached inverse data")
    return(inv)
  }
  data <- x$get()
  inv <- makeCacheMatrix(data, ...)
  x$setinv(inv)
  inv
}
