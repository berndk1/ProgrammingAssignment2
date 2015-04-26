## Cache the inverse of a matrix in order to avoid potentially time-consuming recomputations.
## (Assumption: The matrix supplied is always invertible.)

## Constructor for the data structure of a matrix together with its cached inverse.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Invert a given matrix. Compute inverse only if it's not already cached. 
## Otherwise use the cached inverse.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  matr <- x$get()
  i <- solve(matr, ...)
  x$setinverse(i)
  i
}
