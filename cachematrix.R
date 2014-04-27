## The "cacheMatrix.R" will calculate the inverse of a matrix
## for the first time the user use the function and the inverse
## is cached in the 'm' and the next time the user can get the 
## inverse without calculation. This help save time and computation
## resources

## Function 'makeCacheMatrix' returns a list containing four 
## functions: set, get, getinverse and setinverse.

makeCacheMatrix <- function(x = matrix()) {

  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Function 'cacheSolve' will check whether the inverse of a 
## matrix has been calculate, if yes, it will return the cached
## result directly without calculation, or else it will calculate
## the inverse.


cacheSolve <- function(x = matrix) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached inverse")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m
}
