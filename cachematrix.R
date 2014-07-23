## This functions are used to inverse Matrix using cache if possible


## this function returns a list of 4 functions. The returned list implements the cache Matrix.

makeCacheMatrix <- function(x = matrix()) {
  ## variable for caching the inverse
  inv <- NULL
  ## set allows to give a value to the initial matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  ## get returns the initial matrix
  get <- function() x
  ## setinv set a value to the cache matrix
  setinv <- function(inverse) inv <<- inverse
  ## getinv returns the inverse matrix stored in the cache
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cachesolve inverse a matrix using the cache if possible

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  ## if the inverse is already stored in the cache, return this value
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  ## if not, calculate the inverse and store it to the cache
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
