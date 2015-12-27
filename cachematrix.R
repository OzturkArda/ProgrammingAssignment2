## This package is created for calcutalign and caching 
## the inverse of a given (inversable) square matrix

## This function transforms a regular matrix into an enhanced
## matrix "object" to support caching functionalities

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverseM) inv <<- inverseM
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function calculates inverse of  a "makeCacheMatrix" type matrix 
## If "makeCacheMatrix"s inverse have already calculated it will directly 
## return it without any calculation


cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}
