## This file contains two functions that allow compute and cache of a matrix's inverse
## * makeCacheMatrix is a factory method return a inverse cacheable matrix
## * cacheSolve return a cache of matrix inverse if exists; or calculate inverse and cache

## factory method which returns an instance of cacheable object (in the form of a list)

makeCacheMatrix <- function(x = matrix()) {
  im <- NULL
  set <- function(y) {
    x <<- y
    im <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) im <<- (inverse)
  getInverse <- function() im
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Method which retrieve a cache copy or calculates and cache the inverse of a matrix if cache of the inverse not available yet 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  im <- x$getInverse()
  if(!is.null(im)) {
    message("getting cached data")
    return(im)
  }
  data <- x$get()
  im <- solve(data, ...)
  x$setInverse(im)
  im
}
