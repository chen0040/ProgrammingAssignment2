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

## Sample code for checking correcting of the methods

verifyCacheSolve <- function() {
  set.seed(31)
  x <- matrix(rnorm(16, mean=0, sd=1), nrow=4, ncol=4)
  print(x)
  m <- makeCacheMatrix(x)
  im <- cacheSolve(m)
  print(im)
  im2 <- cacheSolve(m)
  print(im2)
  dot_product <- x %*% im
  print(dot_product)
  result <- round(dot_product)
  print(result)
  if(identical(result, diag(4))) {
    print('correctly return identity matrix when dot product')
  } else {
    print('wrong! dot product result should be a identity matrix')
  }
  if(identical(im, im2)) {
    print('same inverse matrix returned')
  } else {
    print('this should not happen')
  }
}

## test run in console
# > source('cachematrix.R')
# > verifyCacheSolve()
# [,1]       [,2]       [,3]       [,4]
# [1,]  0.05557024  1.5062669 -1.2744714  0.1713988
# [2,] -0.18423859 -0.4447236 -0.7308096 -1.0471108
# [3,]  1.59576183  0.3903673 -1.0689675 -0.9904124
# [4,]  0.96483592  0.9191971 -0.3362180 -0.9175380
# [,1]       [,2]       [,3]       [,4]
# [1,] -0.2232754 -0.5252438  0.6811167 -0.1775044
# [2,]  0.2903755 -0.1517368 -0.4753844  0.7405490
# [3,] -0.4228039 -0.2830118 -0.4763955  0.7582298
# [4,]  0.2110457 -0.6006252  0.4145513 -0.8124814
# getting cached data
# [,1]       [,2]       [,3]       [,4]
# [1,] -0.2232754 -0.5252438  0.6811167 -0.1775044
# [2,]  0.2903755 -0.1517368 -0.4753844  0.7405490
# [3,] -0.4228039 -0.2830118 -0.4763955  0.7582298
# [4,]  0.2110457 -0.6006252  0.4145513 -0.8124814
# [,1]         [,2]         [,3]          [,4]
# [1,] 1.000000e+00 4.163336e-17 1.387779e-16 -1.665335e-16
# [2,] 5.551115e-17 1.000000e+00 5.551115e-17  0.000000e+00
# [3,] 0.000000e+00 1.110223e-16 1.000000e+00  0.000000e+00
# [4,] 2.775558e-17 2.220446e-16 2.220446e-16  1.000000e+00
# [,1] [,2] [,3] [,4]
# [1,]    1    0    0    0
# [2,]    0    1    0    0
# [3,]    0    0    1    0
# [4,]    0    0    0    1
# [1] "correctly return identity matrix when dot product"
# [1] "same inverse matrix returned"
