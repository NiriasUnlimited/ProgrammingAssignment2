## This code aims at using cache to avoid potentially time-consuming computations and store in memory
## result of computation that has already been done.

## This code is written based on the model given for mean computation using cache.

## makeCacheMatrix function creates "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  ## make matrix assign to variable x, and initialize the inverse to NULL.
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() {
    x
  }
  setinverse <- function(m) {
    inverse <<- m
  }
  getinverse <- function() {
    inverse
  }
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

###########################

## cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix 
## function above.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}

###########################

## Tests

## Now we can test with a randomized matrix (a diagonal matrix will always be invertible)

r <- rnorm(10)
testMatrix <- diag(r, nrow=200, ncol=200)
cachedMatrix <- makeCacheMatrix(testMatrix)

## Not yet cached
time <- Sys.time()
s <- cacheSolve(cachedMatrix)
time <- Sys.time() - time
time

## Getting cache data
time <- Sys.time()
s <- cacheSolve(cachedMatrix)
time <- Sys.time() - time
time <- Sys.time() - time
time


## When the matrix is cached, is it way faster to return the result as
## no computation has to be done again.

