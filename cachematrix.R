## Two functions that together caches the inverse of a matrix in a list.
## The first function creates a list to hold the original matrix and its inverse
## while the second function returns checks if the inverse is already computed, 
## and if so, returns it, otherwise it computes it, caches it, and returns it.

## This function takes a matrix as an input and returns a list of four functions
##    get: returns the original matrix
##    set: changes the original matrix and removes any cached inverse
##    getinverse: returns the inverse if a cahced version exists
##    setinverse: sets the inverse
## the last two functions should not be used outside the cacheSolve function, 
## for getting the inverse cacheSolve is the prefered method.
makeCacheMatrix <- function(x = matrix()) { 
  # some simple checks that an inverse exists for the matrix.
  if (!dim(x)[1]==dim(x)[2]) {
    stop("'x' must be a square matrix")
  }
  if(!det(x)){
    stop("'x' is singular and does not have an inverse to cache")
  }
  inv <- NULL
  set <- function(y) {
    # some simple checks that an inverse exists for the matrix.
    if (!dim(y)[1]==dim(y)[2]) {
      stop("'x' must be a square matrix")
    }
    if(!det(y)){
      stop("'x' is singular and does not have an inverse to cache")
    }
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get, 
       setinverse = setinverse, getinverse = getinverse)
}

## Takes the return of the makeCacheMatrix as input and return the inverse 
## of the matrix. If the a cached version exists it returns it otherwise it
## calculates the inverse and stores for future uses.
cacheSolve <- function(x, ..., messOn = TRUE) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)){
    if (messOn) message('Getting cached data')
    inv
  } else {
    if (messOn) message('Calculating inverse')
    inv <- solve(x$get(),...)
    x$setinverse(inv)
    inv
  }
}
