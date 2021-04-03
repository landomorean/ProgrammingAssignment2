## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## makeCacheMatrix creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  # creates set of functions to manage matrices and their inverses
  im <- NULL # im stands for inverse matrix - set to NULL
  setm <- function(y) {
    x <<- y
    im <<- NULL
  }
  getm <- function() x # get the matrix from the environment
  setim <- function(inverse) im <<- inverse
  getim <- function() im
  list(setm = setm, getm = getm,
       setim = setim,
       getim = getim)
}


## Write a short comment describing this function
## cacheSolve computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed),  then the cachesolve should retrieve
## the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  im <- x$getim() #get inverse from parent environment
  if(!is.null(im)) {   #if not null, extract the inverse from cache
    message("getting cached data")
    return(im) # return the inverse
  }
  datam <- x$getm() #if it is not in the cache, then calculate the inverse
  im <- solve(datam, ...)
  x$setim(im) # save inverse in the cache
  im # return the inverse
}
