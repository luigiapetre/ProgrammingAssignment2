## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## The function below returns an object that can set a matrix and its inverse into the cache as well as get the matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  setmatrix <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  getmatrix <- function() x
  setinverse <- function(solve) inverse <<- solve(x)
  getinverse <- function() inverse
  list(setmatrix = setmatrix, getmatrix = getmatrix,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
## The function below returns the inverse of x, first checking to see whether this has been previously cached
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$getmatrix()
  inverse <- solve(data)
  x$setinverse(inverse)
  inverse
}
