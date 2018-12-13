## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  ## this will set the value of the matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  ## this will get the value of the matrix
  get <- function() x
  
  ## this is to set the value of the inverted matrix
  setsolve <- function(solve) m <<- solve
  
  ## this is to get the value of the inverted matrix
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve, 
       getsolve = getsolve)
  
}


## Write a short comment describing this function

## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix 
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  
  ## If the inverse has already been calculated (and not changed)
  ## retrieve the inverse from the cache
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  
  ## assign the value of matrix to data
  data <- x$get()
  
  ## assign the value of inverted matrix to m
  m <- solve(data, ...)
  
  
  x$setsolve(m)
  m
}
