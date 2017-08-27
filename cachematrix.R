## Put comments here that give an overall description of what your
## functions do
## use makeCacheMatrix to store a matrix and calculate a inverse of it
## use set to add the matrix value
## use setsolve to get the solve value and store it in cache
## then, use cachesolve to restore the value. 

## Write a short comment describing this function
##create a function that create an object that store the inverse matrix and use it in future 
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <-  function() m <<- solve(x)
  getsolve <- function() m 
  list( get = get, set = set, 
        setsolve = setsolve,
        getsolve = getsolve)
}


## Write a short comment describing this function
##function to use apply the makeCacheMatrix if solve exist in cache or calculate again
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if (!is.null(m)){
    message("getting the cached inverse matrix")
    return (m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}



