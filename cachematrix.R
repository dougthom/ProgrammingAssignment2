## These functions help cache time-consuming computations. The first function creates
## the special cached object. The second function is for getting/calculating the inverse of the matrix.

## makeCacheMatrix creates a special matrix with get/set functions for the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) 
{
  inv <- NULL
  
  # set and get function for the matrix
  set <- function(y)
  {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  
  # set and get function for the matrix inverse
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  
  # return a list containing the set/get functions
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function returns the inverse of special matrix x. It checks to see if the 
## inverse has already been calculated first.

cacheSolve <- function(x, ...) 
{
  inv <- x$getInverse()
  
  # check if inverse has been calculated already 
  if(!is.null(inv))
  {
    # if so, return the cached matrix inverse
    message("getting cached data")
    return(inv)
  }
  # get the matrix
  data <- x$get()
  
  # compute the matrix inverse and cache it
  inv <- solve(data,...)
  x$setInverse(inv)
  
  # return the matrix inverse
  inv
}
