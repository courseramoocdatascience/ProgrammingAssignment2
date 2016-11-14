## In this assignment, we have 2 functions -- makeCacheMatrix and cacheSolve. The first function creates a special "matrix" object that can cache its inverse. 
## The second function uses the defined functions for such a matrix and if inverse hasn't been calculated yet, it calculates the inverse, if not
## returns it from the cache

## This function creates the matrix object and defines how the inverse is calculated

makeCacheMatrix <- function(x = matrix()) {
  #local variable to hold value of inverse
  inverse = NULL
  
  #function to set the inverse value
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  
  #function to get the inverse value
  get <- function(){
    x
  }
  
  #function to set inverse to be the input parameter. Note that inverse variable is global to the function makeCacheMatrix
  setInverse <- function(x){ 
    inverse <<- x
  }
  
  #function to get inverse
  getInverse <- function() {
    inverse
  }
  
  #list
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Computes the inverse of the input matrix and store it. If the inverse has been precalculated, that cached inverse is returned,
## else, the inverse is calculated
cacheSolve <- function(x, ...)
{
  i <- x$getInverse()
  if (!is.null(i)) 
  {
    message("Since inverse is pre-calculated, we return the cached inverse")
    return(i)
  }
  
  # store the input matrix in m using the get function
  m <- x$get()
  
  # if we reach here, then we haven't yet calculated the inverse, so obtain inverse using the solve()
  inv <- solve(m, ...)
  
  # then, set the inverse calculated to cache
  x$setInverse(inv)
  
  #return the inverse just calculated
  inv #or return as x$getInverse(), but we can avoid the extra function call
}




