## Put comments here that give an overall description of what your
## functions do

##  two functions available makeCacheMatrix and cacheSolve

## the MASS package calculates inverse for non-squared and square matrices
library(MASS)
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL     ## initialize inverse as NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x     ## functions to get  matrix x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function(){
    inver <- ginv(x)
    inver%%x        ## functions to obtain inverse of the matrix
    } 
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function

## to get cache data
cacheSolve <- function(x, ...)    ## gets data cache
  {
  inv <- x$getinv()
  if(!is.null(inv)) {            ## checks if inverse is null 
    message("getting cached data")
    return(inv)               ## returns inverse value
  }
  data <- x$get()
  inv <- solve(data, ...)     ## calculates inverse value
  x$setinv(inv)
  inv    ## Return a matrix that is the inverse of 'x'
}

f <- makeCacheMatrix(matrix(1:8,2,4))
f$get()
f$getinv()
cacheSolve(f)          