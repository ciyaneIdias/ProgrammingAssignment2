## Matrix inversion is usually a costly computation.
## There may be some benefit to caching the inverse of
## a matrix rather than compute it repeatedly.



## The following function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  cacheInv <- NULL 
  
  set <- function(y) {
    x <<- y
    cacheInv <<- NULL
  }
  
  get <- function() x
  
  setMatrix <- function(inverse) cacheInv <<- inverse
  
  getInverse <- function() cacheInv
  
  list(set = set, get = get, 
       setMatrix = setMatrix, 
       getInverse = getInverse)
}

## The following function computes the inverse of the special "matrix" created by 
## the function makeCacheMatrix. 

cacheSolve <- function(x, ...) {
  
  inv <- x$getInverse()
 
   if(!is.null(inv)) {
    	return(inv)
  }
  
  data <- x$get()
  inv <- solve(data, ...)
  x$setMatrix(inv)
  inv
}

