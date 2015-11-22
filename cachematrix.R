## Matrix inversion is usually a costly computation.
## There may be some benefit to caching the inverse of
## a matrix rather than compute it repeatedly.


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


cacheSolve <- function(x, ...) {
  
  inv <- x$getinverse()
 
   if(!is.null(inv)) {
    return(inv)
  }
  
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}