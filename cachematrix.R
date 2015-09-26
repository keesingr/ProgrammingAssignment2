makeCacheMatrix <- function(x = matrix()) {
  ## Creates a list of functions to support cached matrices. Called by 
  ## cacheSolve to either retrieve the cached value in 'cachedInverse'
  ## or compute the inverse if there is no cached value.
  
    cachedinverse <- NULL
 set <- function(y) {
   x <<- y
   cachedinverse <<- NULL
 }
 get <- function() x
 setinverse <- function(solve) cachedinverse <<- solve
  getinverse <- function() cachedinverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
  ## computes the inverse of a square matrix created using makeCacheMatrix,
  ## retrieving the value from a cached variable called 'cachedinverse'
  ## that is dynamically scoped and defined within makeCacheMatrix.
  
  cachedinverse <- x$getinverse()
  if(!is.null(cachedinverse)) {   ## If cachedinverse exists, return it
    return(cachedinverse)
  }
  data <- x$get()                 ## otherwise compute the value of the inverse using solve
  cachedinverse <- solve(data, ...)
  x$setinverse(cachedinverse)     ## and set the value of cachedinverse to the computed inverse
  cachedinverse                   ## and return the resulting matrix
}

