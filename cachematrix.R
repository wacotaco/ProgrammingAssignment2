## Assignment: Caching the Inverse of a Matrix


## A pair of functions that cache the inverse of a matrix.


## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  #inv = NULL
  inv <- NULL
  set = function(y) {                                                       ## set the matrix
    x <<- y
    inv <<- NULL
  }
  get = function() x                                                        ## get the matrix
  setinverse = function(solve) inv <<- solve                                ## set the inverse
  getinverse = function() inv                                               ## get the inverse
  list(set=set, get=get,                                                    ## list for cacheSolve()
       setinverse=setinverse,
       getinverse=getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If inverse has already been calculated (and matrix has not changed), then cachesolve will retrieve inverse from cache.
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if (!is.null(inv)){                                      ## if the inverse has already been calculated
    message("getting cached data")              
    return(inv)                                            ## get it from the cache  
  }
  
  mat.data = x$get()                                       ## otherwise, calculates the inverse 
  inv = solve(mat.data, ...)
  
  x$setinverse(inv)                                        ## set value of inverse matrix in the cache via the setinv function.
  
  return(inv)
}



