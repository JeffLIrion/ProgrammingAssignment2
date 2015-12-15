## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  ## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
  xInv <- NULL
#   set <- function(y) {
#    x <<- y
#    xInv <<- NULL
#   }
#   print(x)
#   print(xInv)

  getx <- function() x
  setxInv <- function(solve) xInv <<- solve
  getxInv <- function() xInv
  #list(set=set, getx=getx, setxInv=setxInv, getxInv=getxInv)
  list(getx=getx, setxInv=setxInv, getxInv=getxInv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  # cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
  xInv <- x$getxInv()
  
  # retrieve the inverse from the cache
  if (!is.null(xInv)){
    message("getting cached data")
    return(xInv)
  }
  
  # otherwise, compute the inverse
  xInv <- solve(x$getx(), ...)
  x$setxInv(xInv, ...)
  xInv
}
