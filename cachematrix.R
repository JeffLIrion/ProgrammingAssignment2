## makeCacheMatrix stores a matrix in such a way that we only need to compute its inverse
## once, after which we can simply retrieve it from memory.  
##
## cacheSolve returns the inverse of a "matrix" from makeCacheMatrix, either by computing
## it or by retrieving it from memory if it has already been computed.  
## functions do



makeCacheMatrix <- function(x = matrix()) {
  ## Create a special "matrix" which is really a list of 3 functions:
  ##   1) getx -- return the actual matrix
  ##   2) setxInv -- cache the inverse matrix
  ##   3) getxInv -- return the inverse matrix
  
  # initialize the inverse of the matrix as NULL
  xInv <- NULL
  
  # define the 3 functions
  getx <- function() x
  setxInv <- function(solve) xInv <<- solve
  getxInv <- function() xInv
  
  # return getx, setxInv, and getxInv
  list(getx=getx, setxInv=setxInv, getxInv=getxInv)
}



cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'.  If the inverse has previously been
  ## computed and cached, retrieve the inverse from the cache.  Otherwise, compute
  ## the inverse via "solve"
  
  # try to retrieve the inverse from the cache
  xInv <- x$getxInv()
  
  # if the inverse has already been computed, simply return it
  if (!is.null(xInv)){
    message("getting cached data")
    return(xInv)
  }
  
  # otherwise, compute the inverse
  xInv <- solve(x$getx(), ...)
  x$setxInv(xInv, ...)
  xInv
}
