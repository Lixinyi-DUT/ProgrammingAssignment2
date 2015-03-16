

##The function 'makeCacheMatrix' creates a special list of methods to set/get (inverse) matrix.

makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
  setmatrix <- function(y) 
  {
    x <<- y
    s <<- NULL
    ## when the matrix changes, the cached solve should be aborted.
  }
  getmatrix <- function() x
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  list(setmatrix = setmatrix, getmatrix = getmatrix,
       setsolve = setsolve,
       getsolve = getsolve)

}


## The function 'cacheSolve' calculates the inverse matrix when there is no cached solve,
## and directly returns the cached solve when the solve has been alreadly cached.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  s <- x$getsolve()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$getmatrix()
  s <- solve(data, ...)
  x$setsolve(s)
  s
}
