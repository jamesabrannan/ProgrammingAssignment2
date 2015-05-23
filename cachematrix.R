## ==================================================================
## Cache a matrix. If doesn't exist create if exists overwrite.
## ==================================================================
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL;
  set <- function(y)
  {
    x <<- y;
    m <<- NULL;
  }
  ## define get as a function that returns x
  get <- function() x;
  
  ## define setMatrix as a function that sets m to the inverseMatrix
  ## ie. the matrix passed as arguement to function
  setMatrix <- function(inverseMatrix) m <<- inverseMatrix;
  
  ## define getMatrix as funtion that return m
  getMatrix <- function() m;
  
  list(set = set, get = get,
  setMatrix = setMatrix,
  getMatrix = getMatrix);
}
## ================================================================
## Get the inverse of a matrix using the matrix cached using
## makeCacheMatrix
## usage:
## > x <- makeCacheMatrix(matrix(c(1,2,3,4), ncol=2))
## > cacheSolve(x)
## ================================================================
cacheSolve <- function(x = matrix(), ...) {
  m <- x$getMatrix();
  if(!is.null(m))
  {
    message("getting cached data");
    return(m);
  }
  theMatrix <- x$get();
  m <- solve(theMatrix);
  x$setMatrix(m);
}
