## Creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  matrixInverse <- NULL
  set <- function(y) {
    x <<- y
    matrixInverse <<- NULL
  }
  ## get the value of the matrix.
  get <- function() x
  ## set the inverse of the matrix.
  setInv <- function(i) matrixInverse <<- i
  getInv <- function() matrixInverse
  ## get the inverse of the matrix.
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}

cacheSolve <- function(x, ...) {
  ## get the inverse of the matrix.
  matrixInverse <- x$getInv()
  ## check if there is the matrix, if yes: print the message.
  if(!is.null(matrixInverse)) {
    print("getting cached data")
    return(matrixInverse)
  }
  ## if not: get the inverse of the matrix.
  data <- x$get()
  matrixInverse <- solve(data, ...)
  ## set the inverse of the matrix.
  x$setInv(matrixInverse)
  matrixInverse
}