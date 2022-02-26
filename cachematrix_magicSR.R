## Assignment: Caching the Inverse of a Matrix
## This assignment is to write a pair of functions that cache the inverse of a matrix.


# This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  mtx <- NULL
  set <- function(y) {     # set a matrix to x 
    x <<- y
    mtx <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse  # for getting inverse of matrix, use "inverse"
  getinverse  <- function() mtx
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  mtx <- x$getinverse()
  if(!is.null(mtx)) {                  # This is for NULL
    message("getting cached data")
    return(mtx)
  }
  data <- x$get()
  mtx <- solve(data, ...)  #returns its inverse
  x$setinverse(mtx)
  mtx
}