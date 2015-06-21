## Cahce the matrix and compute the inverse of a matrix
## if the inverse of a matrix is cheched it is returned else its computed

## The makeCacheMAtrix caches the matrix for repeted invocations

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get, setinv = setinv,getinv = getinv)
}


## This function returns the inverse of a matrix from the cached value, if it exists
## else, computes the inverse of a singular matrix using the solve function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinv(m)
  m
}
