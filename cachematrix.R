## makeCacheMatrix will create a matrix and cache its inverse

makeCacheMatrix <- function(x = numeric()) {
  mat <- matrix(x, sqrt(length(x)), sqrt(length(x)))
  m <- NULL
  set <- function(y) {
    mat <<- matrix(y, sqrt(length(y)), sqrt(length(y)))
    m <<- NULL
  }
  get <- function() mat
  setInverse <- function(inverse) m <<- inverse
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}


## cacheSolve will compute the inverse of the matrix created by
## makeCacheMatrix; otherwise, it will retrieve the already 
## calculated value from the cache

cacheSolve <- function(x, ...) {
    m <- x$getInverse()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInverse(m)
    m
  }
