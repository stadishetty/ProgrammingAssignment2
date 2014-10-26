##  makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
##  cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.


makeCacheMatrix <- function( m = matrix() ) {
  i <- NULL
  ## set method
  set <- function( matrix ) {
    m <<- matrix
    i <<- NULL
  }
  
  ## get method
  get <- function() {
    m
  }
  ## method to set inverse of matrix
  setInverse <- function(inverse) {
    i <<- inverse
  }
  ## method to get inverse of matrix
  getInverse <- function() {
    i
  }
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

cacheSolve <- function(x, ...) {
  ## returns a reverse matrix of x
  m <- x$getInverse()
  ## returns the inverse if it is already existing
  if( !is.null(m) ) {
    message("getting the Cached Data")
    return(m)
  }
  
  data <- x$get()
  ## calculating inverse of a matrix
  m <- solve(data) %*% data
  x$setInverse(m)
  m
}