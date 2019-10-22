
## makeCacheMatrix creates a matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function(){
    x 
  } 
  setinv <- function(inverse) {
    inv <<- inverse 
  }
  getinv <- function() {
    inv 
  }
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}


# cacheSolve computes the inverse of the matrix returned by 
# makeCacheMatrix function above. If the inverse has already
# been calculated and the matrix is same, then the cachesolve
# should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}