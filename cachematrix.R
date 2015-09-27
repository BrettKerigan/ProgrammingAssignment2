# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly. The
# following two functions are used to cache the inverse of a matrix.

## The function makeCacheMatrix creates a list which contains a function to do a number of this:
## 1- Set the value of the matrix
## 2- Get the value of the matrix
## 3- Set the value of the inverse of the matrix
## 4- get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


# The cacheSolve function returns the inverse of the matrix from makeCacheMatrix. First, it checks if
# there is already a computed inverse.If there is already a computed inverse, it will skip the computation
# and retrieve the value of the inverse from the cache. If not, it computes the inverse and sets the value to
# the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
