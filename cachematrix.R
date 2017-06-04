makeCacheMatrix <- function(x = matrix()) {
  invrs <- NULL
  set <- function(y) {
    x <<- y
    invrs <<- NULL
  }
  get <- function() x
  set_inverse <- function(inverse) invrs <<- inverse
  get_inverse <- function() invrs
  list(set = set, get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}

cacheSolve <- function(x, ...) {
  invrs <- x$get_inverse()
  if(!is.null(invrs)) {
    message("getting cached data")
    return(invrs)
  }
  mat <- x$get()
  invrs <- solve(mat, ...)
  x$set_inverse(invrs)
  invrs
}