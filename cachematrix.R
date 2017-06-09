makeCacheMatrix <- function(x = matrix()) {
  original_matrix <- x
  inverted_matrix <- NULL
## this create list, which will hold original matrix
## and inverted matrix as matrix which makes it
## easier for check
  list(original_matrix = original_matrix,
       inverted_matrix = inverted_matrix)
}

cacheSolve <- function(x) {
## if x$inverted_matrix hold cached inverted matrix,
## then it will simply return cached data.
  if(!is.null(x$inverted_matrix)) {
    message("getting cached data")
    return(x$inverted_matrix)
  }
## if x$inverted_matrix is NULL, it will calculating
## inverted_matrix, and cache it to list of x.
  mat <- x$original_matrix
  solved_mat <- solve(mat)
  x$inverted_matrix <<- solved_mat
  solved_mat
}
