## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {

  mat_inv <- NULL
  set <- function(y) {
    x <<- y
    mat_inv <<- NULL
  }
  get <- function() x
  
  set_invmatrix <- function(inverse) mat_inv <<- inverse
  get_invmatrix <- function() mat_inv
  list(set = set, get = get,
       set_invmatrix = set_invmatrix,
       get_invmatrix = get_invmatrix)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
##If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
       
  mat_inv <- x$get_invmatrix()
  
  if(!is.null(mat_inv))
  {
    return(mat_inv)
  } 
  
    m <- x$get()
    mat_inv <- solve(m, ...)
    x$set_invmatrix(mat_inv)
    mat_inv
  
}
