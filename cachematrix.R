## Reduce computation by caching the inverse of a matrix.
##
## Assume the inverse of a matrix is used in many places in the program.
## We will create a special list which stores the value of the inverse in
## a cache variable. Therefore the first time we call cacheSolve() to compute
## the inverse it will take O(n^3) time and each subsequent call for the
## same matrix will take O(1) time. 
##
## To use these functions, call ml<-makeCacheMatrix(m) once for the matrix m,
## then call cacheSolve(ml) to calculate/retrieve the inverse. Note: if m 
## changes, you must use the helper function set() to change the value of ml!

## makeCacheMatrix
## Initializes a list which stores the helper functions for accessing the 
## cache variable. The helper functions are
##    set: change the input matrix and re-initialize the cache
##    get: retrieve the input matrix
##    setinv: store the inverse matrix in the cache variable
##    getinv: retrieve the inverse matrix
## The list this function forms should be used with the cacheSolve() 
## function below.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
         x <<- y
         inv <<- NULL
  }
  get <- function() x
  setinv <- function(new_inv) inv <<- new_inv
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve
## Find the inverse of the matrix stored in the list x, where 
## x was formed using the function above makeCacheMatrix(). 
## If cacheSolve has already been called on x, the inverse is
## retrieved from a cache variable. If it hasn't been called,
## the inverse is computed and stored in the cache. 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if (!is.null(inv)) {
         ## Retrieve cached value
         message("getting inverse from cache")
         return(inv)
  }
  ## Cached value is still default, so compute inverse using solve
  ## and store value in cache
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
