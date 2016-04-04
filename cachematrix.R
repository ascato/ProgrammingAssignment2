## Matrix inversion is usually a costly computation
## and there may be some benefit to caching the inverse of a matrix rather than compute it repeatedly
 
##makeCacheMatrix: This function creates a special “matrix” object that can cache its inverse.

 
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
  x <<- y
  inv <<- NULL
}

get <- function() x
	setmatrix <- function(solve) inv <<- solve
	getmatrix <- function() inv
	list(set = set, get = get,
		setmatrix = setmatrix,
		getmatrix = getmatrix)
}

##This function computes the inverse of the special “matrix” returned by makeCacheMatrix above.
##If the inverse has already been calculated (and the matrix has not changed), then the cacheSolve should retrieve the inverse from the cache

cacheSolve <- function(x = matrix(), ...) {
    inv <- x$getmatrix()
    if(!is.null(inv)){
      message("getting cached data")
      return(inv)
    }
    matrix <- x$get()
    inv <- solve(matrix, ...)
    x$setmatrix(inv)
    inv
}
