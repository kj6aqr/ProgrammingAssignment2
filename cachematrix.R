#' Makes a matrix with the ability to cache its inverse.
#' 
#' @param x Existing matrix data from which to create a cacheMatrix
#' @return A cacheMatrix list
makeCacheMatrix <- function(x = matrix()) {
  # Default values:
  inverse <- NULL    # use NULL to denote that the inverse has not yet been computed
  
  # Set a new matrix into the cacheMatrix.
  # This will return 'inverse' to NULL as it is no longer valid
  set <- function(newMatrix) {
    x <<- newMatrix
    inverse <<- NULL
  }
  
  # Returns the matrix stored in the cacheMatrix
  get <- function() {
    x
  }
  
  # Set an inverse in the cacheMatrix.  Note that there is no actual validation
  # that this value really is the inverse of the initial matrix.
  setinverse <- function(newInverse) {
    inverse <<- newInverse
  }
  
  # Returns the inverse matrix stored in the cacheMatrix
  getinverse <- function() {
    inverse
  }
  
  # Returns all functions as a list
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

#' Solves the inverse of a cacheMatrix, and caches it by setting it in the
#' cacheMatrix..  If the inverse has been previously been solved, this function
#' will use the cached version instead.
#' 
#' @param x The cacheMatrix to get the inverse of
#' @return The inverse matrix to the input cacheMatrix
cacheSolve <- function(x, ...) {
  # First, get the existing inverse stored in x
  inverse <- x$getinverse()
  
  # If the retrieved value is valid, return it.
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }  
  # Otherwise, get the initial matrix, take its inverse, set x's inverseMatrix
  # to this calculated value, and return this value
  else {  
    data <- x$get()
    inverse <- solve(data, ...)
    x$setinverse(inverse)
    return(inverse)
  }
}
