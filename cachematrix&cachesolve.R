makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  # Set a new matrix and reset cached inverse
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # Get the matrix
  get <- function() x
  
  # Set the cached inverse
  setinverse <- function(inverse) inv <<- inverse
  
  # Get the cached inverse
  getinverse <- function() inv
  
  # Return a list of all methods
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  
  # If inverse is already cached, return it
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # Otherwise, compute the inverse
  mat <- x$get()
  inv <- solve(mat, ...)
  
  # Cache the inverse
  x$setinverse(inv)
  inv
}