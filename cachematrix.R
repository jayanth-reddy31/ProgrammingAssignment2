## These functions create a special matrix object that can cache its inverse.
## This avoids repeated costly computations of the matrix inverse.

## makeCacheMatrix creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # initialize inverse as NULL
  
  # Set the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL  # reset inverse cache when matrix is changed
  }
  
  # Get the matrix
  get <- function() x
  
  # Set the inverse
  setInverse <- function(inverse) inv <<- inverse
  
  # Get the inverse
  getInverse <- function() inv
  
  # Return a list of all the above functions
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated and cached, it retrieves it from the cache.
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  
  # If inverse already exists in cache, return it
  if (!is.null(inv)) {
    message("getting cached inverse")
    return(inv)
  }
  
  # If inverse not cached, compute it
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)  # Cache the inverse
  inv
}
# Create a matrix
mat <- matrix(c(2, 1, 1, 2), nrow = 2, ncol = 2)

# Create the special matrix object
cachedMat <- makeCacheMatrix(mat)

# Compute and cache the inverse
inv1 <- cacheSolve(cachedMat)

# Retrieve the cached inverse (should show the message)
inv2 <- cacheSolve(cachedMat)

