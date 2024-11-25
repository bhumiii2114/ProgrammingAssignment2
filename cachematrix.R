## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # Variable to store the cached inverse
  
  set <- function(y) {
    x <<- y
    inv <<- NULL  # Reset the cached inverse when matrix is updated
  }
  
  get <- function() x  # Return the matrix
  
  setInverse <- function(inverse) inv <<- inverse  # Cache the inverse
  
  getInverse <- function() inv  # Return the cached inverse
  
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)  # Return the list of methods
}

# This function computes the inverse of the "matrix" returned by makeCacheMatrix
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()  # Check if the inverse is already cached
  
  if (!is.null(inv)) {
    message("Getting cached inverse")
    return(inv)  # Return the cached inverse
  }
  
  mat <- x$get()  # Get the matrix
  inv <- solve(mat, ...)  # Compute the inverse
  x$setInverse(inv)  # Cache the inverse
  inv  # Return the computed inverse
}

m <- matrix(c(4, 7, 2, 6), nrow = 2, ncol = 2)

specialMatrix <- makeCacheMatrix(m)

inv1 <- cacheSolve(specialMatrix)
print(inv1)

inv2 <- cacheSolve(specialMatrix)
print(inv2)
