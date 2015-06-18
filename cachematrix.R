# Author: Joiey Seeley
# Date: 06/17/2015
# 
# R Programming Assignment 2 - Matrix Caching

# The following function is essentially our Matrix container
# It holds the passed in matrix in x, and the inverse
# of the matrix in i
# 
# It doesn't really do any calculations, just the storage,
# so it is similar to a real cache in this manner.
makeCacheMatrix <- function(x = matrix()) {
  # Here I'm initializing i as NULL
  i <- NULL
  
  # The set function sets x to the newly passed in matrix
  # and clears the inverted matrix i
  set <- function(mtx){
    x <<- mtx
    i <<- NULL
  }
  
  # Return the original matrix
  get <- function(){
    x
  }
  
  # Invert the matrix and store it in i
  invert <- function(inv){
    i <<- inv
  }
  
  # Return the inverted matrix
  inverted <- function(){
    i
  }
  
  # Publish our public functions
  list(set = set, get = get, invert = invert, inverted = inverted)
}

# The cacheSolve function takes in a makeCacheMatrix
# and returns the inverse matrix.
# If the matrix inverse has previously been inverted
# it is returned from cache, otherwise the 
# inverse will be solved and cached.
cacheSolve <- function(x, ...) {
  # Try to set inverse from cache here
  inverse <- x$inverted()
  
  if(is.null(inverse)){
    # If inverse is null it has not yet been cached
    # So I will solve and cache here for next time.

    # The following line just provides some visual confirmation that a 
    # cache miss has occurred.
    print("Cache Miss")
    m <- x$get()
    inverse <- solve(m, ...)
    x$invert(inverse)
  }
  
  # Return inverse
  inverse
}
