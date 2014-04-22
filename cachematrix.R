## Put comments here that give an overall description of what your
## functions do

## Function makeCacheMatrix sets a vector of functions.

makeCacheMatrix <- function(x = matrix()) {
  
  inv <-NULL
  
  #Function 1. Overwrite in cache a matrix
  set <- function(y) {
    x <<- y
  # Every time this function is called erases the inverse
  # matrix 
    inv <<- NULL
  }
  
  ##Function 2. Retrieve original matrix
  get <- function() x
  
  ##Function 3. Save in cache the inverse matrix
  setinverse <- function(inverseMatrix) inv <<- inverseMatrix
  
  ##Function 4. Retrieve inverse matrix
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## This function just returns the inverse matrix.
# It requires the "function vectors" created with makeCacheMatrix

cacheSolve <- function(x, ...) {

  inv <- x$getinverse()
  
  if(!is.null(inv)) {
    message("getting cached matrix")
    return(inv)
  }
  
  ## if there is no "inv" variable in cache,
  ## the function continues and the inverse is calculated
  
  mat <- x$get()
  message("calculating inverse matrix")
  inv <- solve(mat, ...)
  
  x$setinverse(inv)
  inv

}
