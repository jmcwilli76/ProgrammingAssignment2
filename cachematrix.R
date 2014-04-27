## This function will calculate the Inverse of a Matrix and cache the result.
## Allowing CPU / Time to be saved by retrieving the cached value if it is 
## asked to calculate the inverse of a Matrix that has been cached.
## Test matrix1 :  myMatrix1 <- mapply(rnorm,500,1:500)
## Test matrix2 :  myMatrix2 <- mapply(rnorm,5000,1:5000)
## Used system.time(solve(myMatrix1)) to get a baseline.
## Used system.time(solve(myMatrix2)) to get a baseline.

## Create special Matrix object to cache the inverse.

makeCacheMatrix <- function(x = matrix()) {
  ##  This creates a list of functions to be called by cacheSolve.
  m <- NULL
  set <- function(y){
    x <<-y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) m <<- solve
  getInverse <- function() m
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)

}


## Calculates the inverse of the special Matrix or Retrieve cached Matrix.

cacheSolve <- function(x, ...) {
  ## x should be the list created with makeCacheMatrix.
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()  # Get the cached object if it exists.
  if(!is.null(m)){ # If m is null then no cached object exists.
    message("Getting Cached Data!")
    return(m)
  }
  data <- x$get() #  Returns the matrix passed to the mackeCacheMatrix
  m <- solve(data, ...) # Calculates the inverse matrix
  x$setInverse(m) # Stores the inverse matrix to the cache.
  m
}
