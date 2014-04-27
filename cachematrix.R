## This function will calculate the Inverse of a Matrix and cache the result.
## Allowing CPU / Time to be saved by retrieving the cached value if it is 
## asked to calculate the inverse of a Matrix that has been cached.
## Test matrix1 :  myMatrix1 <- mapply(rnorm,500,1:500)
## Test matrix2 :  myMatrix2 <- mapply(rnorm,1000,1:1000)
## Used system.time(solve(myMatrix1))
## Used system.time(solve(myMatrix2))

## Create special Matrix object to cache the inverse.

makeCacheMatrix <- function(x = matrix()) {
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
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m)){
    message("Getting Cached Data!")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}
