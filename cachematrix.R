
## We got the example in the course and changed "mean" to "inverse" because it is the function that we need
## In this first case we are getting the matrix for calling cacheSolve
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
  set <- function(y) {
          x <<- y
          i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## After converting our matrix in Cache Matrix we can call cacheSolve for our makeCacheMatrix
## Here we're going to get the inverse of our matrix
cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if (!is.null(i)) {
          message("getting cached data")
          return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}

## Example
E <- matrix(c(2, 4, 6, 8), 2, 2)
E1 <- makeCacheMatrix(E)
cacheSolve(E1)
