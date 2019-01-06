## Using Lexical scoping to cache values of a matrix and its calculated inverse. 
## Use the cache if no change happens to the matrix.
## If original matrix changes, recalculate the inverse.


## Defines and returns a list of functions to set, get, setinverse and getinverse 
## of a given matrix
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse<- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Return a matrix that is the inverse of 'x'.
## Returns the inverse from cache if present, else computes the inverse,
## caches it and returns the inverse.
cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached value")
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setinverse(i)
  i
}
