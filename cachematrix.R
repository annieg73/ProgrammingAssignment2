## This function creates a data structure to store the inverse of a matrix
## This is completed in some steps:
## First: set the value of the matrix
## Second: get the value of the matrix
## Third: set the value of inverse of the matrix
## Fourth: get the value of inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get,
       setinverse=setinverse,
       getinverse=getinverse)
}



## This function stores the inverse of the matrix if it is not present
## It assumes that x has been already created using makeCacheMatrix


cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}
