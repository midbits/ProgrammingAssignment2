## cacheSolve and makeCacheMatrix is a pair of functions for caching
## the inverse of a matrix to avoid unnecessary computations

## makeCacheMatrix returns a list of getters and setters for 
## storing and retrieving a matrix and it's calculated inverse
## set - store the matrix
## get - retrieve the matrix
## setInv - set the invser of a matrix
## getInv - get the inverse of the matrix
## If a inverse has been set, the getInv will return the inverse, else NULL

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    inv <<- NULL
    x <<- y
  }
  get <- function() x
  setInv <- function(inverse) inv <<- inverse 
  getInv <- function() inv
  list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## cacheSolve checks a cache object for an existing inverse
## and only calculates an inverse if none previously cached exists

cacheSolve <- function(x, ...) {
  inv <- x$getInv()
  if(!is.null(inv)) {
    message("Retrieving cached date")
    return(inv)
  }
  matrix <- x$get()
  inv <- solve(matrix)
  x$setInv(inv)
  inv
}
