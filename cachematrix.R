## The objective is to compute and cache the inversion of a matrix.

## makeCacheMatrix creates a special "matrix", which is a list containing a function to:
## 1) Set the value of the matrix.
## 2) Get the value of the matrix.
## 3) Set the value of the matrix inverse.
## 4) Get the value of the matrix inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve: calculates the inverse of the special "matrix" created in makeCacheMatrix.
## If the inverse has already been calculated, it gets the inverse from the cache and skips the calculation.
## Otherwise, it calculates the inverse of the matrix and sets the value of the inverse in the cache via
## the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("Getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}