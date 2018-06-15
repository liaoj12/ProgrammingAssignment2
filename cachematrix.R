## This function creates a special matrix object
## that will be a list containing the following functions:
## 1. set the value of the matrix x
## 2. get the value of the matrix x
## 3. set the value of the inverse of the matrix x
## 4. get the value of the inverse of the matrix x

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

## This function calculates the inverse of the special
## "matrix" created with the aboved function. It checks
## to see if the inverse of the matrix has already been
## calculated. If so, it gets the inverse of the matrix
## from the cache and skips the computation. Otherwise,
## it calculates the inverse of the matrix and stores
## the result in the cache via the setsolve function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
