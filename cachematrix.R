## Source: 
## 1. R. D. Peng, "Rprog-030: R Programming", "Programming Assignment 2 Sample", 
##    Coursera Course, July, 2015
##
## The first function, makeCacheMatrix creates a "matrix", 
## which is really a list containing a function to:
##    "Adapted from Sample 1 of Source 1"
##    set the value of the matrix
##    get the value of the matrix
##    set the value of the invertible matrix inverse
##    get the value of the invertible matrix inverse

makeCacheMatrix <- function(x = matrix()) {

  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}

## "Adapted from Sample 2 of Source 1"
## The second function calculates the inverse of the "invertible matrix" created
## with the first function. However, it first checks to see if the inverse has already
## been calculated. If so, it gets the inverse from the cache and skips the computation.
## Otherwise, it calculates of the data and sets the value of the inverse in the cache
## via the setinv function. If the matrix is not invertible, it returns a message.
## This message step is not required for this course. It is just an addition.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  if (det(data) != 0) {
    m <- solve(data, ...)
    x$setinv(m)
    m
  } else return ("Singular Matrix; Not invertible")
}
