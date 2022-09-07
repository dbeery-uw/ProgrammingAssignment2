#ProgrammingAssignment2

##Matrix simulation to test functions
z <- matrix(c(1:4, 10:13, 5:8, 6:9, 5:8, 12:16), nrow = 5, ncol = 5)
solve(z)

##Function 'makeCacheMatrix' creates a list of functions to:
  ##(1) set the value of the matrix
  ##(2) get the value of the matrix
  ##(3) set the inverse of the matrix
  ##(4) get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Function 'cacheInverse' first checks to see if the inverse of matrix 'x' was already calculated. 
## If not, cacheInverse calculates inverse of matrix and caches it. 
cacheInverse <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

#Test run
cacheInverse(makeCacheMatrix(z)) #success!

             