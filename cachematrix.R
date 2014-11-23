# Matrix inversion is usually a costly computation and 
# following functions can acquire some benefit by caching 
# the inverse of a matrix rather than computing it repeatedly

# makeCacheMatrix function creates a special "matrix" containing a function to
#  1. set the value of the matrix
#  2. get the value of the matrix
#  3. set the value of the inverse
#  4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

# cacheSolve function returns the inverse of the matrix. It first checks if
# the inverse has already been computed. If so, it gets the result and skips the
# computation. If not, it computes the inverse, sets the value in the cache via
# setinverse function.

# Assumption: The matrix supplied is invertible

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}

## sample:

##  > x <- matrix(rnorm(9),3,3)
##  > mat <- makeCacheMatrix(x)

##  > mat$get()
##  [,1]      [,2]        [,3]
##  [1,] -2.7777590 2.0056874 -0.01097625
##  [2,] -0.7233113 0.9698074 -0.16894938
##  [3,] -0.9630773 1.5993233  0.53529208

##  > cacheSolve(mat)
##  [,1]      [,2]      [,3]
##  [1,] -0.7260116  1.003644 0.3018842
##  [2,] -0.5057796  1.377350 0.4243493
##  [3,]  0.2049346 -2.309469 1.1434245

##  > cacheSolve(mat)
##  getting cached data
##  [,1]      [,2]      [,3]
##  [1,] -0.7260116  1.003644 0.3018842
##  [2,] -0.5057796  1.377350 0.4243493
##  [3,]  0.2049346 -2.309469 1.1434245
