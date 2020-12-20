## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  #initialize object
  m <- NULL
  #set x to x and reset m
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  set_inverse <- function(solve) m <<- solve
  get_inverse <- function() m
  list(set = set, get = get, set_inverse = set_inverse, get_inverse = get_inverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  # check if mean exisits
  m <- x$get_inverse()
  if(!is.null(m)){
    print("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data,...)
  x$set_inverse(m)
  m
}


#test matrix
#m1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)
#remember: 1.st use makeCacheMatrix then cacheSolve to avoid atomic vector error
#Common errors are lapackroutine --< matrix needs to have a inverse matrix,
#remember that some don't
