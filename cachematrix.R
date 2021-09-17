## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function:
### This function creates a list of functions to manage the inverse matrix solution.
### The functions will store and retrieve the original matrix.  It will store a new solution and retrieve 
### a solution that was already created and hence not recalculate the solution.

makeCacheMatrix <- function(x = matrix()) {
  iv <- NULL
  set <- function(y) {
    x <<- y
    iv <<- NULL
  }
  get <- function() x
  setiv <- function(inv) iv <<- inv
  getiv <- function() iv
  list(set = set, get = get, 
       setiv = setiv, 
       getiv = getiv)
}


## Write a short comment describing this function:
### this function will figure out whether a solution has already been calculated by using the functions
### in the list object created with the makeCacheMatrix function.  If a solution already exists, it will
### retrieve the solution.  If not already calculated, it will solve for the inverse matrix.

cacheSolve <- function(x, ...) {
  iv <- x$getiv()
  if(!is.null(iv)) {
    message('Getting Solution')
    return(iv)
  }
  data <- x$get()
  iv <- solve(data)
  x$setiv(iv)
  iv      ## Return a matrix that is the inverse of 'x'
}
