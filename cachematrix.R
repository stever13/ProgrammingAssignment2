## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function:
### This function creates a list of functions to manage the inverse matrix solution.
### The functions will store and retrieve the original matrix.  It will store a new solution and retrieve 
### a solution that was already created and hence not recalculate the solution.

makeCacheMatrix <- function(x = matrix()) {
  iv <- NULL            # creates a Null solution
  set <- function(y) {  # sets the matrix passed to the makeCacheMatrix as the primary matrix
    x <<- y             # puts x into the parent environment
    iv <<- NULL         # puts null iv into the parent environment
  }
  get <- function() x   # function to retrieve matrix
  setiv <- function(inv) iv <<- inv   # function to set the solved inverse matrix into the parent environment 
  getiv <- function() iv              # retrieve the existing solution from the parent environment
  list(set = set, get = get,          # creates list of functions so each function can easily be accessed name
       setiv = setiv, 
       getiv = getiv)
}


## Write a short comment describing this function:
### this function will figure out whether a solution has already been calculated by using the functions
### in the list object created with the makeCacheMatrix function.  If a solution already exists, it will
### retrieve the solution.  If not already calculated, it will solve for the inverse matrix.

cacheSolve <- function(x, ...) {
  iv <- x$getiv()                 # retrieves inverse matrix (iv) from parent environment
  if(!is.null(iv)) {              # test is a solution is alrady in memory.  If so, then retrieves the solution
    message('Getting Solution')
    return(iv)
  }
  data <- x$get()                 # if there was no solution in memory then retrieve matrix and solve for inverse matrix
  iv <- solve(data)
  x$setiv(iv)                     # stores solution in memory
  iv      ## Return a matrix that is the inverse of 'x'
}
