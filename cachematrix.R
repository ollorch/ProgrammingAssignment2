## 
## 2015.08.20 Jorge Gonzalez 
##


## This function creates a list containing a set of caching functions

makeCacheMatrix <- function(x = matrix()) {
  
  # Initialize the Cached List
  inv_matrix <- NULL
  
  # Function set() -> Sets the x argument as the cached object
  set <- function(y) {
    x <<- y
    inv_matrix <<- NULL
  }
  
  # Function get() -> Returns the cached object
  get <- function() x
  
  # Function setInverse () -> Sets the cached matrix
  setinverse <- function(inverse) inv_matrix <<- inverse
  
  # Function getInverse () -> Returns the cached matrix
  getinverse <- function() inv_matrix
  
  # Returns a list with the functions
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}



## This function returns a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
  
  # Check if a previous cached matrix exists
  inv_matrix <- x$getinverse()
  
  # If a previous cached matrix exits, the cached copy is returned
  if(!is.null(inv_matrix)) {
    message("getting cached data.")
    return(inv_matrix)
  }
  
  # Otherwise, the inverse is calculated, cached and returned
  data <- x$get()
  inv_matrix <- solve(data)
  x$setinverse(inv_matrix)
  inv_matrix
  
}
