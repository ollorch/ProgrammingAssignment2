## 
## 2015.08.28 Jorge Gonzalez 
##


## This function creates a list containing a set of caching functions

makeCacheMatrix <- function(x = matrix()) {
  
  inv_matrix <- NULL
  
  set <- function(y) {
    x <<- y
    inv_matrix <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv_matrix <<- inverse
  getinverse <- function() inv_matrix
  
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}



## This function returns a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
  
  inv_matrix <- x$getinverse()
  
  if(!is.null(inv_matrix)) {
    message("getting cached data.")
    return(inv_matrix)
  }
  
  data <- x$get()
  inv_matrix <- solve(data)
  x$setinverse(inv_matrix)
  inv_matrix
  
}
