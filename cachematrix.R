## This is an implementation of a matrix with is inverse cached in memory

## This first function creates the "object" cached matrix. It is a list of all the function that manipulate it

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  list(set = set, get = get, setinverse= setinverse, getinverse=getinverse)
}


## Implementation of the function that returns the inverse of the matrix. It doesn't calculate it again if it has been cached.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setinverse(i)
  i
}
