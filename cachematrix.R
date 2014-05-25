## These two functions combine to create a matrix that can cache its inverse
## and subsequently calculate and return the inverse of the matrix. This function
## assumes that the inverse can be calculated.

## Creates a "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  
  set <- function(y=matrix()) {
    x<<- y
    i <- NULL
  }
  get <- function() x
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Return a matrix that is the inverse of 'x'
## If the inverse has already been calculated, it retrieves it from cache

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
}
