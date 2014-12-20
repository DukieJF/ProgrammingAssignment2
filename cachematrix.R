## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  
    inverse <- NULL
  
    set <- function(y) {
      x <<- y
      inverse <<- NULL
    }
    
    get <- function() {
      x
    }
    
    getinverse <- function(x) {
      inverse
    }
    
    setinverse <- function(x) {
      inverse <<- x
    }
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  cachemat <- makeCacheMatrix(x)
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}
