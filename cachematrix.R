##The following two functions can cache the inverse of the matrix 
##so that when we need it again,
##it can be looked up in the cache rather than recomputed. 


##x and inverse are both global variables.Just like the example we
##get, makeCacheMatrix is a function that returns a list which is actually
##a list containing 4 functions to
##1.set the value of the matrix
##2.get the value of the matrix
##3.set the value of the inverse of the matrix
##4.get the value of the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inverse <<- inverse
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

##The following function calculates the inverse of the special "vector" created with the first function. 
##However, it first checks to see if the inverse of the matrix has already been calculated. 
##If so, it gets the result from the cache and skips the computation. 
##Otherwise, it calculates the inverse of the matrix and sets the value of the inverse in the cache via the 
##setinverse function.
cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}
