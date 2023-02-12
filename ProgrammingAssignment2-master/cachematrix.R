## This shows how to create two functions that will ultimately create 
## a object that stores a square matrix and caches its inverse

## This function will create a matrix, based on internal functions that sets
## the elements of the matrix, gets or retrieves them, and also sets the 
## elements of the matrix inverse, and gets/retrieves them

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
## Here are the internal functions
  
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function will calculate the inverse of the matrix developed from
## the makeCacheMatrix function, first checking to see if the inverse has
## already been calculated (if it has, it takes the cached inverse and skips
## computation; otherwise it calculates inverse of matrix and caches it via
## setinverse function)

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  matrix2invert <- x$get()
  inv <- solve(matrix2invert, ...)
  x$setinverse(inv)
  inv
}

