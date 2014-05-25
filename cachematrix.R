## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function, makeCacheMatrix creates a special "Matrix", which is a list containing a function to
##set the value of the Matrix
##get the value of the Matrix
##set the inverse Matrix
##get the inverse Matrix
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  } ##set the value of the Matrix
  get <- function() x ##get the value of the Matrix
  setinverse<- function(inverse) inv <<-inverse ##set inverse Matrix
  getinverse <- function() inv ##get inverse Matrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## Write a short comment describing this function
## The following cacheSolve function calculates the mean of the special "Matrix" created with the above makeCacheMatrix function.
## However, it first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the matrix and sets the value of the inverse in the cache via the setmatrix function.

cacheSolve <- function(x = matrix(), ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
      message("getting inverse of the cached matrix")
      return(inv)
    }
    matrix <- x$get()
    inv <- solve(matrix, ...)
    x$setinverse(inv)
    inv
  }





