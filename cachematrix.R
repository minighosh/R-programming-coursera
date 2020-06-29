## Creating a function to find the inverse of a nonsingular matrix
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

  p <- NULL
  set <- function(y) {
    x <<- y
    p <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) s <<- solve
  getsolve <- function() p
  list(set = set, get = get,
    setsolve = setsolve,
    getsolve = getsolve)

}


## The following function calculate the inverse of a matrix. First it checks whether it is already calculated or not. If yes then it takes it from the cache otherwise it compute the inverse of the matrix and then returns the value.


cacheSolve <- function(x, ...) {
  p <- x$getsolve()
  if(!is.null(p)) {
    message("getting inverse of the given matrix")
    return(p)
  }
  data <- x$get()
  p <- solve(data, ...)
  x$setsolve(s)
  p

 ## Here p returns the matrix  which is the inverse of 'x'
}


####Simple Example
##### > A=matrix(1:4,2,2)
#####>  B=makeCacheMatrix(A)
######> C=cacheSolve(B)
######> A%*%C
