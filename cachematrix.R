## The aim of the assignment is to create a matrix which can reduce the time to calculate its inverse

## This function creates a object that can cache its inverse

makeCacheMatrix <- function(x = matrix(sample(1:100,9),3,3)) {
  setvar <- NULL
  set <- function(y) {
    x <<- y
    setvar <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) setvar <<- solve
  getsolve <- function() setvar
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## This function calculates the inverse of matrix created by makeCacheMatrix

cacheSolve <- function(x, ...) {
    inverse <- x$getinverse()
    if(!is.null(inverse)) {
        message("Get Cached Data")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$setinverse(inverse)
    inverse        
}
