#These two functions allow us to create a matrix and cache its inverse.
#This way we can get the inverse without waiting for the calculation.
#It is very similar to the example given to us.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  #Using ____$setm(matrix(c(_,_,_,_,_,_,_,_,_))) will change the 3x3 matrix.
  # <<- operator: so that we can use x in the next function
  setm <- function(y) {
    x <<- y
    inv <<- NULL
  }
  #Use ____$getm() to see the matrix
  getm <- function() x
  setinverse <- function(solve) inv <<- solve
  getinverse <- function() inv
  list(setm = setm, getm = getm,
       setinverse = setinverse,
       getinverse = getinverse)
}

#inv will not be NULL if we already have the inverse calculated!!
#otherwise the latter part calculates and stores the inverse.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$getm()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}