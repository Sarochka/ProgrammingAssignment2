##make a "matrix" object that can cache its inverse
##NOTE: I used "solve" in the code for inverse as that is what R uses to calculate inverse
makeCacheMatrix <- function(x = matrix()) {
##set the value of the matrix
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
##get the value of the matrix
  get <- function() x
##set the value of the inverse (use "solve" as it calculates inverse)
  setsolve <- function(solve) m <<- solve
##get the value of the inverse "solve"
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}
____
##returns the inverse of matrix 'x' by 
## returning it from the cache if matrix has not changed and it has already been calculated
## or calculating the inverse
cacheSolve <- function(x=matrix(), ...) {
##check to see if inverse matrix was already calculated
  m <- x$getsolve()
## if inverse was already calculated... 
  if(!is.null(m)){
    message("getting cached data")
## returns this inverse
    return(m)
  }
##otherwise calculates inverse
  data <- x$get()
  m <- solve(data, ...)
##sets the value of the inverse
x$setsolve(m)
##returns matrix
  m
}
