## For Programming Assn 2. makeCacheMatrix is meant to generate a
## matrix, compute the inverse, and cache this inverse, while the 
## associated function cacheSolve is aimed to retrieve this cache
## if available, or re-calculate the inverse.

## makeCacheMatrix is meant to generate a
## matrix, compute the inverse, and cache this inverse, but not
##to print the inverse.

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## cacheSolve is aimed to retrieve this cache
## if available, or re-calculate the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  s <- x$getsolve()
  if(!is.null(s)) {
    message("getting cached data")
    return(s) ## This is for when the solve is already cached, i.e s is not null
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s
}
