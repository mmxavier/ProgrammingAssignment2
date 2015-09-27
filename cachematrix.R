## These functions set a cache and a funtion to calclate de inverse of a matrix
## The function makeCacheMatrix stores a dataframe with the matrix and its inverse.
## The function cacheSolve calls the previous one and if the given matrix has its
## stored, it will return it. Otherwise, it will calculate, cache the solution and 
## return it.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}