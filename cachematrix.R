# Calculating an inverse of a matrix is often computationally expensive. To eliminate the need to re-calculate the inverse of a matrix every time it is needed, the following two functions are coded.

# The makeCacheMatrix function takes a matrix and initializes a cache-saving structure in the form of a list. The list contains 4 functions: two to save and load the original matrix, and two to save and load the inverted matrix.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

# The cacheSolve function is called to return the inverted matrix. As input, it takes the list variable given by the makeCacheMatrix. If the list does not contain the inverted matrix, then it calculates it and returns it. If the list contains the inverted value, it returns the inverted matrix without computing it again.

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
