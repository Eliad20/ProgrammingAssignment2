## The function is taking a matrix, inverted it and caching it for the next time,
## so if the inversion has done once on the current matrix - it shouldn't done again, unless a new matrix has been set. 

## This function defining an object - "makeCacheMatrix" a list that getting a matrix as a input and defining
## 4 functions (2 setters & 2 getters) & 2 objects (x & m).

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


## This function getting an object of the makeCacheMatrix type (makeCacheMatrix of a matrix) as an input
## Then it check whether it had been performed on the current data, if so - then it print the cached inverted matrix
## and write: "getting cached data", if not - it will apply the 'solve' function to invert the matrix
## and then print the inverted matrix that just been calculated.

cacheSolve <- function(x, ...) {
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
