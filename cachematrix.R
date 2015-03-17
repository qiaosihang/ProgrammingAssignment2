## Put comments here that give an overall description of what your
## functions do

## This makeCacheMatrix function's ability is that creating such functions, including get(),
##setsolve(),getsolve(),etc., and making preparation for caching the inverse of the Matrix.

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {                      ##change the martix
            x <<- y
            m <<- NULL
      }
      get <- function()                         ##get the martix
      setsolve <- function(solve) m <<- solve   ##set the inverse of the matrix
      getsolve <- function() m                  ##get the inverse of the matrix
      list(set = set, get = get,
           setsolve = setsolve,
           getsolve = getsolve)
}


##The following function's ability is judging if the matrix is modified and if the inverse
##of the matrix has been calculated. If it's yes, use the cached data without calculation.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      m <- x$getsolve()
      if(!is.null(m)) {                         ##if m equals NULL, we can use the cached data.
            message("getting cached data")
            return(m)
      }
      data <- x$get()                           ##Or use the solve() to calculate the inverse of the matrix.
      m <- solve(data, ...)                     ##change m to show the inverse matrix has calculated yet.
      x$setsolve(m)
      m
}
