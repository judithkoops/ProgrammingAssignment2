## Assignment 2: Lexical Scoping
## This code creates two functions which together cache the inverse of a matrix

## makeCacheMatrix; using information from objects 'data', 'nrow', and 'ncol' 
## this function creates a matrix 'x' using info from the objects and creates
## 'inv' as the inverse of `x'

makeCacheMatrix <- function(x = matrix(), data, nrow, ncol) {
      x <- matrix(data, nrow, ncol)
      inv <- NULL
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      get <- function() x
      setmean <- function(solve) inv <<- solve
      getmean <- function() inv
      list(set = set, get = get,
           setmean = setmean,
           getmean = getmean)
}

## cacheSolve returns inv (if inv is not missing), or calculates and
## returns inv, if inv is missing

cacheSolve <- function(x, ...) {
      inv <- x$getmean()
      if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
      }
      data <- x$get()
      inv <- solve(data)
      x$setmean(inv)
      inv
}