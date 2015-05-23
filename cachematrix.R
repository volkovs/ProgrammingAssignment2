## Functions set allowing cache inverted matrix 
## inside "matrix" object.

## Creates a "matrix" object able to cache inverted 
## version of itself. 
## Function parameter should be a square matrix.
makeCacheMatrix <- function(x = matrix()) {
    solved <- NULL
    set <- function(matrix) {
        x <<- matrix
        solved <<- NULL
    }
    get <- function() x
    setSolved <- function(matrix) solved <<- matrix
    getSolved <- function() solved
    list(set = set, get = get,
         setSolved = setSolved,
         getSolved = getSolved)
}

## Returns inverted version of given "matrix" object.
## Cached version is returned 
## (if already calculated previously).
cacheSolve <- function(x, ...) {
    solved <- x$getSolved()
    if(!is.null(solved)) {
        message("getting cached data")
        return(solved)
    }
    data <- x$get()
    solved <- solve(data, ...)
    x$setSolved(solved)
    solved
}