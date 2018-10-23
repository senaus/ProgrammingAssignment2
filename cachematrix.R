## Functions written for the programming assignment of week 3 of the 
## "R Programming" course on Coursera. The goal is to develop functions that
## compute the inverse of a matrix or retrieves it from the cache if it already 
## exists, saving computation time.

## This function creates a special "matrix"object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    I <- NULL
    set <- function(y) {
        x <<- y
        I <<- NULL
    }
    get <- function() x
    setSolve <- function(inverse) I <<- inverse
    getSolve <- function() I
    list(get = get, set = set , getSolve = getSolve, setSolve = setSolve)
}


## This function computes the inverse of the special "matrix" returned by 
## `makeCacheMatrix` above. If the inverse has already been calculated (and the
##  matrix has not changed), then `cacheSolve` retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    I <- x$getSolve()
    if (!is.null(I)) {
        message("Getting inverse from the cache")
        return(I)
    }
    matrix_data <- x$get()
    I <- solve(matrix_data, ...)
    x$setSolve(I)
    I
}
