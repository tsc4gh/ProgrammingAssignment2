## This package provides two functions which allow to cache the result of a 
## matrix inversion to avoid possibly time-consuming re-calculations of the 
## inverse e.g. inside loops.
##
## Functions:
##    makeCacheMatrix:  Prepare the cache
##    cacheSolve:       Get cached value or update cache if necessary


## Function makeCacheMatrix
## Creates a cache to store the original and the inverted matrix 
##
## Call
##    Cache <- makeCacheMatrix(x)
##
## Arguments
##    x     : matrix, optional
##            Matrix to be inverted
##
## Returns
##    Cache : List containing functions to access the cache
##            set(x)       : store original matrix x
##            get          : retrieve original matrix
##            setinv(xinv) : store inverted matrix xinv
##            getinv       : retrieve inverted matrix

makeCacheMatrix <- function(x = matrix()) {
    
    inv <- NULL
    set <- function(y) {
        ## store (new) original matrix and clear inverse
        x <<- y
        inv <<- NULL
    }
    get <- function() {
        x
    }
    setinv <- function(xinv) {
        inv <<- xinv
    }
    getinv <- function() {
        inv
    }
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Function cacheSolve
## Checks the cache for the inverse of the matrix. Returns inverse from cache
## if available, otherwise calculates inverse (and puts it into cache).
##
## Call
##    inverse <- cacheSolve(Cache, ...)
##
## Arguments
##    Cache : List created by function makeCacheMatrix
##    ...   : additional arguments passed to function solve
##            (calculation of the inverse)
##
## Returns
##   inverse : inverse of the original matrix
##             The original matrix may be (initially) set via function 
##             makeCacheMatrix or (later) via function set from the cache

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    inv <- x$getinv()
    if (!is.null(inv)) {
        ## inverse available in cache
        message("getting cached data")
        return(inv)
    }
    ## inverse not available in cache, calculate
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
