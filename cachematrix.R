## This file provides an interface to compute the inverse of a matrix.
## Once computed, the inverse of a matrix is cached so that it is
## quickly retrieved in subsequent attempts without any re-computation.
##
## Usage:
##    > m <- makeCacheMatrix(matrix(c(1, 4, 6, 0, 2, 9, 7, 5, 6), 3, 3))
##    > m$get()
##     int [1:3, 1:3] 1 2 3 4 5 6 1 2 3
##    > m$getinverse()
##    NULL
##    > str(cacheSolve(m))
##    Computing inverse of matrix...
##     num [1:3, 1:3] -0.2444 0.0444 0.1778 0.4667 -0.2667 ...
##    > str(cacheSolve(m))
##    Returning cached data...
##     num [1:3, 1:3] -0.2444 0.0444 0.1778 0.4667 -0.2667 ...
##    > str(m$getinverse())
##     num [1:3, 1:3] -0.2444 0.0444 0.1778 0.4667 -0.2667 ...


## This function acts as a constructor for CacheMatrix objects.
## A CacheMatrix object encapsulates an invertible matrix and
## its inverted matrix, both accessible through a set of getter
## and setter functions.
##
## Note: This function assumes that the matrix passed as input
##       is invertible.
##
## Parameters:
##    x - A matrix object as in as.matrix(). Defaults to matrix().
##
## Returns:
##    A CacheMatrix object implemented as a list.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(i) inv <<- i
    getinverse <- function() inv
    list(set = set,
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function computes and memoizes the inverse of a matrix.
## If the inverse of the matrix has already been computed, it
## returns the memoized inverse matrix. Otherwise it computes it
## using the solve() function.
##
## Parameters:
##    x - A CacheMatrix object as in makeCacheMatrix().
##    ... - Additional arguments.
##
## Returns:
##    The inverse of the input matrix.

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(is.null(inv)) {
        message("Computing inverse of matrix...")
        inv <- solve(x$get(), ...)
        x$setinverse(inv)
    } else {
        message("Returning cached data...")
    }
    inv
}
