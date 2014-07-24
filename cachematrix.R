## This pair of functions provide a mechanism for computing and caching
## matrix inverses.  Use cacheMat <- makeCacheMatrix(x) to represent the  
## matrix x and call cacheSolve(cacheMat) to return the inverse of x.
##
## Returns a list that represents the matrix given as parameter x.  The list
## includes 2 pairs of get/set functions for accessing the the matrix x and
## its inverse.
##
## Example:
##   mat <- matrix(rnorm(16,10,2), nrow=4, ncol=4)
##   cmat <- makeCacheMatrix(mat)
##   inv <- cacheSolve(cmat)
##
makeCacheMatrix <- function(x = matrix()) {
    x.inv <- NULL
    set <- function(mat) {
        x <<- mat
        x.inv <<- NULL
    }c
    get <- function() { x }
    setInverse <- function(inv) { x.inv <<- inv }
    getInverse <- function() { x.inv }
    list(set = set, 
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## Returns the inverse of a matrix represented by a list from the
## makeCacheMatrix function.  If the matrix inverse has not been computed, 
## it will be computed, cached, and returned.  If it has already been
## computed, then the cached copy is returned.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    if (!is.null(inv)) {
        message("using cached inverse")
    }
    else {
        inv <- solve(x$get())
        x$setInverse(inv)
    }
    inv
}
