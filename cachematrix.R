## Use closures to implement caching of the matrix inverse calculation.

## makeCacheMatrix returns a list of functions for setting and getting a
## matrix and its inverse from the closure of the initial function call.
makeCacheMatrix <- function(x = matrix()) {
    x.inverse <- NULL

    ## Replace the underlying matrix and reset the inverse to NULL.
    set <- function(y) {
        x <<- y
        x.inverse <<- NULL
    }

    ## Get the underlying matrix
    get <- function() { x }

    ## Set the cached inverse
    setInverse <- function(inverse) { x.inverse <<- inverse }

    ## Get the cached inverse
    getInverse <- function() { x.inverse }

    ## Return a list of the local functions.
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## cacheSolve returns the inverse for a cacheMatrix created with
## makeCacheMatrix. If the inverse was previously calculated, then it's cached
## value is returned, otherwise the inverse is computed and cached.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse <- x$getInverse()

    if (!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }

    data <- x$get()
    inverse <- solve(data, ...)
    x$setInverse(inverse)
    inverse
}

## A simple test.
test <- function(epsilon = 1.0e-10) {
    a <- matrix(data=rnorm(100*100), nrow=100, ncol=100)
    ac <- makeCacheMatrix(a)
    ac.inv <- cacheSolve(ac)
    tst = ac$get() %*% ac.inv
    residual <- tst - diag(100)
    rmserr <- sqrt(sum(residual*residual))
    if (rmserr > epsilon) stop("RMS error too large!")
    rmserr
}
