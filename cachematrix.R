## makeCacheMatrix creates a "matrix" object that can cache its inverse.
## cacheSolve retrieves the cached inverse matrix if it has already been computed.
## If the inverse matrix has not already been computed, cacheSolve will compute and cache it.

## makeCacheMatrix ###
## Given an input matrix x, creates a special "matrix" object that can cache its inverse.
## Creates a list containing functions to get/set the value of the input matrix
## and get/set the value of the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    
    # define functions to get and set input and inverse matrices for "matrix" object
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(solve) m <<- solve
    getinv <- function() m
    
    # return list of get/set functions
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


### cacheSolve ###
## Computes the inverse of the special "matrix" x returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
    m <- x$getinv()
    
    # if matrix inverse has previously been cached, return cached inverse
    if(!is.null(m)) { 
        message("getting cached data")
        return(m)
    }
    
    # if matrix inverse has not previously been cached, 
    # compute and cache inverse in matrix object and return matrix inverse
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
}
