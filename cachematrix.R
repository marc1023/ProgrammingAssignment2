## ProgrammingAssignment2

## makeCacheMatrix converts an ordinary matrix into
## a cache matrix, a matrix that stores it's inverse.

## cacheSolve calculates the inverse of the cache matrix
## when it has not been calculated yet.


## makeCacheMatrix
## Allows get and set of some matrix "x"
## Stores the matrix inverse in "m"
## Allows get and set of the inverse "m"
## Returns a list of functions for getting and setting "x" and "m"

makeCacheMatrix <- function(x = matrix()) {
		m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## cacheSolve
## Calculates and sets the inverse of a cache matrix
## Returns the inverse "m" of cache matrix "x"

cacheSolve <- function(x, ...) {
		m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}