## The following functions are an example for the lexical scoping in R. They
## show how R makes use of objects in different environments.

## This function creates a matrix in a special way, so that the inverse of the
## matrix can be calculated and stored in a cache. This cache is later accessable
## for other functions.

makeCacheMatrix <- function(x = numeric()) {
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

## This function calculates the invers of a matrix, if it was not already calculated.
## If the inverse already excists, the function retrieves the invers from the cache.

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

        ## Return a matrix that is the inverse of 'x'