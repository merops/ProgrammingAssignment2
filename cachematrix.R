## Caching The Inverse Of A Matrix

## These functions working together will
## calculate the inverse of a matrix and
## cache the inverted matrix so that it
## can be retrieved later if need be.

## makeCacheMatrix is a function that contains
## several functions for setting and retrieving
## the matrix as defined by the user, and the
## inverse of the matrix as defined by the user

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL                               ## Initialize matrix
        set <- function(y) {                    ## Assigns user-defined matrix to variable 'm'
                x <<- y
                m <<- NULL
        }
        get <- function() {                     ## Retrieves user-defined matrix
                x
        }
        setinverse <- function(solve) {         ## Applies the invert function to user-defined matrix
                m <<- solve
        }
        getinverse <- function() {              ## Retrieves inverse of user-defined matrix
                m
        }
        list(set = set, get = get,              ## List of functions contained in makeCacheMatrix
                setinverse = setinverse,
                getinverse = getinverse)
}


## cacheSolve takes a user-defined matrix
## and utilizes the functions contained in
## makeCacheMatrix in order to invert the
## original matrix or retrieve a cached matrix

cacheSolve <- function(x, ...) {
        m <- x$getinverse()                     ## Use getinverse and apply result to 'm'
        if(!is.null(m)) {                       ## If there is already a cached result, print it
                message("getting cached data")
                return(m)
        }
        data <- x$get()                         ## If not, take the user-supplied matrix
        m <- solve(data, ...)                   ## Use the solve function to invert the matrix
        x$setinverse(m)                         ## Cache the result using the setinverse function
        m                                       ## Print the result
}
