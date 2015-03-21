## These two functions demonstrate the use of cache by assigning values to an environment different than the current environment
## and performing a function to return a value from cache without needing to compute the function

## This function creates a matrix that stores its inverse to cache

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse, 
             getinverse = getinverse)
}


## This function looks to compute the inverse of the matrix returned by the
## makeCacheMatrix function and if no changes, will return the value from cache 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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