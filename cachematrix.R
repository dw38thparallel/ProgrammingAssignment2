## Functions makeCacheMatrix and cacheSolve implement solution to build a matrix
## and return the inverse of that matrix, using a cached inverse if one exists

## Function returns a vector of functions which set and get a matrixa, and set and get the inverse of that matrix

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


## Function returns the inverse of a matrix, using a cached value if one exists

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


