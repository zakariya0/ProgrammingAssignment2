## These functions will produce a matrix and invert it or retrieve the inverted form of it
## The first function produces a matrix and initializes methods that can get it and set it.

makeCacheMatrix <- function(x = matrix()) 
{
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function initially tries to retrieve the cached inverted form of the matrix above
## and if there's nothing available it inverts the matrix

cacheSolve <- function(x, ...) 
{
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
