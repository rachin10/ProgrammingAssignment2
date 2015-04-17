## The following functions calculate and cache the inverse of 
# a matrix. If the inverse of the matrix has already been 
# calculated, the inverse is retrieved from the cache.

## The makeCacheMatrix function creates a special "matrix" 
# object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) { 
        x <<- y
        inv <<- NULL
    }
    get <- function() x 
    setinverse <- function(inverse) inv <<- inverse 
    getinverse <- function() inv 
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## The cacheSolve function computes the inverse of the special
# "matrix" returned by makeCacheMatrix above. If the inverse 
# has already been calculated (and the matrix has not changed),
# then cachesolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
    inv <- x$getinverse() 
    if(!is.null(inv)) { 
        message("getting cached data")
        return(inv) 
    }
    data <- x$get() 
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
