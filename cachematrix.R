
#Given a invertible matrix, the following two functions will calculate the inverse matrix or retrieve the inverse matrix from the cache.
#========================================================================================================================================



#Function “makeCacheMatrix” creates a special “matrix” object that can cache its inverse. makeCacheMatrix contains 4 functions: set, get, setmean, getmean.
#============================================================================================================================================================

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




# cacheSolve output is the inverse of the matrix created in makeCacheMatrix
#===========================================================================

## Return a matrix that is the inverse of 'x'
#============================================

cacheSolve <- function(x, ...) {
    
    m <- x$getinverse()
    if(!is.null(m)) {
        message("Extracting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
