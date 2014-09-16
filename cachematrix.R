## makeCachedMatrix creates a matrix, however, it is really a list 
## functions to: 

#set the value of the matrix, and reset the cached inversed matrix to NULL
#get the value of the matrix
#setCachedInverse the cached value for the inversed matrix
#getInverse the cached value of the inversed matrix

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setCachedInverse <- function(CachedInverse) inverse <<- CachedInverse
        getInverse <- function() inverse
        list(set = set, get = get,
             setCachedInverse = setCachedInverse,
             getInverse = getInverse)
        
}


## cacheSolve function will check and return the inversed matrix of the "matrix"
# that is created by the above function makeCachedMatrix.
## it will check whether the inversed matrix has been cached first
# if YES, it will return the cached inversed matrix
# otherwise NO, it will produce inversed matrix using solve() function, return it, and also 
# cache the inversed matrix.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getInverse()
        if(!is.null(inverse)) {
                message("getting inverse data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setCachedInverse(inverse)
        inverse
}
