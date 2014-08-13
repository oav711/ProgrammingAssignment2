## The following functions compute and cache the inverse of a matrix.


## Function makeCacheMatrix creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    xinv <- NULL
    set <- function(y){
        # Save the new matrix ...
        x <<- y;
        # ... and clear cache of the inverse matrix
        xinv <<- NULL
    }
    get <- function() x
    setinverse <- function(y) xinv <<- y
    getinverse <- function() xinv
    # Create a list of methods of access to the original and inverse matrices
    list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}


## Function cacheSolve computes the inverse of the special "matrix"
## returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {
    xinv <- x$getinverse()
    # If the inverse has already been calculated and the matrix has not changed ...
    if(!is.null(xinv)) {
        # ... retrieve the inverse from the cache
        message("getting cached data")
        return(xinv)
    }
    data <- x$get()
    # Calculate the inverse of the matrix "data"
    xinv <- solve(data, ...)
    # Save this inverse into special "matrix" created by makeCacheMatrix
    x$setinverse(xinv)
    # Return a matrix that is the inverse of 'x'
    xinv
}
