## makeCacheMatrix creates a matrix object whose inverse can be cached.
## cacheSolve returns the inverse of the cacheMatrix object, 
## making use of the cache if the inverse is known.

## Creates a cacheMatrix object with four functions: $set, $get, $setinv and $getinv
## If known, the inverse of the matrix can be supplied as a parameter.

makeCacheMatrix <- function(x = matrix(), inverse = NULL) {
        inv <- inverse
        set <- function(y, inverse = NULL) {
            x <<- y
            inv <<- inverse
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
    
}


## Returns a cacheMatrix object which is the inverse of the given matrix.
## The inverse of the returned matrix is known and cached.
## The cache is returned unless it is NULL.

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    
    if(!is.null(inv)){
        message("Getting cached data")
        return(inv)
    }
    
    inv <- solve(x$get(), ...)
    matrInv <- makeCacheMatrix(inv,x)
    x$setinv(matrInv)
    
    matrInv
}
