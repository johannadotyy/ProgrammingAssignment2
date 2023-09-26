## functions work together to find matrix inverse and store result in cache

## creates a list of functions to be used by cacheSolve

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setmatrix <- function(inv) i <<- inv
        getinverse <- function() i
        list(set = set, get = get,
             setmatrix = setmatrix,
             getinverse = getinverse)

}


## reports matrix inverse if already stored in cache, otherwise calculates
## matrix inverse

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if (!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        mat <- x$get()
        i <- solve(mat, ...)
        x$setmatrix(i)
        i
}
