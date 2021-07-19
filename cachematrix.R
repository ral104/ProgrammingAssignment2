## These functions work together to cache the inverse of a matrix rather
## than computing it repeatedly.

## makeCacheMatrix creates a special matrix containing a function 
## used to set and get the value of a matrix x and set and get the inverse of x

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
                x <<- y
                i <<- NULL
    }
    get <- function() x
    setinv <- function(solve) i <<- solve
    getinv <- function() i
    list(set = set,
         get = get,
         setinv = setinv,
         getinv = getinv)
}


## cacheSolve calculates the inverse of the matrix created by makeCacheMatrix
## it first checks to see whether the inverse has already been calculated. If so,
## it skips the calculation

cacheSolve <- function(x, ...) {
        i <- x$getinv()
        if(!is.null(i)) {
            message("getting cached data")
            return(i)
        }
        data <- x$get()
        i <- setinv(data, ...)
        x$setinv(i)
        i
}
