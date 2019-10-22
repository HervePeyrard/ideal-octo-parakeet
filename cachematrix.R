## --------------------------------------------------------------
## makeCacheMatrix
## --------------------------------------------------------------
## Creates a special "matrix" object (that can cache its inverse)
## Which is really a list containing a function to
##     1. set the value of the vector
##     2. get the value of the vector
##     3. set the value of the mean
##     4. get the value of the mean
##
## NB. inverse matrix is computed using "solve" function
##     for this assignment, matrix supplied must always be invertible.
## --------------------------------------------------------------

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set,
         get = get,
         setinv = setinv,
         getinv = getinv)
}


## -------------------------------------------------------------------------------
## cacheSolve
## -------------------------------------------------------------------------------
## Computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
## -------------------------------------------------------------------------------

cacheSolve <- function(x, src = x$get(), ...) {
    ## if the matrix has changed, refresh the content
    if (!identical(x$get(), src)) {
        message("refreshing source data")
        x$set(src)  
    }
    ## inverse retreival
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }

    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv  
}