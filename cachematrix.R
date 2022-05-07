## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    asb <- NULL
    get <- function() x
    set <- function(y) {
        x <<- y
        asb <<- NULL
    }
    finv <- function() asb
    fncinv <- function(INV) {
        asb <<- INV
    }
    return(list(
        set = set,
        get = get,
        getinverse = finv,
        setinverse = fncinv
    ))
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    INV <- x$getinverse()
    if (!is.null(INV)) {
        return(INV)
    }
    m <- solve(x$get())
    x$setinverse(m)
    #return(m)
}
