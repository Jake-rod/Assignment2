## The makeCacheMatrix function is constructed to cache another matrix inverse. The cacheSolve function is made up to calculate the inverse of the matrix.

## "makeCacheMatrix" is concocted to store the calculated arrays of a matrix(the inverse matrix)

makeCacheMatrix <- function(x = matrix()) {
    aks <- NULL
    set <- function(y) {
        x <<- y
        aks <<- NULL
    }
    get <- function() x
    getinv <- function() aks
    setinv <- function(INV) {
        aks <<- INV
    }
    return(list(
        set = set,
        get = get,
        getinverse = getinv,
        setinverse = setinv
    ))
}


## Since the "solve" function can determine all elements of the inverse matrix, cacheSolve function design to provide the inverse matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    INV <- x$getinverse()
    if (!is.null(INV)) {
        message("Getting Cached Data!")
        return(INV)
    }
    mtr <- solve(x$get())
    x$setinverse(mtr)
    return(mtr)
}
