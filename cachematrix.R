## Put comments here that give an overall description of what your
## functions do

## Creates a list of functions that set and get the value of a matrix along with the inverse of that matrix.   Called before cacheSolve().

makeCacheMatrix <- function(x = matrix()) {
    invMatrix <- NULL
    set <- function(y) {
        x <<- y
        invMatrix <<- NULL
        
    }
    get <- function() x
    setInverse <- function(inverse) invMatrix <<- inverse
    getInverse <- function() invMatrix
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Returns the inverse of the matrix previouisly defined with the makeCacheMatrix() function and prevents duplicate inverse calculations.
## Pass the list of functions created with makeCacheMatrix(x) where x is the matrix to calculate its inverse.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x' where 'x' was previously passed to makeCacheMatrix.
    invMatrix <- x$getInverse()
    if(!is.null(invMatrix)) {
        message("getting cached data")
        return(invMatrix)
    }
    data <- x$get()
    invMatrix <- solve(data, ...)
    x$setInverse(invMatrix)
    invMatrix
}
