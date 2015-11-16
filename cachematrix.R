## cachematrix.R
#    A set of functions that caches the inverse of a matrix, for the sake of
#    processing efficiency.

## makeCacheMatrix
#    This function creates a special "matrix" object, including a list of 
#    functions that: 1) set/get the value of a matrix; and 2) set/get the
#    inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) s <<- solve
        getsolve <- function() s
        list(set=set, get=get, setsolve=setsolve, getsolve=getsolve)
}

## cacheSolve
#    This function checks to see if the inverse of the special matrix in the 
#    argument list has been cached. If it has, the cached value is returned.
#    If it has not, the inverse is calculated, cached, and returned.

cacheSolve <- function(x, ...) {
        s <- x$getsolve()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setsolve(s)
        s
}

## TEST SCRIPT
#    Load the functions        : source("cachematrix.R")
#    Ensure repeatability      : set.seed(1)
#    Initialize special matrix : tst <- makeCacheMatrix()
#    Populate the matrix       : tst$set(matrix(runif(36,0,1), nrow=6, ncol=6))
#    Examine the contents      : tst$get()
#    Calculate the inverse     : cacheSolve(tst)
#    Retrieve from cache       : cacheSolve(tst)
#    Verify the inverse        : round(tst %*% tst_inv)
