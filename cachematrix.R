## The functions contained in this script compute, cache, and return the inverse of a matrix that is input as the argument into makeCacheMatrix

makeCacheMatrix <- function(x = matrix()) {
    ## This function creates a special "matrix" object that can cache its inverse
    ## The inverse of the argument matrix is calculated and cached
    ## The return object is a list containing the results of the various functions used inside the parent function: makeCacheMatrix
    
    m <- NULL
    
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    get <- function() x
    setsolve <- function(solve) {
        m <<- solve
    }
    
    getsolve <- function() {
        m
    }
    
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}

cacheSolve <- function(x, ...) { 
    ## The function returns the cached inverse matrix from makeCacheMatrix if matrix has not changed
    ## Otherwise, the function computes the inverse
    
    m <- x$getsolve()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m
}