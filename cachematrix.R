## This R file contains 2 function. The first 'makeCacheMatrix' function can be 
## used to create a matrix object. It has functions nested inside to store a matrix, 
## return it, store its inverse and return the value of the inverse matrix.
## -------------------------------------------------------------------------
## The second function 'cacheSolve' is used to compute the inverse of the matrix
## from 'makeCacheMatrix' function's object.
## This function retrieves the inverse of the matrix from saved data if 
## the inverse has been calculated once.

## 'makeCacheMatrix' function has 4 functions nested inside it namely set, get, 
## setinverse and getinverse.  
## This function takes a matrix as an argument.

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {   
        x <<- y
        inverse <<- NULL
    }
    get <- function() x   
    setinverse <- function(inv) inverse <<- inv     
    getinverse <- function() inverse    
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function checks for a cached inverse and if it does not find one 
## then calculates it. 
## This function takes a matrix object as an argument.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inverse <- x$getinverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)  
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$setinverse(inverse)
    inverse
}
