## R Programming Assignment 2 - Caching Inverse Matrix

##Matrix inversion is usually a costly computation and there may be some 
##benefit to caching the inverse of a matrix rather than compute it repeatedly 
##(there are also alternatives to matrix inversion that we will not discuss here). 
##Your assignment is to write a pair of functions that cache the inverse of a matrix.


##Caching the Inverse of a Matrix

##1. Describing the functions 'makeCacheMatrix' and 'cacheSolve' 

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i 
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

##This function computes the inverse of the special “matrix” returned by 
##makeCacheMatrix above. If the inverse has already been calculated (and the matrix 
##has not changed), then cacheSolve shall retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if (!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}

##2. Testing 

##Below we call the function with a matrix, compute the inverse, 
##retrieve the inverse from the cache list, change the call matrix to the inverse, 
##compute the inverse on that and return the original function.

A <- matrix(c(1,2,3,4),2,2) 
A1 <- makeCacheMatrix(A)
cacheSolve(A1)