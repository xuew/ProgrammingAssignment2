## This file is for the assignment of Coursera class: R Programming, 3rd week. The goal is to cache the inverse of a matrix which could save time for repeated computation, especially for large-scale matrices. It contains 2 functions: makeCacheMatrix() and cacheSolve(). 
## 
## 1) makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
##    Input: a user-defined matrix
##    Output: a list containing functions to:
##            set the matrix
##            get the matrix
##            set the inverse of matrix
##            get the inverse of matrix
##

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        ## Generate functions for the output list
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## 2) cacheSolve
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
##    Input: a "matrix" object generated from makeCacheMatrix()
##    Output: the inverse of user-defined matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        
        ## If the computation had been done, extract and return the inverse from cache directly.
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        
        ## If not, use solve() to compute the inverse.
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}