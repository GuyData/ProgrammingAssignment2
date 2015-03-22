## Caching the inverse of a matrix
## Author: Guy Jason Shaw
## Date: 22/03/2015

## Student assignment for the Coursera course 'R Programming'
## by Roger D. Peng, PhD, Jeff Leek, PhD, Brian Caffo, PhD
## Programming Assignment 2: Lexical Scoping

## This R Script file includes the functions: makeCacheMatrix, cacheSolve


## makeCacheMatrix takes a matrix object as input. It creates a special "matrix"
## object as output that can cache both the input matrix and it's inverse and
## allow both to be returned or replaced using it's get, set, getinv and setinv
## functions.

makeCacheMatrix <- function(x = matrix()) {
    ## If no input matrix is provided then default to an empty matrix.

    ## Initialize the inverted matrix cache.
    inv <- NULL

    ## Function to allow input matrix to be changed.
    ## It will will also clear the inverted matrix cache.
    set <- function(x.new = matrix()) {
        x <<- x.new
        inv <<- NULL
    }

    ## Function to return the cached input matrix
    get <- function() x

    ## Function to cache the inverted matrix
    setinv <- function(inverted) inv <<- inverted

    ## Function to return the cached inverted matrix
    getinv <- function() inv

    ## Return the four functions as a list
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve takes an object returned by makeCacheMatrix above plus any
## additional arguments to be passed to the solve function. It will return the
## cached value of the inverted matrix or use the solve function to calculate it.
## This function assumes that the matrix supplied is always invertible.
## Since this function is specifically to calculate the inverse of a matrix,
## the b argument of solve is intentionally missing so it will default to an
## identity matrix.

cacheSolve <- function(x, ...) {
    ## Check if the inverse has already been calculated and if so return it
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached inverse")
        return(inv)
    }

    ## Otherwise calculate the inverse of the matrix, cache it and return it
    data <- x$get()
    message("calculating new inverse")
    inv <- solve(a=data,b=, ...)
    x$setinv(inv)
    inv
}
