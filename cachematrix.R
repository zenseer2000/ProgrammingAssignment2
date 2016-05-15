## Put comments here that give an overall description of what your
## functions do
#
# Programming Assignment 2--write a pair of functions that cache the inverse of a matrix
#
#########################################
# FULL EXAMPLE USAGE OF ALL INCLUDED FUNCTIONS:
# mtx <- makeMatrix(4)
# cm <- makeCacheMatrix(mtx)
# cachesolve(cm)
#########################################
#
## Write a short comment describing this function
# makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse
#
# Usage: makeCacheMatrix(mtx), where mtx is a square invertible matrix
#

makeCacheMatrix <- function(x = matrix()) {
	cacheInvMtx <- NULL
	set <- function(y) {
		x <<- y
		cacheInvMtx<<- NULL
	}
    get <- function() x
    setInverse <- function(inverse) cacheInvMtx <<- inverse
    getInverse <- function() cacheInvMtx
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

#
## Write a short comment describing this function
#
# cacheSolve--This function computes the inverse of the special "matrix" returned by makeCacheMatrix
# above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve
# should retrieve the inverse from the cache.
#
# Usage: cacheSolve(cm), where cm is the returned value of makeCacheMatrix(mtx)
#

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
	inverse <- x$getInverse()
	if(!is.null(inverse)) {
		message("retrieving inverse matrix from cache")
		return(inverse)
	}
	source <- x$get()
	inverse <- solve(source, ...)
	x$setInverse(inverse)
	inverse
}

## Creates a square invertible matrix from any integer  

makeMatrix <- function(x = integer()) {
    matrix(rnorm(x*x),x)
	}
