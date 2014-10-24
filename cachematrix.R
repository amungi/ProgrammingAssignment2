## Put comments here that give an overall description of what your
## functions do

## Function "makeCacheMatrix":
## This function creates a special "matrix" object that can cache its inverse.
## It has a list of 4 functions to:
## 1.  set the value of the matrix
## 2.  get the value of the matrix
## 3.  set the value of the inverse of the matrix
## 4.  get the value of the inverse of the matrix

## Function "cacheSolve":
## This function computes the inverse of the special "matrix" returned by
## the function "makeCacheMatrix". If the inverse has already been calculated
## and the matrix has not changed, then this function "cacheSolve"
## retrieves the inverse from the cache instead of computing it again.

## It is assumed that the square matrix supplied is always invertible


## Write a short comment describing this function

## Function "makeCacheMatrix":
## This function creates a special "matrix" object that can cache its inverse.
## It has a list of 4 functions to:
## 1.  set the value of the matrix
## 2.  get the value of the matrix
## 3.  set the value of the inverse of the matrix
## 4.  get the value of the inverse of the matrix
## It is assumed that a square matrix is supplied, and that it is always invertible.

makeCacheMatrix <- function(x = matrix()) {
    
    m <- NULL
    
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    get <- function() x
    
    setinvmatrix <- function(solve) m <<- solve
    
    getinvmatrix <- function() m
    
    list (set = set, get = get,
          setinvmatrix = setinvmatrix,
          getinvmatrix = getinvmatrix)
}


## Write a short comment describing this function

## Function "cacheSolve":
## This function computes the inverse of the special "matrix" returned by
## the function "makeCacheMatrix". If the inverse has already been calculated
## and the matrix has not changed, then this function "cacheSolve"
## retrieves the inverse from the cache instead of computing it again.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    m <- x$getinvmatrix()
    
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    
    matrix <- x$get()
    
    m <- solve(matrix, ...)
    
    x$setinvmatrix(m)
    
    m
    
}
