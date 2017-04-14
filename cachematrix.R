## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates a object that has the following attributes:
## getMat: obtain the matrix in the object
## setMat: Initialize the values in the matrix
## setInverse: intialize the inverase of the matrix
## getInverse: obtain the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  
  InvMatrix <- NULL
  setMat <- function(mat) {
    x <<- mat
    Inv <<- NULL
    
  }
    
  getMat <- function() x
  
  setInverse <- function(inv) InvMatrix <<- inv
  getInverse <- function() InvMatrix  
  
  list(setMat = setMat, getMat= getMat, setInverse= setInverse, getInverse = getInverse)

}


## cacheSolve: A fast way to compute the inverse of a matrix
## this function compute the inverse of a matrix store in a list
## generated using makeCacheMatrix. If the inverse of the matrix is
## already computeed, the inverser will be retrieve else it will be computed.

cacheSolve <- function(x, ...) {
  
  inv <- x$getInverse()
  if (!is.null(inv)){
    return(inv)
  }
  newMat <- x$getMat()
  inv <- solve(newMat)
  x$setInverse(inv)
  inv
  
}
