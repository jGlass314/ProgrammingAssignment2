## Put comments here that give an overall description of what your
## functions do

## Provides an interfact to the user to set the value of a matrix and
## the inverse of that matrix, and to get the value of a matrix and
## the inverse of that matrix

makeCacheMatrix <- function(matrix = matrix()) {
    m_inv <- NULL
    setMatrix <- function(y) {
        matrix <<- y
        m_inv <<- NULL
    }
    getMatrix <- function() matrix
    setInv <- function(inv) m_inv <<- inv
    getInv <- function() m_inv
    list(set = setMatrix, get = getMatrix,
         setInverse = setInv,
         getInverse = getInv)
}


## This takes a matrix object created by makeCacheMatrix(x=matrix())
## and computes its inverse. (For the sake of this exercise, we assume
## x is invertable.) It also caches the calculated inverse for later
## retrieval.
cacheSolve <- function(x, ...) {
    m_inv <- x$getInverse()
    if(!is.null(m_inv)) {
        message("getting cached data")
        return(m_inv)
    }
    data <- x$get()
    m_inv <- solve(data, ...)
    x$setInv(m_inv)
    m_inv
}
