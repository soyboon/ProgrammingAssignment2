## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## the makeCacheMatrix function creates a list of functions to
## 1. set the value of a matrix
## 2. get the value of a matrix
## 3. set the inverse of a matrix
## 4. get the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
    m = matrix()
    m <- NULL
    
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInv <- function(inv_matrix) m <<- inv_matrix
    getInv <- function() m
    
    list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## Write a short comment describing this function

## the cacheSolve function takes in a matrix and first check
## if the inverse of the matrix has been calculated before
## and  exist in the environment.  If so, it will retrieve 
## the value from the cache and return the result.
## otherwise, it will get the data from the environment, 
## solve the inverse, and store it into the environment.

cacheSolve <- function(x = matrix(), ...) {
    ## Return a matrix that is the inverse of 'x'
  
    m <- x$getInv()
    if(!is.null(m)) {
        message("Get cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInv(m)
    m
}

