## This set of functions calculates the inverse of a matrix and stores the value in cache
## This saves time by avoiding the need to calculate the partial inverse every time

## This function creates a matrix with the ability to get and set its value and get and set its inverse
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    setx <- function(y) {
        inverse <<- NULL
        x <<- y
    }
    
    getx <-function() {
        x
    }
    
    setInverse <- function(inverse) {
        x <<- inverse
    }
    
    getInverse <- function() {
        inverse
        
    }
    
    list(set = setx, get = getx, setInverse = setInverse, getInverse = setInverse)
}


## This function calculates the inverse of the matrix passed in unless its already in cache

cacheSolve <- function(x, ...) {
    inverse <- x$getInverse()
    if(!is.null(inverse)) {
        return(inverse)
    }
    
    temp <- x$get()
    inverse <- solve(temp, ...)
    x$setInverse(inverse)
    inverse
}
