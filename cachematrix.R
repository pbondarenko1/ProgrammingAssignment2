## Two functions that can cache the inverse of a matrix

## This function creates a special matrix object that can cache its inverse. The function sets the matrix, gets the matrix,
## sets the inverse of the matrix, and gets the inverse of the matrix.
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(matrix) {
        x <<- matrix
        i <<- NULL
    }
    get <- function() {x}
    setInverse <- function(inverse) {
        i <<- inverse
    }
    getInverse <- function() {i}
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## The following function calculates the inverse of the special matrix created with the above function. 
## However, it first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache.
cacheSolve <- function(x, ...) {
    x <- x$getInverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    x <- solve(data) %*% data
    x$setInverse(x)
    x
}
