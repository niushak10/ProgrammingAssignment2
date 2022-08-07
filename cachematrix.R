## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function defines 4 setter/getter functions for the matrix and int's inverse values
makeCacheMatrix <- function(x = matrix()) {
        inverseValue <- NULL
        set <- function(y) {
                x <<- y
                inverseValue <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inverseValue <<- inverse
        getInverse <- function() inverseValue
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Write a short comment describing this function
## This function checks for the existence of inverse value, if it exists it returns it if it doesn't it computes it using solve() and returns it, it also saves it in the cache using setInverse function for future calls.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverseValue <- x$getInverse()
        if(!is.null(inverseValue)) {
                message("getting cached data")
                return(inverseValue)
        }
        data <- x$get()
        inverseValue <- solve(data, ...)
        x$setInverse(inverseValue)
        inverseValue
}