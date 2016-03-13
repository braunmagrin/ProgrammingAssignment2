## Provides a cached matrix object and a function to calculate it's inverse
## using cache

## Create a cached matrix object

makeCacheMatrix <- function(x = matrix()) {
    inverseMatrix <- NULL
    set <- function(anotherMatrix) {
        x <<- anotherMatrix
        inverseMatrix <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inverseMatrix <<- inverse
    getinverse <- function() inverseMatrix
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Solves a cached matrix, calculating it's inverse if necessary or returning
## the cached result otherwise

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse <- x$getinverse()
    if (!is.null(inverse)) {
        message('returning cached result')
        return(inverse)
    }

    inverse <- solve(x$get(), ...)
    x$setinverse(inverse)
    inverse
}
