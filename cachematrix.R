## The functions makeCacheMatrix and cacheSolve are made to create and compute the cached inverse matrix.

## The function makeCacheMatrix creates a matrix that can cache the inverse of this matrix.
              
makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(u){
                x <<- u
                inverse <<- NULL
        }
        get <- function() u
        setInverse <- function(solve) inverse <<- solve
        getInverse <- function() inverse
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## The function cacheSolve computes the inverse of the matrix given as output from the function makeCacheMatrix.

cacheSolve <- function(x, ...) {
        i <- x$getInverse()
        if (!is.null(i)) {
                 message("getting cached data")
                 return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setInverse(i)
        i
}
