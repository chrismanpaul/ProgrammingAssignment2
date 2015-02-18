## These functions implement a way to cache the inverse of a matrix so that it 
## does not need to be recalculated each time it is needed

## The makeCacheMatrix function takes a matrix as input and returns a list of 
## functions which can both set and retrieve the matrix and its inverse.  The 
## function initiates the inverse as NULL.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## The function cacheSolve first retrieves the inverse from the special matrix 
## list functions (using getinverse).  It then checks if the inverse is NULL, 
## if it is not NULL, it returns the inverse that was previously calculated.  If
## NULL it calculates the inverse, stores it in the cache and then returns the 
## inverse.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
