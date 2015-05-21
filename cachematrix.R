## Put comments here that give an overall description of what your
## functions do

## The makeCacheMatrix creates a matrix containing respective values. It then creates the 
## inverse of it.

makeCacheMatrix <- function(x = matrix()) {

        inv <- NULL
        set <- function(y) {
                
                x <<- y
                inv <<- NULL
        }
get <- function() x
setsolve <- function(solve) inv <<- solve
getsolve <- function() inv
list(set = set, get = get,
     setsolve = setsolve,
     getsolve=getsolve)
}


## The function cacheSolve checks whether the the inverse is already stored in the cache. In
## case it is the message "getting cached data" as well as the inverse is returned,
## otherwise it is calculated.

cacheSolve <- function(x, ...) {
        inv <- x$getsolve()
        if(!is.null(inv)) {
                message("getting cached data")
                return inv
        }
        data <- x$get()
        inv <- solve(data,...)
        x$setsolve(inv)
        inv
        ## Return a matrix that is the inverse of 'x'
}
