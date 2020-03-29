## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) { #setting y to x and setting inv as a global env variable as NULL.
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
        #returns a list of all the subfunctions created by the function makeCacheMatrix

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv() # if  an inverse is already present, then getting that value.
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        #if inverse value is not present then saving the inverse to the inv variable.
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
