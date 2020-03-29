## Assignment: Caching the Inverse of a Matrixless 
#Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than compute it repeatedly.

## This function creates a special "matrix" object that can cache its inverse.
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


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

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
