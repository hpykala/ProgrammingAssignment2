## Functions makeCacheMatrix and cacheSolve make it possible to cache 
## the inverse of a matrix to save time with computations.
## Use makeCacheMatrix to create a cachable matrix and
## cacheSolve to solve and cache its inverse.


## Create a cacheable matrix to save time inverting the matrix
makeCacheMatrix <- function(x = matrix()) {
    # cached value is set as null as it's not calculated yet
    inv <- NULL
    
    # if a new value for matrix is set, null the inverse
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    # get the value of matrix
    get <- function() x
    
    # set inverse
    setinv <- function(inverse) inv <<- inverse
    
    # get inverse
    getinv <- function() inv
    
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)

}


## Return a matrix that is inverse of x. 
##
## X must be a CacheMatrix created by makeCacheMatrix function
## If the inverse matrix has been already calculated cacheSolve returns the cached value along with message 
## "getting cached data"
cacheSolve <- function(x, ...) {

    # check if the inverse has been cached and return the cache value if it exists
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    # If there's no cached inverse get the CacheMAtrix data, solve its inverse, and store the result
    data <- x$get()
    
    # no error handling since we can assume the matrix is always invertible. 
    # If this is not the case solve will throw an error
    inv <- solve(data, ...)
    
    # save the inverse to the CacheMatrix so it can be reused
    x$setinv(inv)
    
    inv
}
