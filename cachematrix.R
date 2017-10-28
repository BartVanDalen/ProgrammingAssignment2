## These two routines resp. create a matrix object with its inverse stored within it. At first, the inverse is a NULL object
## The second function takes the stored inverse. If NULL, then it calculates the inverse, and stores it in the cache.

## Creates a matrix object, with the possibility to store the inverse

makeCacheMatrix <- function(x = matrix()) {
        matr <- NULL
        set <- function(y) {
                matr <<- y
                cacheInverse <<- NULL
        }
        get <- function() matr
        setInv <- function(inverse) cacheInverse <<- inverse
        getInv <- function() cacheInverse
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
}



## Attempts to read the inverse of a matrix object from its cache. 
## If unsuccessfull, it will calculate it by itself, and store it in the cache of the original matrix object

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		inv <- x$getInv()
        if(!is.null(inv)) {
                message("getting cached inverse")
                return(inv)
        }
        matr <- x$get()
        inv <- solve(matr, ...)
        x$setInv(inv)
        inv
}
