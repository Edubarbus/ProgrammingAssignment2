## Functions allow to compute the inverse of a matrix and then cache the results. If the inverse of a matrix was already calculated, the result from cache will be returned.
## Functions assume that the matrix is invertible.
## I apologize if the explanation is not clear. English is not my first language. 

## Function initialize objects x and s. It allows the possible extration of the inverse s if it exists.
## Last, it creates a new object that contains the cache.

makeCacheMatrix <- function(x = matrix()) {             ##initialization of x
        s <- NULL                                       ##initialization of s
        set <- function(y) {
                x <<- y                                 ##repeating initialization of x 
                s <<- NULL                              ##and m
        }
        get <- function() x                             ##use of lexical scoping
        setsolve <- function(solve) s <<- solve         ##inverse s
        getsolve <- function() s                        ##use of lexical scoping to retrieve inverse s
        list(set = set, get = get,                      ##creation of a new object
             setsolve = setsolve,
             getsolve = getsolve)
}


## It return the inverse of the matrix x. If the matrix x was calculated before, the result must be saved in cache. In that case, the function will return 
## the value from cache, omiting the calculation. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s <- x$getsolve()
        if(!is.null(s)) {                               ##It search for a possible cache value
                message("getting cached data")          ##If there is a cache value
                return(s)                               ##Omits the calculations and shows the cache value 
        }
        data <- x$get()                                 ##If  there is no cache value for the matrix,
        s <- solve(data, ...)                           ##... the inverse is calculated
        x$setsolve(s)
        s
}
