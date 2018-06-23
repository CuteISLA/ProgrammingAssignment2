## This function creates a special "matrix" object
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y = matrix()){
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinv <- function(inv) i <<- inv
        getinv <- function() i
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed)
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(cache_list, ...) {
        inv <- cache_list$getinv()
        if(!is.null(inv)){
                message("getting cached data!")
                return(inv)
        }
        data <- cache_list$get()
        inv <- solve(data)
        cache_list$setinv(inv)
        inv
}
