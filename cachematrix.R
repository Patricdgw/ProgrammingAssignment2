## The functions below will compute the inverse of a matrix and cache the inv for future use.


## The makeCacheMatrix function is called to by cacheSolve to see if there is a cached inverse 
## for the matrix x, if there is an inverse cached for that matrix, the function will return 
## the inverse to cachesolve. If there is no inverse cached, cacheSolve will call the setinv
## function within makeCacheMatric to cache the inv of matrix x

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(inv) m <<- inv
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## cacheSolve will call makeCacheMatrix (using getinv) to see if it has a cached inverse for 
## the matrix x, if there is an inverse for x, cacheSolve will return the inverse. If there is 
## no cached inverse, cacheSolve will compute the inverse and cache it by calling setinv and  
## then returns the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- ginv(data, ...)
        x$setinv(m)
        m
}

