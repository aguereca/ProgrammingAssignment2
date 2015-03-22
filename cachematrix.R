#
# Test case:
#
# # Test matrix
# origin <- matrix(c(2,4,7,1,6,2,9,7,6),3,3)
# source <- makeCacheMatrix(origin)
# # Initial operation, no cache
# cacheSolve(source)
# # use cache on subsecuent attepts
# cacheSolve(source)
#


makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    # cache Matrix with empty inverse
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


cacheSolve <- function(x, ...) {
    m <- x$getsolve()
    # prove cache
    if(!is.null(m)) {
        message("Getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    # warm cache of inverse
    x$setsolve(m)
    print(m)
}


