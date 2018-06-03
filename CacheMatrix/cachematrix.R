
## the makeCacheMatrix creates the object that caches the inverted matrix for the given output

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<-y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(solve) inv <<- solve
    getinv <- function() inv
    list (set = set, get = get, setinv = setinv, getinv = getinv)
}


## the below function retrieves the calculation for the inverted matrix from Cache (based on makeCacheMatrix) or, if there wasn't
## inverted matrix for the given matrix, calculates the inverted matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}

