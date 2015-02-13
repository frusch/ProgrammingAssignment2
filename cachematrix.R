## This functions caches the inverse of a matrix


makeCacheMatrix <- function(x = numeric()) {
        ## The function returns a list of functions for operations with a matrix
        invmatrix <- NULL
        set <- function(y) {  ## sets new matrix and clean cached inverse of a matrix
                x <<- y
                invmatrix <<- NULL
        }
        get <- function() x  ## returns the matrix
        setsolve <- function(solve) invmatrix <<- solve  ## sets the inverse of a matrix
        getsolve <- function() invmatrix  ## returns the inverse of a matrix 
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

cacheSolve <- function(x, ...) {
        ## The function returns the inverse of a matrix
        invmatrix <- x$getsolve()
        if(!is.null(invmatrix)) {  ## returns the inverse of a matrix if it was cached
                message("getting cached data")
                return(invmatrix)
        }
        ## if the inverse of a matrix wasn't cached:
        data <- x$get()  ## gets a matrix
        invmatrix <- solve(data, ...)  ## calculates the inverse of a matrix
        x$setsolve(invmatrix)  ## sets the inverse of a matrix
        invmatrix
}