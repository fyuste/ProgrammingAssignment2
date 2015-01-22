## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        # Set the matrix
        inv <- NULL 
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        # Get the matrix
        get <- function() x
        # Set the inverse of the matrix
        setinverse <- function(inverse) inv <<- inverse
        # Get the inverse of the matrix
        getinverse <- function() inv
        # Return the matrix
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above
cacheSolve <- function(x, ...) {
        # Get the inverse of the matrix 
        inv <- x$getinverse()
        # If the inverse is already calculated, it is returned
        if(!is.null(inv)) {
                message("getting cached data.")
                return(inv)
        }
        # The inverse is not yet calculated, so it is calculated 
        data <- x$get()
        inv <- solve(data, ...)
        # Set the inverse of the matrix
        x$setinverse(inv)
        # Return the matrix
        inv
}
