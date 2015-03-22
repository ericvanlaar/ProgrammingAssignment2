# Matrix inversion is usually a costly computation specifically when inside a loop. Caching
# the results alleviates the need to recompute the matrix inversion each time.

# The following two functions are used to cache the inverse of a matrix.

makeCacheMatrix <- function(x = matrix()) {
        
        # assumptions:          'X' is a square invertible matrix
        # return (list):        1.) Set Matrix, 2.) Get Matrix
        #                       3.) Set Inverse, 4.) Get Inverse 
        
        inv <- NULL
        set <- function(y) {
                # The '<<-' operator is used to assign a value to an object in an 
                # environment that is different from the current environment 
                x <<- y
                inv <<- NULL
        }
        
        get <- function() x
        setinvrs <- function(inverse) inv <<- inverse
        getinvrs <- function() inv
        list(set=set, get=get, 
             setinvrs=setinvrs, getinvrs=getinvrs)
}


# 'cacheSolve' returns the inverse of the matrix. It first checks if
# the inverse has already been computed. If so, it gets the result and skips the
# computation. If not, it computes the inverse, sets the value in the cache via
# 'setinvrs' function.

# This function assumes that the matrix is always invertible.

cacheSolve <- function(x, ...) {
        
        # assumptions:          'X' Matrix is invertible
        # return:               Function returns inverse of Matrix
        
        inv <- x$getinvrs()
        
        # IF inverse is already calculated 
        if(!is.null(inv)) {
                # get values from cache and skip computation
                message("getting cached data.")
                return(inv)
        }
        
        # ELSE calculates the inverse
        data <- x$get()
        inv <- solve(data)
        
        # set value of inverse in cache with 'setinvrs' from 'makeCacheMatrix' function
        x$setinvrs(inv)
        
        inv
}

