## Compute the inverse of a matrix (assuming invertible matrix)
##  or lookup for an already computed (cached) inverse.
makeCacheMatrix <- function(x = matrix()) {
        # Initialize
        s <- NULL
        
        ## Setters
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
                
        setSolve <- function(solve) s <<- solve
        
        ## Getters        
        get <- function() x
        
        getSolve <- function() s
        
        ## Return
        list(set = set,
             get = get,
             setSolve = setSolve,
             getSolve = getSolve
        )
}

## Check if a cached inverse exists and the matrix has not been changed.
##  Otherwise, it calculates and saves to cache.
##  Finally, return its inverse.
cacheSolve <- function(x, ...) {
        s <- x$getSolve()
        
        # Check if exists a cached inverse
        if(!is.null(s)) {
                message("Getting cached data...")
                
                ## Return result
                return(s)
        }
        
        # Get data
        data <- x$get()
        
        # Compute inverse
        s <- solve(data, ...)
        
        # Cache inverse
        x$setSolve(s)

        ## Return result
        s
}