# Returns the reverse of a matrix, caching the results
# Example usage: 
# matrixTest <- matrix(c(9,2,3,3,3,3,3,2,9),3,3)
# cacheSolve(makeCacheMatrix(matrixTest))

# Caches inversed matrix
makeCacheMatrix <- function(x = matrix()) {
		m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmatrix <- function(matrix) m <<- matrix
        getmatrix <- function() m
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)
}

# This caches the inverse matrix or returns cached value if already cached
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getmatrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setmatrix(m)
        m
}
