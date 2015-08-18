## Matrix inversion is usually a costly computation and there may be some
## benefit to caching the inverse of a matrix rather than computing it
## repeatedly

## This function creates a special "matrix" object that can cache its inverse
##
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmatrix <- function(mtrix) m <<- mtrix
        getmatrix <- function() m
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)
}


## This function computes the inverse of the special "matrix" returned by `makeCacheMatrix` above. 
## If the inverse has already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache
##
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getmatrix()
        if(!is.null(m)) {
                message("getting cached matrix")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setmatrix(m)
        m
}

## For debugging purposes:
## create matrix:
# x <- c(2:5)              # create a vector
# x <- matrix(x,2,2)       # create matrix x 2x2 for test
## test the code
# y <- makeCacheMatrix(x)  # converts x to special matrix object
# cacheSolve(y)            # should inverse matrix, save to cache and display result
# cacheSolve(y)            # should output 'getting cached matrix' and display the cached matrix


