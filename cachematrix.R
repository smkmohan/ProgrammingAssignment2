# Coursera R programming 
# Programming assigment week 3

# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly. The
# following two functions are used to cache the inverse of a matrix.

# The first folowing function creates the list of 4 functions: [1] set the 
# value of the matrix, [2] get the value of the matrix, [3] set the value of 
#inverse of the matrix and [4] get the value of inverse of the matrix.

# Setting the input data
matice <- matrix(rnorm(9),3,3)
matice

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinv <- function(inv) i <<- inv
        getinv <- function() i
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

# Following funcion takes the matrix stored by the previous function and then
# returns its inverse. If the inverse is already computed, the function uses
# the cached data, so it spares some time.
cacheSolve  <- function(x, ...) {
        i <- x$getinv()
        if(!is.null(i)) {
                message("Getting cached data.")
                return(i)
        }
        data <- x$get()
        i <- solve(data)
        x$setinv(i)
        i
}

