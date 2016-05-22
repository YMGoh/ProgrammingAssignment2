# Programming assignment for Lexical Scoping : Solution as follows 
#
# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly. The
# following two functions makeCachceMatrix() and cacheSolve() are used to cache the inverse of a matrix.

# makeCacheMatrix creates a list containing a function to
# Firstly, set the value of the matrix
# Secondly, get the value of the matrix
# Thirdsly, set the value of inverse of the matrix
# Finally, get the value of inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

# The cacheSolve() function returns the inverse of the matrix. 
# This function assumes that the matrix is always invertible. It first checks if
# the inverse has already been computed. If so, it gets the result and skips the
# computation. If not, it computes the inverse, sets the value in the cache via
# setinverse function.

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data.")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv

# Functions can be run as follows :
# x = rbind(c(1, -1/2), c(-1/2, 1))
# m = makeCacheMatrix(x)
# m$get()
# Will result in the following output
#    [,1] [,2]
# [1,]  1.0 -0.5
# [2,] -0.5  1.0

# No cache in the first run
# cacheSolve(m)

# Retrieving from the cache in the second run
# > cacheSolve(m)
# getting cached data. Will result in the following output
#       [,1]      [,2]
# [1,] 1.3333333 0.6666667
# [2,] 0.6666667 1.3333333
