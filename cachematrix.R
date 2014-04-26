## Put comments here that give an overall description of what your
## Create a special matrix, calculate the inverse of the matrix 
## using solve(A) 
## makeCacheMatrix creates the special matrix which is a list
## cacheSolve is a function that reads the inverse from the cache
## if it's available, else calculates the inverse.
## functions do

## Write a short comment describing this function


makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinv <- function(solve) i <<- solve
        getinv <- function() i
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        i <- x$getinv()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinv(i)
        i
}
