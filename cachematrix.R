## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a vector of 4 functions to:
## 1. set value of matrix
## 2. get value of matrix
## 3. set inversion of matrix
## 4. get inversion of matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(solve) inv <<- solve
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)

}


## Write a short comment describing this function
## this function checks if the inversion of the matrix already exists, if so it returns the calculated matrix
## if it doesn't exist, it calculates inversion matrix and returns it.
                
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
