## The following functions present a solution that cache the inverse of a matrix

## The makeCacheMatrix creates a matrix object with four functions
## 1 - sets the value of the matrix
## 2 - gets the value of the matrix
## 3 - sets the inverse value of the matrix
## 4 - gets the inverse value of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## The cacheSolve function returns the inverse value of the matrix
## It first checks to see if the inverse value has already been calculated
## If it is then it provides the value that was previously stored
## If not then it calculates and returns the inverse value and stores it

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}