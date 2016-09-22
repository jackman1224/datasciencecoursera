## This function caches the inverse of a matrix. This function 
## creates a 'special object' that caches its inverse.

## makeCachematrix is a function that creates a 'special object' that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y){
        x <<- y
        m <<- NULL
    }
    get <- function()x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list (set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function creates the inverse of the 'special object' matrix above called makeCachematrix

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!isnull(m)){
            message("getting cached data")
            return(m)
        }
        data <- x$get()
        m <- solve (data,...)
        x$setinverse(m)
        m
}
