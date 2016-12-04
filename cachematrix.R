## makeCacheMatrix returns a object of a matrix (a special matrix)
## cacheSolve returns a matrix that is the inverse of the object returned by makeCacheMatrix

## how to use
## myObject <- makeCacheMatrix(matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2))
## invertedMatrix <- cacheSolve(myObject)


## makeCacheMatrix is an object constructor. It receives a matrix as argument

makeCacheMatrix <- function(x = matrix()) {
    
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverted <- function(inverted) i <<- inverted
    getinverted <- function() i
    list ( set=set, get=get, setinverted=setinverted, getinverted=getinverted)
    
}


## CacheSolve returns an inverted matrix from the object sent as argument

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverted()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data,...)
    x$setinverted(i)
    i
}
