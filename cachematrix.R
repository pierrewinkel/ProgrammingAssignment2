## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x=matrix()){
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
                }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list (set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Return a matrix which is the inverse of 'x'

cacheSolve <- function(x=matrix()) {
        m <- x$getinverse()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        y <- x$get()
        m <- solve(y)
        x$setinverse(m)
        m
}
