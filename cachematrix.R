## x is a matrix supposed to be invertible
## makeCacheMatrix is a function that returns a list of four functions : 
##      get : to read the content of x
##      set : to set the content of x
##      setinverse : to set the inverse of x
##      getinverse : to get the inverse of x

## Use '<<-' in order to store m in an environment at a higher level
## than the current environment (cache)

## makeCacheMatrix is designed to store the inverse à x in the 'cache'

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


## cacheSolve is a function that returns the inverse of 'x'
## If the f the inverse has already been calculated previously
## it gets it from the cache and skips the computation

cacheSolve <- function(x=matrix()) {
        m <- x$getinverse()

## If the f the inverse has already been calculated previously
## it gets it from the cache and skips the calculation
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }

## Use the functions 'get' and 'setinverse' defined in the makeCacheMatrix function
## the function solve return the inverse of a matrix
        y <- x$get()
        m <- solve(y)

## It's here that the inverse of x is stored in the cache on first calculation
        x$setinverse(m)
        m
}
