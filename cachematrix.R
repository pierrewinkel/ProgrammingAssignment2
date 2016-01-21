## makeCacheMatrix is a function that creates a matrix object ('x') (which is supposed to be invetible)
## that can be used four functions : 
##      get : to read the content of x
##      set : to set the content of x
##      setinverse : to set the inverse of x
##      getinverse : to get the inverse of x
## 

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
