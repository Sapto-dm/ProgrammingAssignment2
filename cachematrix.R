## Consist of two function named makeCacheMatrix and cacheSolve
## 

## Function accepts a matrix as a input, Returns a list with four functions

makeCacheMatrix <- function(x = matrix()) {
     
        inx <- NULL
        set <- function(y) {
                x <<- y
                inx <<- NULL
        }
        get <- function() {x}
        setinv <- function(inv) inx <<- inv
        getinv <- function() {inx}
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)


}


## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
        
        inx<- x$getinv()
        # Checks if the object is exisiting in cache
        if(!is.null(inx)) {
                message("getting cached data")
                return(inx)
        }
        data <- x$get()
        inx <- solve(data)
        x$setinv(inx)
        inx
}

# Testing x<-matrix(c(1:4),ncol=2) z<-makeCacheMatrix(x) k<-cacheSolve(z)