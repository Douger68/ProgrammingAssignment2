##Inverse of a Matrix:
##Matrix inversion is usually a costly computation 
##and there may be some benefit to caching the inverse of a matrix rather than compute it repeatedly

makeCacheMatrix() <- function(x = matrix()) {
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
##cacheSolve is a function that gets the inverse of the matrix created above.
cacheSolve<-function(x,...){
    inv<-x$getinverse()
    if(!is.null(inv)){
        message("getting cache data")
        return(inv)
    }
    data<-x$get()
    inv<-inv(data,...)
    x$setinv(inv)
    inv
}