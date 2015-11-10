## These functions provde a way to store and access a Matrix and its Inverse
## Useful for tasks where the inverse is often needed without the underlying
## Matrix changing


## This function takes an R matrix and returns a special Cachable Matrix with
## getters and setters for the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {

    i <- NULL
        
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
        
    get <- function() x
    setinverse <- function(inv) i <<- inv
    getinverse <- function() i
    list(set = set, 
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}


## This function takes a special Cachable Matrix (created by MakeCacheMatrix)
## and returns the inverse either by using the cached version, 
## or recomputing and storing it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
    i <- x$getinverse()
    
    if(!is.null(i)) {
        message("getting cached inverse data")
        return(i)
    }
    
    data <- x$get()
    i <- solve(data)
    x$setinverse(i)
    i
}
