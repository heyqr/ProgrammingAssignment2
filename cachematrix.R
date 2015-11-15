## Following functions create a special object that stores a matrix and caches its inverse.

#makeCacheMatrix creates a list containing a function to
#set the value of the matrix
#get the value of the matrix
#set the value of the inverse matrix
#get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse<- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

##cacheSolve calculates the inverse of the special "matrix" created with the above function.
#It first checks if the inverse matrix has already been calculated. 
#If so, gets the inverse from the cache and skips the computation. 
#Otherwise, calculates the inverse matrix and set it to cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}
