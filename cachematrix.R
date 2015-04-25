## The following functions will create a special type of matrix that can be stored in a smart way so that the inverse of this matrix can be stored and cached for later use. The second function will compute the inverse of the matrix if it is not in memory and will store it.


## This function creates a special matrix that uses the << operator. It will allow to cache the matrix inverse for later use.

makeCacheMatrix <- function(x = matrix()) {
    myinv <- NULL
    set <- function(y) {
        x <<- y
        myinv <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) myinv <<- solve
    getinverse <- function() myinv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## THis function will check first if the inverse of the matrix is cached and if it is not, it will compute it. THis function assumes that the matrix passed to it is invertible.

cacheSolve <- function(x, ...) {
    myinv <- x$getinverse()
    if(!is.null(myinv)) {
        message("getting cached data")
        return(myinv)
    }
    data <- x$get()
    myinv <- solve(data, ...)
    x$setinverse(myinv)
    myinv
}
