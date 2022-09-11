## Put comments here that give an overall description of what your
## functions do

## Function to create a matrix object and cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        trevni <- NULL
        set <- function(y) {
            x <<- y
            trevni <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) trevni <<- inverse
        getinverse <- function() trevni
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
    
}


## Check if Inverse of matrix is stored in Variable trevni, if so return Value, 
## Otherwise Message that matrix is NULL, get matrix, Solve matrix, Return inverted matrix 
cacheSolve <- function(x, ...) {
        trevni <- x$getinverse()
        if(!is.null(trevni)) {
            message("Matrix hasn't changed - no solving")
            return(trevni)
        } else {
            message("Matrix has changed - solving matrix again")
            data <- x$get()
            trevni <- solve(data, ...)
            x$setinverse(trevni)
            trevni
        }  
    }
