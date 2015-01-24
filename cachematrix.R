## These functions will calculate the inverse of a matrix and both values (matrix/inverse) will be stored
## in cache. If a matrix already calculated is entered, the function will return the value already cached

## "makeCacheMatrix" will set the space in the cache for the given matrix and its inverse, and also 
## contains the corresponding "set" and "get" functions for storing values in cache or retrieving 
## results previously calculated

makeCacheMatrix <- function(x = matrix()) {
 
        inversematrix <- NULL
  
        setmatrix <- function(i) {
                x <<- i
                inversematrix <<- NULL
        }
        
        getmatrix <- function() x
        setinverse <- function(solve) inversematrix <<- solve
        getinverse <- function() inversematrix

        list(setmatrix = setmatrix, 
             getmatrix = getmatrix, 
             setinverse = setinverse, 
             getinverse = getinverse)
}


## "cacheSolve" will calculate the inverse of a given matrix. If calculation has been already performed,
## the function will return the value stored in the cache, skipping the calculation

cacheSolve <- function(x, ...) {
        
        ## Checks if the inverse of the matrix provided exists in cache. If so, gets
        ## the data. If not, calculation is performed and results are stored in the cache
                
        inversematrix <- x$getinverse()
        
        if(!is.null(inversematrix)) {
                message("Retrieving data in cache")
                return(inversematrix)
        }
        
        data <- x$getmatrix()
        inversematrix <- solve(data, ...)
        x$setinverse(inversematrix)
        inversematrix
}
