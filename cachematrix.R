## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix function set and get the value of the matrix provided

makeCacheMatrix <- function(x = matrix()) {
 
  im <- NULL

  setmatrix <- function(i) {
    x <<- i
    im <<- NULL    
  }
  getmatrix <- function() x
  setinverse <- function(solve) im <<- solve
  getinverse <- function() im

  list(setmatrix = setmatrix, getmatrix = getmatrix, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inversematrix <- x$getinverse()
  if(!is.null(inversematrix)) {
    message("getting cached data")
    return(inversematrix)
  }
  data <- x$getmatrix()
  inversematrix <- solve(data, ...)
  x$setinverse(inversematrix)
  inversematrix
}
