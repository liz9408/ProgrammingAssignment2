## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function( m = matrix() ) {
  
  ## initialize inverse as null
  i <- NULL
  
  ## set the matrix
  set <- function( matrix ) {
    m <<- matrix
    i <<- NULL
  }
  
  ## get the matrix
  get <- function() {
    ## Return the matrix
    m
  }
  
  ## set the inverse of the matrix
  setinverse <- function(inverse) {
    i <<- inverse
  }
  
  ## get the inverse of the matrix
  getinverse <- function() {
    ## return the inverse
    i
  }
  
  ## return a list of the methods
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
##This function computes the inverse of the special 
##"matrix" returned by `makeCacheMatrix` above.

cacheSolve <- function(x, ...) {
        ## matrix of the inverse of 'x'
  m <- x$getInverse()
  
  ## return the inverse if its already set
  if( !is.null(m) ) {
    message("getting cached data")
    return(m)
  }
  
  ## get the matrix
  data <- x$get()
  
  ## calculate the inverse using matrix multiplication
  m <- solve(data) %*% data
  
  ## set the inverse to the object
  x$setinverse(m)
  
  ## return the matrix
  m
}
