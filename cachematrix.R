## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##`makeCacheMatrix`: This function creates a special "matrix" object 
##that can cache its inverse.

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
  setInverse <- function(inverse) {
    i <<- inverse
  }
  
  ## get the inverse of the matrix
  getInverse <- function() {
    ## Return the inverse property
    i
  }
  
  ## return a list of the methods
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function

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
  x$setInverse(m)
  
  ## return the matrix
  m
}
