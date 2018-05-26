##Function creates a special “matrix” and provides function to
##set values,getvalues,set inverse, get inverse
makeCacheMatrix <- function(x = matrix()) {
  I <- NULL
  set <- function(y) {
    x <<- y
    I <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) {
    I <<- solve
    ## store the matrix that resulted in getting inverse
    ## so we can compare next time for equality if we can reuse old inverse or ##recalculate
    stored <<- x
  }
  getInverse <- function() I
  getStored <- function() stored
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse,
       getStored = getStored)
  
}

## Below  Function will look into cache to see if inverse exist for matrix and ##retrieve the value. 
##if its the same matrix but will calculate if no inverse exist or ##matrix is not same.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  ## Return the inverse if its already set
  if( !is.null(m) ) {
    message("getting cached data")
    return(m)
  }
  ## Get the matrix from our object
  data <- x$get()
  ## Calculate the inverse using matrix multiplication
  m <- solve(data) %*% data
  ## Set the inverse to the object
  x$setInverse(m)
  ## Return the matrix
  m      
}
