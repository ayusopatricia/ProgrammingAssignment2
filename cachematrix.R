## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  ## Initializing inverse variable
  i <- NULL
  
  ## Setting the value of the matrix
  set <- function(m) {
    x <<- m
    i <<- NULL
  }
  
  ## Getting the value of the matrix
  get <- function() x
  
  ## Setting the value of the inverse result
  setInv <- function(y) i <<- y
  
  ## Getting the value of the inverse result
  getInv <- function() i
  
  ## listing all the methods
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  i <- x$getInv()
  
  ## If is in cache... Return a matrix that is the inverse of 'x'
  if(!is.null(i)) {
    return(i)
  }
  
  ## Else... let's get the matrix
  m <- x$get()
  
  ## and get the inverse
  y <- solve(m)
  
  ## Setting the inverse in cache
  x$setInv(y)
  
  ## Return a matrix that is the inverse of 'x'
  y
}
