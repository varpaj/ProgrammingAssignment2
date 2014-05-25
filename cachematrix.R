## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# The makeCacheMatrix function will allow you to use the get function to retrieve the matrix
 # that is stored. Using the set function you can set the value of a matrix. Using getinverse, 
 # one can retrieve the value of inverse of a matrix and using setinverse function, one can set the
 # value of matrix inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL # assign a default value of null
  # create a function within the function to set the value of x
  set <- function(y=matrix()) {
    x <<- y
    m <<- NULL
  }
  #Define a function get to retrieve the value of x
  get <- function() x
  #Define the setinverse function to set the value of inverse of x
  setinverse <- function(inverse=matrix()) m <<- inverse
  # Retrieve the value of inverse. It will return a value if it is avaialable otherwise 
  # it will return NULL
  getinverse <- function() m
  # The function returns a list of functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
  
}


## Write a short comment describing this function
# This function will first check if inverse is already available, if so it will return the
# inverse. If currently inverse has not been calculated, i.e. if the default value is null, it will then calculate
# the inverse and return the result. It will also set the value of inverse as well so that next time
# inverse will not need to be calculated. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  # Obtain the inverse. m will be assigned NULL if inverse has not been computed already
  m <- x$getinverse()
  # If m has a value (inverse), this value will be retrieved
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  #if m is NULL, the inverse will have to be computed
  data <- x$get()
  #obtain inverse and then assign to m
  m <- solve(data, ...)
  # set the inverse value so that it could be retrieved in future
  x$setinverse(m)
  #return the inverse value
  m
}
