## makeCacheMatrix creates an object containing a matrix and getter/setter functions.
## cacheSolve can solve such an object, returning the solution from cache if possible
##
## USAGE EXAMPLE: 
##  c=rbind(c(4, 3), c(3, 2)) #this is the matrix to solve for
##  solve(c) #this is the expected solution
##  ensure both functions are in memory
##  item<-makeCacheMatrix(c) # create the object
##  cacheSolve(item) # the first time it calculates the value
##  cacheSolve(item) # the second and all further times the same operation returns the value from cache




## Creates an object allowing for the caching of inverse calculations on a matrix
## thus saving time on repeated calculations with the same matrix (object)

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setInverse <- function(newInverse) inverse <<- newInverse
  getInverse <- function() inverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}




## Solves the matrix for the matrix contained in the object x, 
#returning an inverse, getting the value from cache if it has already been computed

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getInverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setInverse(inverse)
  inverse
  
}