## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function


#This function will create inv and setting it up as NULL as a placeholder for future calculations
#set will defines a function that will reset the inv to NULL and assign the value of the vector to a new vector
#get will return the vector
#setinv will set the value of inverse
#getinv will get the value of inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function
# The below function will return the inverse of a provided matrix
#checking first if the inverse has bee previously calculated
# if so it will skip the step and get the value from the cached data
#otherwise it will do the calculation using setinv function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting matrix inv from cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}