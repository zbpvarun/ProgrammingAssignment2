## These two functions allow the user to retrieve the inverse of a matrix from a 
## cached value if it has already been computed and the matrix has not changed.
## If the matrix changes, then the inverse is computed again and the values of the
## matrix and inverse are updated in the cache. Similar to the example provided, this
## needs to be implemented over two functions as below.

## makeCacheMatrix is the function that returns a list that allows us to either 
## retrieve values from the cache if they exist or set new ones if the values need
## to be updated. It creates dummy variables to store the matrix and the inverse
## and defines 4 methods to store or retrieve the cached values of both the matrix
## and inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(i) inv <<- i
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve is the function that can actually compute the inverse and store values 
## using the infrastructure built by the function above. It will call on the functions
## defined within makeCacheMatrix to either set or retrieve the stored values.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
