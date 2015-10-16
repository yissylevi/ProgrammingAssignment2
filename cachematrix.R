## Using the super assignment opperator <<- we create a function that chaches the inverse value
## of a matrix into a a seperate enviornment from the current one. This allows the program
## to run faster because it is getting data from a cached enviroment rather than having 
## culculate it each time.

##  This function "makeCacheMatrix creates a special "matrix" object that can cache 
## its inverse using the super assignment opperator <<- to be retieved later.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
  }


## This function takes the special "matrix" returned by `makeCacheMatrix` and computes  
## its inverse. If the computation has already taken place so and the 
## matrix has not changed, the `cacheSolve` retrieves the inverse from the 
## cache.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}