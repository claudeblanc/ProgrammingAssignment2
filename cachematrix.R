makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inv <<- solve
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

cacheSolve <- function(x, ...) { # the input x is an object created by makeCacheMatrix
  ## Return a matrix that is the inverse of 'x'
  
    inv <- x$getinverse()               # accesses the object 'x' and gets the value of the inverse
    if(!is.null(inv)) {                 # if inv was already cached (not NULL) ...
      
      message("getting cached data")    # ... send this message to the console
      return(inv)                       # ... and return the inverse ... "return" ends 

    }
    mat <- x$get()                     # we reach this code only if x$getinverse() returned NULL
    inv <- solve(mat, ...)              # if inv was NULL then we have to calculate the inverse
    x$setinverse(inv)                   # store the calculated inverse in x
    inv                                 # return inv to the code that called this function 
}