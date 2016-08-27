## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
          d<-dim(x)[1]
          inv <- matrix(1:d^2*NA,d,d)
          set <- function(y) {
            x <<- y
            inv <<- matrix(1:d^2*NA,d,d)
          }
          get <- function() x
          setinv <- function(invers) inv <<- invers
          getinv <- function() inv
          list(set = set, get = get,
               setinv = setinv,
               getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      inv <- x$getinv()
      if(!is.na(inv[1,1])) {
        message("getting cached data")
        return(inv)
      }
      data <- x$get()
      inv <- solve(data)
      x$setinv(inv)
      inv
}
