# function that catch the inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
  # error if x insnt a matrix
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



cacheSolve <- function(x, ...) {
  ## return matrix inverse of x
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("cached matrix inverse")
    return(inv)
  }
  
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
