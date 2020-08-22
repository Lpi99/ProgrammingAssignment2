## makeCacheMatrix returns a list of functions, given a matrix
## cacheSolve returns the inverse of an invertible matrix, given the list
## from makeCacheMatrix

## 'set' sets the value of the matrix
## 'get' gets the value of the matrix
## 'setinverse' sets the value of the inverse matrix
## 'getinverse' gets the value of the inverse matrix
## the above functions are then returned in a list

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## if m, the inverse matrix, has already been calculated
## a message is written out and m is returned without recalculation
## otherwise, the matrix is stored in 'data'
## then the inverse is computed using solve()

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data,...)
  x$setinverse(m)
  m
}