## these 2 functions work together to cache and calculate
##the inverse of a matrix


## this function is able to create a matrix that can cache
## its inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
  x <<- y
  i <<- NULL
}
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse )
}

## this function calculates the inverse of the matrix returned
## by the above function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data) %*% data()
  x$setinverse(i)
  i
}





