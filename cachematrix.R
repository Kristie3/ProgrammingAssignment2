## The functions makeCacheMatrix and cacheSolve will calculate the
## inverse of a matrix and cache it for further use.

## makeCacheMatrix creates a list of 4 functions that will:
## 1) set the value of a matrix
## 2) get the value of a matrix
## 3) set the inverse of a matrix, and
## 3) get the inverse of a matrix.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
       x <<- y
       inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## cacheSolve calculates the inverse of a matrix, first
## checking if the inverse has been calculated. If it has been calculated,
## it gets and returns the cached inverse.  Otherwise, it calculates the
## inverse and caches the inverse via the setinv function and then returns it.

cacheSolve <- function(x, ...) {
    inv<- x$getinv()
    if(!is.null(inv)) {
         message("getting cached data")
         return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinv(inv)
    inv
  }

