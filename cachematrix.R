## This function caches the inverse of a matrix
## 

##The basic philosophy behind this function is same as the
##example given. First, it sets the value of the matrix
##gets the value of the matrix, then, sets the value of
##inverse matrix, and finally gets the value of the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
  I <- NULL
  set <- function(y) {
    x <<- y
    I <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) I <<- inverse
  getInverse <- function() I
  list(set = set, get = get, setInverse = setInverse,
       getInverse = getInverse)
}


## As the function above, this function also has
##the same background as the example we read in the course page.
##This function calculates the inverse of the matrix,
##but checks the existence of inverse matrix first.
##If there's already one calculated from the function above,
##It gets from the cache and skips the computation, while
##it calculates the inverse in the other case.

cacheSolve <- function(x, ...) {
        I <- x$getInverse()
        if(!is.null(I)) {
          message("getting cached data")
          return(I)
        }
        matrix <- x$get()
        I <- solve(matrix, ...)
        x$setInverse(I)
        I
}
