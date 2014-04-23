## These two functions provide the oppotunity to skip unecessary calculations.
## More specifically, if the inverse of a given matrix is calculated once,
## it is stored in cache. Prospective repetitions of the same calculation 
## is simply skipped, and instead the calcualted value is retrieved.

## makeCacheMatrix create a special matrix which is list whose components are:
## set value of the matrix
## set value of the inverse
## get value of the matrix
## get value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(theMat) {
      x <<- theMat
      inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list (set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## casheSolve calcualtes and returns the inverse of a given matrix.
## if the inverse is already calculated and stored in the cache,
## it simply skips the calculation and returns the existing inverse.

cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    if(!is.null(inv)) {
        message("using cached data...")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setInverse(inv)
    inv
}

## Used for testing
## X = matrix(c(4,6,3,6,4,6,6,3,5), 3, 3)
## sX <- makeCacheMatrix(X)
## cacheSolve(sX)
## cacheSolve(sX)
