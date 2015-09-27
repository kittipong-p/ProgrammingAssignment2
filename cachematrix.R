## Caching the Inverse of a Matrix 

## Function to create special matrix that can cache its own inverse

makeCacheMatrix <- function(x = matrix()) {
         inv <- NULL
         set <- function(y) {
                 x <<- y
                 inv <<- NULL
         }
         get <- function() x
         setInverse <- function(inverse) inv <<- inverse
         getInverse <- function() inv
         list(set = set,
              get = get,
              setInverse = setInverse,
              getInverse = getInverse)
 }


## Function to determine inverse of the matrix created in the above function

cacheSolve <- function(x, ...) {
         ## Return an inverse matrix of 'x'
         if (!is.null(x$getInverse())) {
                 message("getting cached data")
                 return(x$getInverse())
         }
         z <- x$get()
         x_inv <- solve(z, ...)
         x$setInverse(x_inv)
         x_inv

