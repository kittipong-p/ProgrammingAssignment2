## Caching the Inverse of a Matrix 

## Function to create special matrix that can cache its own inverse

makeCacheMatrix <- function(m = matrix()) {
         inv <- NULL
         set <- function(n) {
                 m <<- n
                 inv <<- NULL
         }
         get <- function() m
         setInverse <- function(inverse) inv <<- inverse
         getInverse <- function() inv
         list(set = set,
              get = get,
              setInverse = setInverse,
              getInverse = getInverse)
 }


## Function to determine inverse of the matrix created in the above function

cacheSolve <- function(m, ...) {
         ## Return an inverse matrix of 'm'
         if (!is.null(m$getInverse())) {
                 message("getting cached data")
                 return(m$getInverse())
         }
         p <- m$get()
         m_inv <- solve(p, ...)
         m$setInverse(m_inv)
         m_inv
 }
