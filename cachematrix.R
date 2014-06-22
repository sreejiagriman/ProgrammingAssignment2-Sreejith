## makeCacheMatrix: Creates a special "matrix" object that can cache its inverse.
## cacheSolve: This function computes the inverse of the special "matrix" returned 
 ## by makeCacheMatrix above.
## However, if the matrix does not change and the inverse has already been 
 ## calculated, then it retrieves the value from the cache
  ## By doing so, computer reources are saved
   ## makeCacheMatrix: Creates a special "matrix" object
  ## that can cache its inverse.
  
 makeCacheMatrix <- function(x = matrix()) {
    +   m <- NULL                                       ##initializing as NULL
    +   set <- function(y)  {                           ##defining the set function
      +     x <<- y
      +     m <<- NULL
      +   }
    +   get <- function() x                             ##returns x
    +   setinverse <- function(solve) m <<- solve       ##solve used to calculate inverse
    +   getinverse <- function() m                      ##returns m
    +   list(set = set, get = get,                      ##list
             +        setinverse = setinverse,
             +        getinverse = getinverse)
    + }

   
 ##The following function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
 ##If the inverse has already been calculated (and the matrix has not changed), 
  ##then the cachesolve should retrieve the inverse from the cache.
  
 cacheSolve <- function(x, ...) {
    +   
      +   m <- x$getinverse()
    +   if(!is.null(m)) {             ##Checking whether inverse already calculated or not
      +     message("getting cached data")
      +     return(m)
      +   }
    +   data <- x$get()
    +   m <- solve(data, ...)
    +   x$setinverse(m)
    +   m                             ## Returns a matrix that is the inverse of 'x'
    + }
