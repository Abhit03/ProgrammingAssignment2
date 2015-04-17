## pair of functions that check the cache and return the inverse of a matrix 
## from cache if available in cache or inverts the matrix and stores it in 
## cache for future use.


## makeCacheMatrix: This function that takes a matrix as its argument creates 
## a special "matrix" object that can cache its inverse. 
## It returns a list of functions as its objects
makeCacheMatrix <- function(x = matrix()) {
        nr <- nrow(x)
        m <- matrix(nrow = nr, ncol = nr)    ##creates empty matrix
        
        set <- function(y = matrix()) {
                x <<- y
                nr2 <- nrow(y) 
                m <<- matrix(nrow = nr2, ncol = nr2)
        }

        get <- function() x
        getinverse <- function() m
        setinverse <- function(inverse) m <<- inverse
        
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## cacheSolve: This function takes the list returned by makeCacheMatrix above and
## computes the inverse of the special "matrix".If the inverse has already been 
## calculated and the matrix has not changed the cachesolve retrieves the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()                ##takes the matrix from cache
        if(!all(is.na(m))) {               ##checks if inverse contains values 
          message("getting cached data")   ##and is not an empty matrix
          return(m)
        }
        
	  data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)      ##stores the inverse in cache for future use
        m
}