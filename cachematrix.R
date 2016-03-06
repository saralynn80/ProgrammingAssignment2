## A pair of functions that cache the inverse of a matrix

## Creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        m = NULL
        set = function(y) {
                x <<- y
                m <<- NULL
        }
        get = function() x ## Fx returning vector x stored in main fx
        setinverse = function(solve) m <<- solve
        getinverse = function() m
        list(set = set, get = get, 
             setinverse = setinverse,
             getinverse = getinverse)
}

## Computes the inverse of the special matrix returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
  
        ## Return a matrix that is the inverse of 'x'
        m = x$getinverse()
        if (!is.null(m)){
          message("getting cached data")
          return(m)
        }
        ## Runs if the inverse isn't yet calculated
        my_matrix = x$get()  ## Gets the matrix that was stored with makeCacheMatrix
        m = solve(my_matrix, ...) ## Calculates the inverse
        x$setinverse(m) ## Stores inverse of the matrix in object m
        m
}
