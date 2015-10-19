## Following are two functions used to cache the inverse of a matrix.

## makeCacheMatrix creates a list containing a function to 
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        m_inverse <- NULL
        set <- function(y) {
                x <<- y
                m_inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m_inverse <<- inverse
        getinverse <- function() m_inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The following function calculates the inverse of the matrix created with
## the above function. It first checks to see if the inverse has already been
## calculated. if so, it gets the inverse from the cache and skips the computation.
## Otherwise, it calculates the inverse of the data and sets the value of the 
## inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        m_inverse <- x$getinverse()
        if(!is.null(m_inverse)) {
                message("getting cached data")
                return(m_inverse)
        }
        data <- x$get()
        m_inverse <- solve(data, ...)
        x$setinverse(m_inverse)
        m_inverse
}
