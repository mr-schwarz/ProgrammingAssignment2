## Matrix inversion is usually a costly computation and there is some benefit
## to caching the inverse of a matrix rather than compute it repeatedly.
## Function makeCacheMatrix creates a special "matrix" object that can cache
## its inverse.
## Function cacheSolve computes the inverse of the special "matrix" returned
## by makeCacheMatrix. If the inverse has already been calculated (and the
## matrix has not changed), then cacheSolve retrieve the inverse from the cache.


## Function makeCacheMatrix creates a special "matrix", which is really a
## list containing a function to
## 1. set the value of the matrix (set)
## 2. get the value of the matrix (get)
## 3. set the inverse of the matrix (setinverse)
## 4. get the inverse of the matrix (getinverse)
##
## Example:
## x <- matrix(c(2, 1, 5, 3), nrow = 2, ncol = 2)
## m <- makeCacheMatrix(x)
makeCacheMatrix <- function(x = matrix()) {
        inver <- NULL
        set <- function(y) {
                x <<- y
                inver <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inver <<- inverse
        getinverse <- function() inver
        list(set = set
             ,get = get
             ,setinverse = setinverse
             ,getinverse = getinverse
             )
}


## Function cacheSolve calculates the inverse of the special "matrix" created
## with function makeCacheMatrix. It first checks to see if the inverse has already 
## been calculated. If so, it gets the inverse from the cache and skips the computation.
## Otherwise, it calculates the inverse of the data and sets the value of the inverse in
## the cache via the setinverse function.
##
## Example:
## x <- matrix(c(2, 1, 5, 3), nrow = 2, ncol = 2)
## m <- makeCacheMatrix(x)
## cacheSolve(m)
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inver <- x$getinverse()
        if(!is.null(inver)) {
                message("Getting cached matrix")
                return(inver)
        }
        data <- x$get()
        inver <- solve(data)
        x$setinverse(inver)
        inver
}
