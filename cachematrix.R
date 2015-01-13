## Put comments here that give an overall description of what your
## functions do

## These two functions "makeCacheMatrix" and "cacheSolve" will return the
## inverse of a given square matrix. If the result is already in the chache
## they do not computer the inverse but just return the chached result.
## If the inverse is not in the cache they will compute the inverse, store 
## the result in the cache, and return the result. For a matrix such as 

## > array(1:4, c(2,2))
##     [,1] [,2]
## [1,]    1    3
## [2,]    2    4

## and call the functions as

## > cacheSolve(makeCacheMatrix(array(1:4,c(2,2))))

## the result should be

##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5

## If you multiply the both inverse and original matrices
## the reslut should be

## > cacheSolve(makeCacheMatrix(array(1:4,c(2,2)))) %*% array(1:4, c(2,2)) 
##      [,1] [,2]
## [1,]    1    0
## [2,]    0    1 

## If you call cacheSolve twice with the same argument as

## > zl<-(makeCacheMatrix(z))
## > cacheSolve(zl)
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## > cacheSolve(zl)
## getting cached data
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5

## the result is the cached data.


## Write a short comment describing this function
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


## Write a short comment describing this function
## This function computes the inverse of the special "matrix" returned
## by makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then the cachesolve should retrieve
## the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getsolve()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m
}
