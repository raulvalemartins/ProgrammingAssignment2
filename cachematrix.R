## Put comments here that give an overall description of what your
## functions do

## the 'makeCacheMatrix' function creates a special object as 
## that stores a matrix and cache's its inverse 

makeCacheMatrix <- function( x = matrix()) {
        
        i <- NULL
        set <- function (y) {
                x <<- y
                i <<- NULL
        }
        get <- function () x
        getInverse <- function () i
        setInverse <- function (inverse) i <<- inverse
        list(set = set, get = get, getInverse = getInverse, setInverse = setInverse)
}


## the 'cacheSolve' function calculates de inverse matrix create by the 'makeCacheMatrix'.
## Before the calculation proceds this function checks to see if the inverse has already 
## been calculated and at same time if the content of the matrix is the same. 
## If so, it gets the inverse matrix from the cache and skips 
## the computation. Otherwise, it calculates the inverse matrix of the data and 
## sets the value of the inverse matrix in the cache via the 'setInverse' function
## the variables: 'originalData' and 'originalObject' auxliates the determination if  
## the content of the matrix was not changed.

cacheSolve <- function(x, ...) { 
        
        data <- x$get()
        
        if (identical(originalData, data )) {
                m <- originalObject$getInverse()
                print("getting cached data 1")
                return(m)
        } else
        {
                m <- x$getInverse()
                if(!is.null(m) ) {
                        ##       ##message("getting cached data")
                        print("getting cached data2 ")
                        originalData <<- data
                        originalObject <<- x
                        return(m)
                }
        }
                 
        m <- solve(data, ...)
        x$setInverse(m)
        originalData <<- data
        originalObject <<- x
        m
}
