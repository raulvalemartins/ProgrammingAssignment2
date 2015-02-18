## HOW TO TEST ALL THE CENARIOS

##      SECTION 1: Run the next four commands until SECTION 2

##      m1 <- matrix(data = 1:4, nrow = 2, ncol = 2)
##      m2 <- matrix(data = 5:8, nrow = 2, ncol = 2)
##      matrixCache <- makeCacheMatrix( m1 )
##      cacheSolve( matrixCache )
##      
##      Note: at this moment if you run 'cacheSolve( matrixCache )' several times
##      the inverse cached value is always used because the inverse matrix has 
##      already been calculated and at same time the matrix has not changed

##      SECTION 2: Now, run the next two commands.

##      matrixCache$setMatrix( m2 )
##      cacheSolve( matrixCache )
##
##      Note: the last call to the 'cacheSolve( matrixCache )' doesn't use 
##      the chached value of inverse matrix, because the new matrix is not the 
##      same that was previously cached.

##      RESULTS:
##      for these testcases the inversed matrices are:
##      Inverse of m1
##              [,1] [,2]
##      [1,]   -2  1.5
##      [2,]    1 -0.5

##      Inverse of m2
##              [,1] [,2]
##      [1,]   -4  3.5
##      [2,]    3 -2.5 

makeCacheMatrix <- function( x = matrix()) {
        
        ## the 'makeCacheMatrix' function creates a special object
        ## that stores a matrix and cache's its inverse
        
        i <- NULL

        setMatrix <- function (y = NULL)  {
                lastMatrix <<- x
                x <<- y
        }
        getMatrix <- function () x
        getLastMatrix <- function () lastMatrix
        getInverse <- function () i
        setInverse <- function (inverse) i <<- inverse
        list(setMatrix = setMatrix, getMatrix = getMatrix, 
             getLastMatrix = getLastMatrix, 
             getInverse = getInverse, setInverse = setInverse )
}

cacheSolve <- function(x, ...) { 
        
        ## the 'cacheSolve' function calculates de inverse matrix create by the 
        ## 'makeCacheMatrix'. Before the calculation proceds this function 
        ## checks to see if the inverse has already been calculated and at 
        ## same time if the content of the matrix is the same. 
          
        m <- x$getInverse()
        if(!is.null(m) && identical(x$getMatrix(), x$getLastMatrix())) {
                
                message("getting cached data")
                return(m)
        }
        
        data <- x$getMatrix()
        m <- solve(data, ...)
        x$setInverse(m)
        x$setMatrix (data)
        m
}
