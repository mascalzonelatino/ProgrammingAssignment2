## This R script computes and caches the inverse of a square matrix.  
## 
## If the input data does not change, any call to the cacheSolve() function
## after the first time will return the inverse matrix from the cache rather than calculating it again.
##  
## Using the cached value is a more efficient operation than recomputing 
## the inverse especially if the input matrix is a large one. 

## This is how you would test this script using a randomly generated square matrix  
## 
## testmatrix <- matrix(rnorm(25), 5)
## testCache <- makeCacheMatrix(testmatrix)
## test_inverse <- cacheSolve(testCache) 
## test_inverse <- cacheSolve(testCache) # 'using cached value' 
## test_inverse <- cacheSolve(testCache) # 'using cached value' 

# makeCacheMatrix creates a list containing a function to:
# set the value of the matrix
# get the value of the matrix
# set the value of the inverse matrix
# get the value of the inverse matrix 

makeCacheMatrix <- function(x = matrix()) {

        inv  <- NULL
        
        set  <- function(y){
                x <<- y
                inv <<- NULL 
        }
        
        get  <- function() x
        
        setinverse  <- function(inverse) inv  <<- inverse
        
        getinverse  <- function() inv
        
        list(set= set, get = get, setinverse = setinverse, getinverse = getinverse)

}


## cacheSolve calculates the inverse of the matrix created with makeCacheMatrix. 
## If the inverse matrix already exists in the cache, the cached value is used. 
## Otherwise, the inverse matrix is computed and updated to cache using the setinverse function
cacheSolve <- function(x, ...) {
        inv  <- x$getinverse()
        
        if (!is.null(inv))
        {
                message("using cached data")
                return(inv)
        }

        matrix_data  <- x$get()
        
        inv  <- solve(matrix_data, ...)
        
        x$setinverse(inv)
        
        inv

}

