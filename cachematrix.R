## R Proggraming - Week 3 Assignment
## Author: Luis Padua @ May 18th 2017
## Functions to create Object that contains a Matrix and had its Inverse 
## cached to avoid redoing Inverse Calculation once it was already calculated


## makeCacheMatrix - creates a List that contains a Matrix and its Inverse

makeCacheMatrix <- function(x = matrix()) {
        invX <- NULL
        
        set <- function(y) {
                x <<- y
                invX <<- NULL
        }
        
        get <- function() x
        
        setInverse <- function(solveX) invX <<- solveX
        
        getInverse <- function() invX
        
        list(set = set, 
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## cacheSolve - calculates the Inverse of the matrix stored on the List created
## by makeCachedMatrix / however if Inverse was already stored in the list 
## cacheSolve returns the cached Inverse without spending time on the calculation

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invX <- x$getInverse()
        
        if(!is.null(invX)) {
                message("getting cached data")
                return(invX)
        }
        
        data <- x$get()
        invX <- solve(data)
        x$setInverse(invX)
        
        invX
}
