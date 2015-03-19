## Matrix inversion is usually a costly computation and the below 2 functions
## are used to perform the computation and cache the values.
##
## Repeated calls can return the cached values rather than perform the 
## computation
## 
## The below 2 functions makeCacheMatrix and cacheSolve are used to
## store the matrix and its inverse and to compute the inverse of the matrix 
## and cache the values
##
## Example usage is given below for clarity
##
## Step 1. Create a matrix
##    	   m <- matrix( c(4, 3, 3, 2), nrow=2, ncol=2, byrow = TRUE)
##
## Step 2. Create the special matrix object
##         sm <- makeCacheMatrix (m)
##
## Step 3. Compute the inverse of the special matrix 
##         cacheSolve ( sm )
##  
## Step 4. This method call returns the cached value rather than computing the inverse and 
##         prints the message confirming the same.
##         cacheSolve ( sm )           

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {

    m <- NULL
    
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    get <- function() x
    
    setinvmatrix <- function(matrx) m <<- matrx

    getinvmatrix <- function() m

    list(set = set, get = get, setinvmatrix  = setinvmatrix , getinvmatrix = getinvmatrix)

}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {

    m <- x$getinvmatrix()
        
    if(!is.null(m)) {
	## this message is included here to show that the code works as expected    
        message("getting cached data")
        ## Return a matrix that is the inverse of 'x'
        return(m)
    }

    data <- x$get()

    m <- solve(data, ...)

    x$setinvmatrix(m)

    m

}
