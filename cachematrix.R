## Put comments here that give an overall description of what your
## functions do
#This function creates a special "vector", which is really a list containing a 
#function to:
#1. set the value of the matrix
#2. get the value of the matrix
#3. set the value of the inverse matrix
#4. get the value of the inverse matrix

## Write a short comment describing this function
#This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        #m will store inverse matrix
        m <- NULL
        
        #setup the initial value
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        #get main matrix
        get <- function() x
        
        #set inverse of main matrix x
        setinverse <- function(inval) m <<- inval
        
        #get the inverse of matrix x
        getinverse <- function() m
        
        #return the list with functions
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function
#This function computes the inverse of the special "matrix" returned by 
#makeCacheMatrix above. If the inverse has already been calculated (and the 
#matrix has not changed), then the cachesolve retrieves the inverse from the 
#cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        
        #check if inverse returned is null, if not then return the cached value
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        #calculate inverse and set the same
        data <- x$get()        
        m <- solve(data)
        x$setinverse(m)
        
        #return inverse matrix
        m
}
