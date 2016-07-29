## makeCacheMatrix() function let us do following operations to provided matrix  - 
##      -getinverese(): get inverse matrix from memory(if available)
##      -setinverse(): save inverse matrix in the memory
##      -set(): change the matrix of the x parameter 
##      -get(): retrieve the matrix provided in argument.

makeCacheMatrix <- function(x = matrix()) {
        
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
}



# cacheSolve() first gets inverse of the matrix from makeCacheMatrix() using getinverse(). 
# In case matrix is not saved in memory then makeCacheMatrix returns NULL because
# that's how the inverse matrix is initialized in makeCacheMatrix().
# In case the inverse matrix is not NULL[checked with if()] then the matrix returned by getinverse() is returned to cacheSolve().
# In case matrix received from getinverse() is NULL then inverse is calculated using the solve() function. The inverse matrix
# calculated by solve() is saved in memory using setinverse() and also that inverse matrix is returned to cacheSolve().



cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        
        if(!is.null(i)) {
                message("getting cache value")
                return(i)
        }
        
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
