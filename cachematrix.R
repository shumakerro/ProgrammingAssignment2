## The functions herein create a special "matrix" and calculate its inverse 
## The first function, makeCacheMatrix, creates a special "matrix", which is really a list containing a function to
    #set the value of the matrix
    #get the value of the matrix
    #set the inverse of the matrix
    #get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
          setInverseMatrix <- function(InvMatrix) m <<- solve
          getInverseMatrix <- function() m
        list(set = set, get = get,
          setInverseMatrix = setInverseMatrix,
          getInverseMatrix = getInverseMatrix)
}
##cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
   #If the inverse has already been calculated (and the matrix has not changed), 
   #then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
         m <- x$getInverseMatrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- mean(data, ...)
        x$setInverseMatrix(m)
        m
}
