## Name:Assignment2
## Date:10/30/2016
## This function is used to inverse the Matix

makeCacheMatrix <- function(x = matrix()) {
        
        m <- NULL
        
        set <- function(y) {
                
                #check whether y is matrix or not
                
                if (!is.matrix(y)) {
                        message("this is not matrix, pass valid matrix")
                        return(y)
                    }
                
                x <<- y
                # set this variable to NULL if i/p matrix is passed
                m <<- NULL
        }
       
        get <- function() x
       
         setInverse <- function(inverse) m <<- inverse
        
         getInverse <- function() m
        
         list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)

}


## using this function we can calucluate 
##inverse of matix created by first function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
 
        m <- x$getInverse()
        
        if (!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        in_matrix <- x$get()
        m <- solve(in_matrix, ...)
        x$setInverse(m)
        m
}

