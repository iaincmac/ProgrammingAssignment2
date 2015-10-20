# Put comments here that give an overall description of what your
# functions do
#
# makeCacheMatrix provides a lis of functions to the working environment to :
# a. set the value of the matrix 
# b. get the value of the matrix 
# c. set the value of inverse of the matrix 
# d. get the value of inverse of the matrix 


makeCacheMatrix <- function(x = matrix()) {
        # store the cached value
        # initialised to NULL
        
        mtrx <- NULL
        
        #create the matrix in the working environment
        
        setup <- function(y) {
                x <<- y
                mtrx <<- NULL
        }
        
        #get the value of the matrix
        
        get <- function() x
        
        #invert the matrix
        setMatrix <- function(inverse) mtrx <<- inverse
        #get the inverted matrix from cache
        getInverse <- function() mtrx
        
        # return thes above functions to the working environment
        
        list(setup = setup, get = get,
             setMatrix = setMatrix,
             getInverse = getInverse)

}


# cacheSolve function returns the inverse of the matrix. It first checks if the
# inverse has already been computed. If so, it gets the result and skips the
# computation. If not, it computes the inverse, sets the value in the cache via 
# setMatrix function. tryCatch is used to gracefully handle a non invertible
# matrix


cacheSolve <- function(x, ...) {
        # Return a matrix that is the inverse of 'x'
        # attempt to get the inverse of the matrix stored in cache
        mtrx <- x$getInverse()
        
        # returns inverted matrix from cache if it exists
        # else create the matrix in working environment
        
        if (!is.null(mtrx)) {
                message("Getting the cached data")
                
                # display matrix in console
                return(mtrx)
        }
        
        # create matrix since it does not exist
        newmtrx <- x$get()
        
        # make sure matrix is square and invertible
        # if not, handle exception cleanly
        
        tryCatch( {
                # set and return inverse of matrix
                mtrx <- solve(newmtrx, ...)
        },
        error = function(e) {
                message("Error:")
                message(e)
                message()
                return(NA)
        },
        warning = function(e) {
                message("Warning:")
                message(e)
                message()
                return(NA)
        },
        finally = {
                # set inverted matrix in cache
                x$setMatrix(mtrx)
        } )
        
        # display matrix in console
        return (mtrx)
}
