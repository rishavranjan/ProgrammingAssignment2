## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) 
{
	cache <- NULL

        # create the matrix
        set <- function(y) {
                x <<- y
                cache <<- NULL
        }

        # get the value of the matrix
        get <- function() x
        # invert the matrix and store in cache
        setMatrix <- function(inverse) cache <<- inverse
        # get the inverted matrix from cache
        getInverse <- function() cache

        # return the created functions 
        list(set = set, get = get,
             setMatrix = setMatrix,
             getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) 
{
        ## Return a matrix that is the inverse of 'x'
        cache <- x$getInverse()

        # return inverted matrix from cache
        # else create the matrix
        if (!is.null(cache))
        {
                message("getting cached data")
                return(cache)
        }

        # create matrix
        matrix <- x$get()
        tryCatch( 
        {
                # set and return inverse of matrix
                cache <- solve(matrix, ...)
        },
        error = function(e) 
        {
                message("Error:")
                message(e)

                return(NA)
        },
        warning = function(e) 
        {
                message("Warning:")
                message(e)

                return(NA)
        },
        finally = 
        {
                # set inverted matrix in cache
                x$setMatrix(cache)
        } )

        # display matrix in console
        return (cache)
}
