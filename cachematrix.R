##****************************************************************************
## This function creates a special "matrix" object that can cache its inverse.
##****************************************************************************

makeCacheMatrix <- function(x = matrix()) {
	 mtx <- NULL
        set <- function(y) {
                x <<- y
                mtx <<- NULL
        }
        get <- function() x
        setInv <- function(inverse) mtx <<- inverse
        getInv <- function() mtx
        list(set = set, 
             get = get,
             setInv = setInv,
             getInv = getInv)
}

##***************************************************************************
## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then cacheSolve should retrieve the inverse 
## from the cache.
##***************************************************************************

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

	  mtx <- x$getInv()
        if(!is.null(mtx)) {
                message("getting cached data")
                return(mtx)
        }
        data <- x$get()
        mtx <- solve(data, ...)
        x$setInv(mtx)
        mtx
}
