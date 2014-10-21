# functions that cache the inverse of a matrix


## Create matrix object for cacheing its inverse
makeCacheMatrix <- function( m = matrix() ) {

     ## inverse property
    i <- NULL

    ## set the matrix
    set <- function( matrix ) {
            m <<- matrix
            i <<- NULL
    }

    ## get the matrix
    get <- function() {
    	
    	m
    }

    ## set the inverse of the matrix
    setInverse <- function(inverse) {
        i <<- inverse
    }

    ## get the inverse of the matrix
    getInverse <- function() {
       
        i
    }

    ## list of the methods
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## Get the inverse of the special matrix returned by "makeCacheMatrix"
## If the inverse has already been calculated then get the inverse from the cache.
cacheSolve <- function(x, ...) {

    ## get a matrix that is the inverse of 'x'
    m <- x$getInverse()

    ## return if the inverse is already set
    if( !is.null(m) ) {
            message("getting cached data")
            return(m)
    }

    ## get the matrix 
    data <- x$get()

    ## get the inverse using matrix multiplication
    m <- solve(data) %*% data

    ## set the inverse 
    x$setInverse(m)

    ## return
    m
}
