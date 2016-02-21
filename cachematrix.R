## create a special object that stores a matrix and cache's its inverse.

## function "makeCacheMatrix" creates a special "matrix", containing a function to

##set the value of the matrix
##get the value of the matrix
##set the value of the inverse
##get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        cache_Inverse<- NULL
        set <- function(y) {
                x <<- y
                cache_Inverse<<- NULL
        }
        get <- function() x
        set_Inverse <- function(inverse) cache_Inverse <<- inverse
        get_Inverse <- function() cache_Inverse
        list(set = set, get = get,
             set_INVERSE = set_INVERSE,
             get_INVERSE = get_INVERSE)
}


## The following function calculates the inverse of the special "matrix" created with the above function"makeCacheMatri". 
## it first checks to see if the inverse has already been calculated. If so, it gets the inverse from the cache and 
## skips the computation. Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the set_INVERSE function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invFunc <- x$get_Inverse()
        if(!is.null(invFunc)) {
        message("getting cache data")
        return(invFunc)
        }
        data <- x$get()
        invFunc <- solve(data, ...)
        x$set_Inverse(invFunc)
        invFunc
}
