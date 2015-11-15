## Functions to perform a cached matrix inverse operation

## Function to create a cachematrix
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    
    ## Function to set the matrix contents and invalidate the cache
    set <- function(y){
        x <<- y
        i <<- NULL
    }
    
    ## Function to return the matrix contents
    get <- function(){x}
    
    ## Function to cache the inverse
    setInv <- function(inv) {i <<- inv}
    
    ## Function to return the cached inverse
    getInv <- function(){i}
    
    ## Return a list containing these functions
    list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## Function to find (or recall) the inverse of a cachematrix
cacheSolve <- function(x, ...) {
    
    ## Get the cached inverse
    i <- x$getInv()
    
    ## if the cached inverse is valid return the cached value
    if(!is.null(i)){
        print("returning cached value")
        return(i)
    }
    
    ## the cache was invalid, so get the data and calculate the inverse
    print("solving for the inverse")
    i <- solve(x$get())
    
    ## cache the newly calculated inverse
    x$setInv(i)
    
    ## Return the matrix that is the inverse of 'x'
    i
}
