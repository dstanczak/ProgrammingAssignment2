## That file contains two functions to calculate the 
#inverse of a matrix which can be inversible.
#The inverse is calculating only in case 
#it is not stored in the cache.

## Function creates a special matrix object
#to cache its inverse
#contain methods to get and set the inverse in cache

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    #setters and getters
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    
    #set solve in cache
    setsolve <- function(solve) m <<- solve
    
    #get solve from cache
    getsolve <- function() m
    
    #list of defined functions
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## Function to get a matrix inverse
#gets one argument - a inversible matrix x
#returns one object - a matrix which is the inverse of x
#inverse is taken from the cache if it already exists there
#if not it is calculated and send to the cache

cacheSolve <- function(x, ...) {
    ## check if inverse is cached
    m <- x$getsolve()
    
    ##if yes then return it from cache
    if(!is.null(m)) {
        message("getting cached inverse")
        return(m)
    }
    
    ##otherwise it calculates the inverse and set in cache
    message("calculating inverse...")
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m
}
