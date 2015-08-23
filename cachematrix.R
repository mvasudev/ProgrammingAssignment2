## Two functions are defined to assign a value to an object in an environment
## that is different from the current environment.

## The two functions are used to create a special object that stores a matrix
## vector and caches its inverse.

## The makeCacheMatrix function creates a special object, which is a list
## containing a function to:
## 1. set the value of the matrix vector (using the set function)
## 2. get or return the matrix (using the get function)
## 3. set or store the inverse of the matrix (using the setinv function)
## 4. get or return the inverse of the matrix (using the getinv function)

makeCacheMatrix <- function(x = matrix()) {
    M <- NULL
    
    set <- function(Y) {
        x <<- Y
        M <<- NULL
    }
    
    get <- function() x
    
## The setinv function is only storing the inverse of the matrix, and not
## calculating it here.
    setinv <- function(inv) M <<- inv

    getinv <- function() M
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## The cacheSolve function calculates the inverse of the matrix created with the
## makeCacheMatrix function. However, it first checks to see if the inverse
## has already been calculated. If so, it gets the inverse from the cache
## and skips the computation. Otherwise, it calculates the inverse of the data
## and sets the value of the inverse in the cache via the setinv function.

cacheSolve <- function(x, ...) {
    
    M <- x$getinv()
    if(!is.null(M)) {
        message("getting cached data")
        return(M)
    }

## Since M does not exist, it first gets the matrix stored with makeCacheMatrix
    data <- x$get()
    
##  The inverse of data is calculated using solve() function and stored.
    M <- solve(data,...)
    x$setinv(M)

## Return a matrix that is the inverse of 'x'
    M
}
