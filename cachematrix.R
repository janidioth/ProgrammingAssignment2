## Put comments here that give an overall description of what your
## functions do.
## 
##  makeCacheMatrix: This function creates a special "matrix" object 
##                   that can cache its inverse. It uses the <<- form 
##                   of the assignment operator to assign its data 
##                   objects in the parent environment. It returns a
##                   list of getter and setter functions.
##  
##  cacheSolve: This function computes the inverse of the special 
##              "matrix" returned by makeCacheMatrix. If the inverse 
##              has already been calculated (and the matrix has not 
##              changed), then cachesolve will retrieve the inverse 
##              from the cache otherwise it will calculate the matrix
##              inverse to return.

## Write a short comment describing this function
##
## makeCacheMatrix creates the special "matrix" object that contains 
## 2 data objects, x and i, and returns a list of 4 functions (set,  
## get, setinversematrix and getinversmatrix).
##
## The 2 set functions uses the <<- operator to assign values to the 
## data objects in the parent environment. The 2 get functions return 
## the data objects value. 
makeCacheMatrix <- function(x = matrix()) {
    ## inverse is set to NULL when new matrix is use for "make"
    i <- NULL
    set <- function(y = matrix()) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinversematrix <- function(inversematrix) i <<- inversematrix
    getinversematrix <- function() i
    list(set = set, get = get,
         setinversematrix = setinversematrix,
         getinversematrix = getinversematrix)
}


## Write a short comment describing this function
##
## cacheSolve uses the special "matrix" from makeCacheMatrix to compute
## and caches its inverse. 
##
## If the inverse is NULL (when a new matrix was "make" or replaced by
## set and could not be found in the parent environment), the inverse 
## will be calculated and cached (using setinversematrix from 
## makeCacheMatrix). If the inverse has already been calculated (and the 
## matrix has not changed), then cachesolve will retrieve the inverse 
## (using getinversematrix) from the cache to return.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinversematrix()
        
        ## if inverse in cache (value not NULL), retrieve from cache
        ## otherwise calculate and store in cache before returning 
        if(!is.null(i)) {
            message("getting cached data")
            return(i)
        }
        data <- x$get()
        i <- solve(data)
        x$setinversematrix(i)
        i
}
