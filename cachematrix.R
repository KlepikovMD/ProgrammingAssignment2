## Inverting a matrix is costly computation. Because of this, 
## here is a "class" representing a pair of matrix and inverted
## matrix, accessible vea getter and setter methods. makeCacheMatrix
## makes a new instance of this class, and cacheSolve is a inversion 
## function workong only on makeCacheMatrix instances. When 
## called a fist time, it will cache the result in memory
## and will return it again afterwards.

## Create instance of object representing both matrix and
## it's inversion

makeCacheMatrix <- function(x = matrix()) {
    mat <- x
    inv_mat <- NULL
    get <- function(){
        mat
    }
    set <- function(new){
        mat <<- new
        inv_mat <<- NULL
    }
    getinv <- function(){
        inv_mat
    }
    setinv <- function(new){
        inv_mat <<- new
    }
    list(get = get, set = set, getinv = getinv, setinv = setinv)
}


## Inverts the cacheMatrix. At first call will calculate inversion
## and return it, on subsequent will return memorised result.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)){
        ## message("Returning cached result") ##UNCOMMENT FOR DEBUG
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinv(inv)
    inv
}
