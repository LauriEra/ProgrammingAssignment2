## Put comments here that give an overall description of what your
## functions do
## Functions using caching for operating with matrices

## Write a short comment describing this function
## Creates a matrix object
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set=set,get=get,setInverse=setInverse,
         getInverse=getInverse)
}


## Write a short comment describing this function
## Returns the inverse, from cache if solved before and if not solves and also caches the result
cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    if(!is.null(inv)){
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setInverse(inv)
    inv
}
