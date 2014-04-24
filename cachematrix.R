## makeCacheMatrix construct a special object whcih 
## acts like a container for storing matrix related data
## cacheSolve is responsible for calculating matrix inverse 
## and caching functioality

## makeCacheMatrix function construct an object for holding 
## matrix related data. It also implements getter and setter 
## function for the ease of use
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        get <- function() x
        set <- function(mx) {
                inv <<- NULL
                x <<- mx
        }
        getInverse <- function() inv
        setInverse <- function(inverse) inv <<- inverse 
        list(get=get, set=set, getInverse=getInverse, setInverse=setInverse)
}


## cacheSolve function ask CacheMatrix object for cached inverse matrix to 
## print out. If there is no such matrix, than it takes the cached matrix,
## compute the inverse matrix, cache it to the CacheMatrix object.
cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        if(!is.null(inv)) {
                print("getting cached inverse matrix")
                return(inv)
        }
        mx <- x$get()
        inv <- solve(mx)
        x$setInverse(inv)
        inv
}
