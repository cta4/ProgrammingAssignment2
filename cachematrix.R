## The pair of functions calculate and cache the result of matrix inversion
## the inversion is calculated only once
## The result is cached in global scope after both functions have run


## makeCacheMatrix takes an invertable matrix as its argument
## It then  creates a function object with that matrix
makeCacheMatrix <- function(x = matrix()) {
    
    m <- NULL
    set <- function(y){
        x <<- y
        m <<- NULL
    }
    get <- function() { x }
    setInverse <- function(solve){ m <<- solve }
    getInverse <- function() { m }
    list( set = set, get = get, 
         setInverse = setInverse, getInverse = getInverse)

}


## cacheSolve takes a function object (FO) as its argument
## It uses the functions of the FO to determine if this accesses 
## a previously calculated inversion of matrix associated with FO 
## if yes - then it returns the inverted matrix
## else it performs a new calculation 
## and returns the new inverted matrix
cacheSolve <- function(x, ...) {
    
    m <- x$getInverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    else {
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
    }
    return(m)
}


## myMatrix <- matrix(runif(9), nrow=3, ncol=3)
## invObj <- makeCacheMatrix(myMatrix)
## b <- cacheSolve(invObj)


