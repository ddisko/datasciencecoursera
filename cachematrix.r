## following functions tests for an existing inverse of a matrix and will pull
## from cache if it exists and if not it calculates that inverse

makeCacheMatrix <- function(x = matrix()) {
      
        inv = NULL
        set = function(y) {
                x <<- y
                inv <<- NULL
        }
        get = function() x
        setinv = function(inverse) inv <<- inverse
        getinv = function() inv
        list(set=set, get=get, setinv=setinv, getinv=getinv)
}

## this section tests for the inverse and calculates if does not exist

cacheSolve <- function(x, ...) {
        
        inv = x$getinv()
        
        if (!is.null(inv)){
                message("inverse already exists no need to calculate")
                return(inv)
        }
        
        targetmatrix = x$get()
        message("calculating inverse")
        inv = solve(targetmatrix, ...)        
        x$setinv(inv)
        
        return(inv)
}
