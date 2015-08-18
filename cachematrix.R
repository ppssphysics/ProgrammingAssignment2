## 18 August 2015 - Data Science Specialization
## RProgramming / Week2 / ProgrammingAssignment2 

## Test Example
## > tstmtx <- matrix(c(3, 4, 2, 1), 2, 2)
## > tstlst <- makeCacheMatrix(tstmtx)
## > cacheSolve(tstlst)
## > cacheSolve(tstlst) --> will return result from cache





## makeCacheMatrix() function ---
## Function creates special "matrix" object that can cache its inverse.
## We assume matrix is always invertible (assignment indications)

makeCacheMatrix <- function(x = matrix()) {
    
    # returned object initialization
    m <- NULL
    
    ## set() : set value of matrix in cashe
    ## get() : get value of matrix from cashe
    
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    
    ## setinv() : set inverse of matrix in cashe
    ## getinv() : get inverse of matrix from cashe
    
    setinv <- function(solve) m <<- solve
    getinv <- function() m
    
    ## Create list of functions to access set,get,setinv,getinv
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}





## cacheSolve() function ---
## Function computes inverse of special "matrix" returned by makeCacheMatrix. 
## If inverse already calculated, cachesolve retrieves inverse from the cache.
## We assume matrix is always invertible (assignment indications)

cacheSolve <- function(x, ...) {
    
    ## Associate getinv() of list x (might not be defined yet)
    m <- x$getinv()
    
    ## if getinv() for x defined in cashe (!=NULL), return inverse from cashe
    if(!is.null(m)) { 
        message("getting cached data")
        return(m)
    }
    
    ## if getinv() for x not yet defined in cashe :
    ## 1. get matrix from cashe
    ## 2. calculate inverse of matrix
    ## 3. set inverse in cashe
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    
    # return inverse matrix
    m
}
