## 18 August 2015 - Data Science Specialization
## RProgramming / Week2 / ProgrammingAssignment2 

## Test Example
## > tstmtx <- matrix(c(0, 7, -1, 0), 2, 2)
## > a <- makeCacheMatrix(tstmtx)
## > cacheSolve(a)
## > cacheSolve(a) --> will return result from cache




## makeCacheMatrix()
## Function creates special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    
    ## Check that matrix is invertible, else return NULL
    ## Assignment said to assume invertible but wanted to give it a try.
    if(det(x)==0) {
        message("matrix passed is not invertible: det(x) == 0. Returned NULL!")
        return(NULL)
    } 
    
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


## cacheSolve()
## Function computes inverse of special "matrix" returned by makeCacheMatrix. 
## If inverse already calculated, cachesolve retrieves inverse from the cache.

cacheSolve <- function(x, ...) {
    
    ## Associate getinv() of list x (might not be defined yet)
    m <- x$getinv()
    
    ## if getinv() for x defined in cashe (!=NULL), return inverse from cashe
    if(!is.null(m)) { 
        message("getting cached data")
        return(m)
    }
    
    ## if getinv() for x not yet defined in cashe :
    ## 1. get matrix from cashe: solve() won't work on list x
    ## 2. get inverse of matrix
    ## 3. set inverse in cashe
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    
    # return inverse matrix
    m
}
