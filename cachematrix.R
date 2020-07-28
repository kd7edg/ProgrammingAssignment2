## These functions work together to cache a matrix and the 
## inverted matrix. Initially a call to makeCacheMatrix will
## store the matrix in memory. Inverted matrix doesn't exist
## yet. 
## Next you call cacheSolve. If this is the first time you call
## this function, it creates an inverted matrix and stores it
## in object makeCacheMatrix. Subsequent calls to cacheSolve
## will fetch the cached invented matrix, i.e no need to recalculate
## the inverted matrix. 

## Function makeCacheMatrix returns an object with 2 objects:
## the matrix (x) and the inverted matrix (invMat), and getters and setters 
## for each object.

makeCacheMatrix <- function(x = matrix()) {
        invMat <- NULL
        set <- function(y) {
                x <<- y
                invMat <<- NULL
        }
        get <- function() x
        setinv <- function(mtx) invMat <<- mtx
        getinv <- function() invMat
        list(set = set, get = get,
                setinv = setinv,
                getinv = getinv)
}


## Function cacheSolve returns an inverted matrix. However 
## if that matrix and inverted matrix are cached, there is
## no need to recalculated the inverted matrix. Instead 
## the cached value is returned.
## The input parameter x, is the makeCacheMatrix object

cacheSolve <- function(x, ...) {
        mi <- x$getinv()
        if(!is.null(mi)) {
                message("getting cached data")
                return(mi)
        }
        matrix <- x$get()
        mi <- solve(matrix, ...)
        x$setinv(mi)
        mi
}
