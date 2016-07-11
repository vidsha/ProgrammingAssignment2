## Since Matrix inversion is a costly computation, there are benefits in 
## caching the matrix and its inverse over computing the same repeatedly. 	
## The functions makeCacheMatrix and cacheSolve are used to cache the inverse
## given a square matrix. 

## The following function, makeCacheMatrix does the following tasks:
## 	1> set the value of the matrix
## 	2> get the value of the matrix
## 	3> set the value of the matrix inverse
## 	4> get the value of the matrix inverse


makeCacheMatrix <- function(x = natrix()) {
        Minverse <- NULL
        set <- function(y) {
                x <<- y
                Minverse <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) Minverse <<- inverse
        getinv <- function() Minverse
        list(set= set, 
             get = get,
             setinv = setinv,
             getinv = getinv)
}
 
## Assumption: Matrix passed is always invertible.
## If the inverse of a particular matrix is already calculated, it is returned from cache, 
## else matrix inverse is calculated and is saved to cache using setinv 
 
cacheSolve <- function(x, ...) {
        Minverse <- x$getinv()
        if(!is.null(Minverse)) {
                message("getting inverse from cache...")
                return(Minverse)
        }
        message("Matrix inverse not in cache. Calculating...")
	matrix1 <- x$get()
        Minverse <- solve(matrix1)
        x$setinv(Minverse)
        Minverse  ## Returns inverse of matrix 'x'
}