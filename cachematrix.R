## The makeCacheMatrix function is almost identical to the makeVector function.
# It creates a special "matrix", which is really a list containing a function to
        # 1.  set the value of the square matrix
        # 2.  get the value of the square matrix
        # 3.  set the value of the matrix inversion 
        # 4.  get the value of the matrix inversion 

## This function creates an object that caches the computation of a matrix 
# inversion , and stores it in the list object that can be accessed by the 
# next function.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) i <<- solve
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## The following function calculates the inverse of a matrix of the special 
# "square matrix"created with the above function.
# However, it first checks to see if the
# inverse (solve) has already been calculated. If so, it `get`s the solve from the
# cache and skips the computation. Otherwise, it calculates the inverse (solve) of
# the data and sets the value of the solve in the cache via the `setinverse`
# function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        i <- x$getinverse()
        if(!is.null(i)){
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}

## Unit Test

A <- matrix(c(1,1,4,0,3,1,4,4,0), 3, 3)
solve(A)

a <- makeCacheMatrix(A)
cacheSolve(a)
cacheSolve(a)


