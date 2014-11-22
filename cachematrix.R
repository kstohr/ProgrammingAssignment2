## This function will reset and cache the matrix 

makeCacheMatrix <- function(x = matrix()) {
        thematrix<- NULL
        # matrix will be our 'matrix' and it's reset to NULL every
        # time makeCacheMatrix is called
        set <- function(y) {
                x <<- y
                thematrix <<- NULL
        }
        get <- function() { x }
  
        setinverse <- function (matrix) { thematrix <<- matrix }
        # this is called by cacheSolve() during the first cacheSolve()
        # access and it will store the value using superassignment
        getinverse <- function() { thematrix } 
        # this will return the cached value to cacheSolve() on
        # subsequent accesses
        
        list(get = get, set=set,# OK, this is accessed each time makeCacheMatrix() is called,
             setinverse = setinverse, # that is, each time we make a new object. This is a list of
             getinverse = getinverse) # the internal functions ('methods') so a calling function
        # knows how to access those methods.
}


## This function returns the inverse of a matrix created by makeCacheMatrix
## On the first time it's called, it solves for the inverse of the matrix 
## On subsequent calls, it returns the cached value of the inverse of the matrix.  

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x', if cached, return cache
        thematrix <- x$getinverse()
        ## Gets the value of the matrix from the object x
        if(!is.null(thematrix)) {
                message("getting cached data")
                return(thematrix)
        }
        ## Checks to see if the value is not "null." 
        ##If so, returns the cached value with a message that the data is cached
        ## if not, continues on below to compute the inverse of the matrix
        
        data <- x$get()
        ## gets the value of x 
        thematrix <- solve(data, ...)
        ##computes the inverse of x and saves it the variable thematrix
        x$setinverse(thematrix)
        ## passes the value of x to the function cachematrix through superassignment 
        thematrix 
        ##returns the matrix, now inverted
}
