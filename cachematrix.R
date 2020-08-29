## R Programming course week 3 assignment 

## This function will create a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {           ## sets argument x to be a matrix initially
 
   inv <- NULL                                        ## initializes inverse to be null, it will hold values of matrix inverse
  set  <- function(y)    {                            
    x     <<- y
    inv   <<- NULL                                    ## if matrix new then set inv to null 
  }
  get        <- function() x                          ## Will get the matrix x
  
  setinverse <- function(inverse) inv <<- inverse 
  getinverse <- function() inv
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)   ## it is required to subset with $ sign.
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If inverse has already been calculated (and the matrix has not changed),
## then the cachesolve will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()                                       
    if(!is.null(inv))    {                              ## if inverse of matrix is in cache it will look for it in the cache function
      message("getting cached data")
      
      return(inv)                                       ## gives inverse of the matrix retrieved from makeCacheMatrix
    }
    data <- x$get()
    inv  <- solve(data, ...)                            ## computes the inverse of the matrix in data when inverse is not cached
    x$setinverse(inv)                                   ## this stores the value of inverse to inv.
    inv
}

