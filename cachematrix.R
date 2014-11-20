## The functions below will cache the inverse of a matrix

## makeCacheMatrix creates a matrix objet that can cache it's inverse

makeCacheMatrix <- function(x = matrix()) { ## input x is a matrix
     
     i <- NULL ## is the inverse, reset to NULL when makeCacheMatrix is called
     
     get <- function(){x} ## returns original matrix
     
     setinv <- function(inverse) {i <<- inverse} ## sets i to inverse 
     
     getinv <- function() {i} ##returns cached inv value to cacheSolve
     
     list(get=get,setinv=setinv,getinv=getinv) ## list of methods accessed each time makeCacheMatrix is called.    

}


## cacheSolve computes the inverse of the matrix returned by makeCacheMatrix
## If inverse matrix already calculated then retrieves it from cache

cacheSolve <- function(x, ...) { # x is the object created by makeCacheMatrix
        
     i <- x$getinv() ## gets cached inverse value
        
     if(!is.null(i)) { ## if inverse was cached (i is not NULL)
             
          message("Getting cached data")  # 
          
          return (i) ## cacheSolve ends and the cached value of i is returned
     }
     
     data <- x$get()        # if NULL get original x value
     i <- solve(data, ...)   # if i is NULL, compute inverse
     x$setinv(i)           # store inverse in x
     i               # return the inverse
}
     
