## The first function makeCacheMAtrix() creates a special vector which contains a list of 4 functions
## 1.set() to set the value of matrix
## 2.get() to get the value of matrix
## 3.setinverse() to set the value of inverse matrix
## 4.getinverse() to get the value of inverse matrix
## The second function cacheSolve() computes the inverse of matrix but it first checks if the inverse 
## had previously been computed.This saves time as it prevents computation again and again. 



## This function takes an input in a matrix form and creates a special vector containing a list of functions.
## In this function we have used <<- operator which can be used to assign a value to the object in an 
## environment that is different from current environment.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL                                  
        set <- function(y) {                      
                x <<- y
                m <<- NULL
        }
        get <- function() x                       
        setinverse <- function(inv) m <<- inv     
        getinverse <- function() m                
        list(set = set, get = get,
             setinverse = setinverse,             
             getinverse = getinverse)
}


## This function computes inverse of matrix using the solve() function but before that it first checks 
## If the inverse has already been computed.We use 'm' to check if the inverse is already computed or not.
## If 'm' is null,then inverse is computed otherwise cached data is returned which saves time.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {                         
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)                     
        x$setinverse(m)
        m
}
