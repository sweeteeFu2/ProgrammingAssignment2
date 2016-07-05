## Matrix inversion is usually a costly computation and there may
## be some benefit to caching the inverse of a matrix rather than 
## compute it repeatedly. The pair of functions given below chache
## the inverse of a matrix

## The makeCacheMatrix function creates a special "matrix" object
## that can cache its inverse. This function will return a list 
## containing a function to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse matrix
## 4. get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) { 
        inverse<-NULL
        set<-function(m){
          x<<-m
          inverse<<-NULL
        }
        get<-function() x
        setInverse<-function(solve) inverse<<-solve
        getInverse<-function() inverse
        list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## The cacheSolve function computes the inverse of the special 
## matrix returned by the makeCacheMatrix function above.If the 
## inverse has already been calculated (and the matrix has not
## changed), then the cacheSolve should retrieve the inverse
## from the cache. This function assumes that the matrix supplied
## is always invertible.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse<-x$getInverse()
        if(!is.null(inverse)){
            message("getting cached data")
            return(inverse)
        }
        data<-x$get()
        inverse<-solve(data)
        x$setInverse(inverse)
        inverse
}
