## =============================================== ##
##
## Program:     Cachematrix.R
##
## Project:     Programming assignment 2: Lexical Scoping
##
## Programmer:  Nathaniel Taylor: May, 2018 
## 
## =============================================== ##

## ----------------------------------------------- ##
## 1. Create function that creates a special matrix
##     object that can cache its inverse
## ----------------------------------------------- ##

   makeCacheMatrix <- function(x = matrix()) {
           inv <- NULL
           set <- function(y) {
                x <<- y
                inv <<- NULL
           }
           
           get <- function() x
           setinverse <- function(inverse) inv <<- inverse
           getinverse <- function() inv
           list(set = set, get = get, 
                setinverse = setinverse, 
                getinverse = getinverse)
   }

## ----------------------------------------------- ##
## 2. Create function that computes the inverse of 
##      the special matrix returned by makeCacheMatrix
##      If the inverse has already been calculated
##      (nad the matrix has not changed), then the 
##      cachesolve will retrieve the inverse from the
##      cache. 
## ----------------------------------------------- ##

    cacheSolve <- function(x, ...) {
            inv <- x$getinverse()
            if(!is.null(inv)) {
                    message("getting cached data")
                    return(inv)
            }
            
            data <- x$get()
            inv <- solve(data, ...)
            x$setinverse(inv)
            inv
    }
