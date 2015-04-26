### made by Eva Bosch i Roura -- Coursera R Programming, Assignment 2 ###

## The following two functions are used to cache the inverse of a matrix

## x is a matrix. makeCacheMatrix(x) creates a kind of matrix object that
## can cache its inverse. It contains 4 functions: set, get, storeSolve 
## and getSolve.

makeCacheMatrix <- function(x = matrix()) { 
  inv <- NULL ## NULL v. to process inverse if not already stored
  set <- function(y) { ##FUNCTION 1: set new matrix. 
                        ##USE: matrixname$set(matrix(c(num:num),nrows,ncols))
    x <<- y           ##substitutes stored matrix (x) for input (y)
    inv <<- NULL      ##restores value of inv to NULL, since new 
                      ##inverse must be calculated
  }
  get <- function() x ##FUNCTION 2: returns matrix stored in the main function. 
                      ##USE: matrixname$get()
  storeSolve <- function(solve) inv <<- solve ##FUNCTION 3: stores the value of 
                                              ##input in variable "inv". 
                                              ##USE: matrixname$storeSolve(x)
  getSolve <- function() inv ##FUNCTION 4: return value of stored inv. 
                              ##USE: matrixname$getSolve()
  list(set = set, get = get,  ##make sure the object has all 4 functions when 
       storeSolve = storeSolve,   ##we apply makeCacheMatrix
       getSolve = getSolve)
}


## cacheSolve calculates the inverse of the spec. matrix created by makeCacheMatrix.

cacheSolve <- function(x, ...) { ## Return a matrix that is the inverse of 'x'
  inv <- x$getSolve() ##returns de result of the getSolve function (makeCacheMatrix)
  if(!is.null(inv)) { ## if inv is not null
    message("getting cached data") ##print message while processing
    return(inv) ##return inv, which is the inverse matrix, already stored
  }
  data <- x$get() ##if inverse is not stored, store result of get function 
  inv <- solve(data, ...) ## calculate its inverse --> solve function
  x$storeSolve(inv) ## store it as inv, get it through makeCacheMatrix
  inv ##return inv
}


### EASY INVERT TEST (by Al Warren, Community TA) ###
# m <- matrix(c(-1, -2, 1, 1), 2,2)
# x <- makeCacheMatrix(m)
# x$get()
# [,1] [,2]
# [1,]   -1    1
# [2,]   -2    1
# 
# inv <- cacheSolve(x)
# inv
# [,1] [,2]
# [1,]    1   -1
# [2,]    2   -1
# 
# > inv <- cacheSolve(x)
# getting cached data
# > inv
# [,1] [,2]
# [1,]    1   -1
# [2,]    2   -1