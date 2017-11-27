## Assignment 2: Lexical Scoping, saving a matrix inverse in the cache
## SALoseke
## you can
## Clear the Console
cat("\014")
## Clear the data Environment
rm(list=ls())


## This function will save a matrix inverse to cache for future use, when we need it again.
## the inverse can be looked up in the cache instead of recomputing. 

makeCacheMatrix <- function(x = matrix()) {
          minv <- NULL
          set <- function(y = matrix()) {
              x <<- y
              minv <<- NULL
          }

          get <- function() x
          setinv <- function(solve) minv <<- solve
          getinv <- function() minv
          list(set = set, get = get,
               setinv = setinv,
               getinv = getinv)
}


## this function will retreive the inverse if it exists, if not, it will compute the inverse and 
## Save in cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        minv <- x$getinv()
        if(!is.null(minv)) {
              message("getting cached inverse")
              return(minv)
        }
        data <- x$get()
        minv <- solve(data, ...)
        x$setinv(minv)
        minv
}


# Examples
#myMat <- matrix(c(4, 2, 7, 6), nrow = 2, ncol = 2)
#myMat2 <- matrix(c(4, 2, 7, 6, 5, 1, 8, 3, 9), nrow = 3, ncol = 3)
#myInv <- makeCacheMatrix(myMat)
#myInv$get()
#myInv$getinv()

#cacheSolve(myInv)

#myInv$getinv()
#myInv$set(myMat2)

#cacheSolve(myInv)

#myInv$get()
#myInv$getinv()




