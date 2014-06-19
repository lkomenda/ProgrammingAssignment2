## Functions below can be used for matrix inverse computation. The first one is a collection
##(list) of auxiliary functions the second one gets or compute the matrix inverse.

## Function makeCacheMatrix creates a list containing 4 functions:
#1. set_data  - sets the matrix on which inverse will be computed
#2. get_data - gets the matrix for computations
#3. set_inverse - sets the inverse of a matrix
#4. get_inverse - gets the inverse of a matrix
#Calling the function sets the empty cache also


makeCacheMatrix <- function(x = matrix()) {
  
		inv <- NULL
        set_data <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get_data <- function() x
        set_inverse <- function(inverse) inv <<- inverse
        get_inverse <- function() inv
        list(set_data = set_data, get_data = get_data,
             set_inverse = set_inverse,
             get_inverse = get_inverse)
}


## cacheSolove is a function used on result of makeCacheMatrix. It tries to get cached inverse. 
## If cache is empty it computes the inverse and returns it.
## This is simple function which can be used only on invertible matrices
## (an error will be returned if the matrix isn't invertible)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		inv <- x$get_inverse()
        if(!is.null(inv)) {
                message("getting cached inverse")
                return(inv)
        }
        dane <- x$get_data()
        inv <- solve(dane, ...)
        x$set_inverse(inv)
        inv
}


####Example
c<-rbind(c(1, -1/4), c(-1/4, 1))  

makeCacheMatrix(c)->X
cacheSolve(X)
