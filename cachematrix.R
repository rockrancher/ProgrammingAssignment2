## This is a function pair to avoid recalculating matrix inversions

## makeCacheMatrix() will take an invertible matrix and register it
## with the parent environment of makeCacheMatrix(), making the 
## operations set(), get(), set_inv() and get_inv() available to 
## manipulate the registered matrix

## cacheSolve() will return the inverse of a registered matrix,
## however, it will only calculate it the first time it is called
## on any particular matrix, on any subsequent calls it will simply
## retrieve and return the pre-calculated inverse

## Author:  Thom Wescott  17 July 2015
## derived from examples given in the Coursera cours R Programming
## in the Data Science Specialization



## This function provides methods to set and get both the original matrix
## and its inverse.  Note that both of the set methods promote the symbol 
## into the calling environment, making the labels for the matrix and 
## its inverse available to sibling functions

makeCacheMatrix <- function(x = matrix()) {
    
    ## set inverse to NULL
    inv <- NULL
    
    set <- function(y) {
        
        x <<- y
        inv <<- NULL
    }

    get <- function() {
        x
    }
    
    set_inv <- function(inverse) {
        inv <<- inverse
    }
    
    get_inv <- function() {
        inv
    }
    
    list(set = set, get = get,
         set_inv = set_inv,
         get_inv = get_inv)
    
}


## This function will return the inverse of a matrix that has been
## registered by calling the makeCacheMatrix() function

## Note that the message texts have be left in to verify the correct
## operation of the functions, when the code is deployed, these 
## messages can be deleted or commented out

cacheSolve <- function(x, ...) {
       
    ## Retrieve the  value of the inverse
    inv <- x$get_inv()
    
    ## If it is not NULL, it has already been calculated,
    ## just return the retrieved value
    if(!is.null(inv)) {
        message("retrieving chached inverse")
        return(inv)
    }
    
    ## If it is NULL, this is the first time for this matrix,
    ## calculate, store and return the inverse
    message("calculating and storing inverse")
    mat <- x$get()    
    inv <- solve(mat)    
    x$set_inv(inv)
    
    
}


neo <- c(2, 3, 2, 6, 8, 5, 1, 9, 1, 3, 6, 4, 7, 2, 4, 8)
neo
dim(neo) <- c(4, 4)
morph <- solve(neo)
morph
foo <- rnorm(9)
foo
bar <- matrix(rnorm(25), 5, 5)
bar
foo <- makeCacheMatrix(bar)
foo
choo <- cacheSolve(foo)
choo
solve(bar)
foo <- matrix(rnorm(25), 5, 5)
