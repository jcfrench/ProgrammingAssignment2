## cachematrix.R for RDPeng's R-Programming Section 010: Class Assignment #2
## Author/Student: JCFrench - 01/21/2015

## Two functions, makeCacheMatrix and cacheSolve, demonstrate data caching 
## and lexical scoping.

## This function creates a special "makeCacheMatrix" object that contains a 
## copy of a base matrix, and may contain a cached copy of an inverse matrix.
## "makeCacheMatrix" is limited in that it does not ensure internal consistency 
## between the base matrix "x", and the inverse matrix "m". Instead, consistency
## management is deferred to the "cacheSolve" function.

## 4 utility functions handle the base & inverse matrix:
## makeCacheMatrix.set() initializes the makeCacheMatrix object with a base 
###     matrix and clears any previously cached inverse matrix.
## makeCacheMatrix.get() returns the base matrix
## makeCacheMatrix.setinv() stores the inverse matrix
## makeCacheMatrix.getinv() returns the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(base) {
                x <<- base
                m <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) m <<- inverse
        getinv <- function() m
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## cacheSolve provides state control for the makeCacheMatrix object. When the 
## "cacheSolve" function is called with a "makeCacheMatrix" object, "cacheSolve"
## retrieves the inverse matrix. If the inverse matrix has been calculated, 
## it is returned. If the inverse matrix is NULL, "cacheSolve" calculates the 
## inverse matrix, "solve(data,...)", stores the inverse with "setinv()" 
## function, and then returns the inverse matrix. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached inverse matrix")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}

## testCacheMatrix calculates inverse matrices, identity matrices
## This function test the above functions by calculating a inverse matrices 
## with caching functions, & then performs matrix multiplication to demo
## an identity matrix calculation. 

##Note that "makeCacheMatrix.set()" is not tested or called in this example.

testCacheMatrix <- function(){
## first, basic demonstration of matrix multiplication
        a <- matrix(c(2,3,5,7,11,13,17,19,23),nrow=3,ncol=3)
        print("Base Matrix a:")
        print(a)
        print(" ")
        
        a.inv <- solve(a)
        print("Inverse of Matrix a (a'):")
        print(a.inv)
        print(" ")

        print("Identity Matrix = Matrix product of a * a':")
        i1 <- a %*% a.inv
        print(i1)
        print("Note the rounding errors in numeric calculations,") 
        print("several entries in the identity matrix are almost zero.") 
        print(" ")        
## second, demonstration of makeCacheMatrix & cacheMatrix functionality
        b = makeCacheMatrix(a)
        print("Base Matrix from makeCacheMatrix b:")
        print(b$get())
        print(" ")

        print("Calculating Inverse Matrix from b with cacheSolve(b):")
        print("Inverse Matrix from b:")
        print(cacheSolve(b))
        print(" ")

        print("Re-using Inv Matrix from b cacheSolve to calculate identity matrix:")
        print("Identity Matrix = Matrix product of b$get() * cacheSolve(b):") 
        i2 <- b$get() %*% cacheSolve(b)
        print(i2)
        print(" ")

        print("Note: The identity matrix in each example is identical: ")
        print(identical(i1,i2))
}