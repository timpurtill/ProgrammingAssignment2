## Combined, the makeCacheMatrix() and cacheSolve() functions allow the creation
## of a cached matrix and its cached inverse.  If the first time the cacheSolve
## function is called and the cached inverse matrix has not already been 
## calulcated, the solve() function will be executed on the matrix and be cached.
## If it has already been calculated the previously inverted matrix will be 
## returned without having to rerun the solve() function again

## Please note these functions are highly coupled.

## Usage:
##
## First - Create a cached matrix from a regular R matrix.  E.g.
##      myCachedMatrix <- makeCacheMatrix(myRegularMatrix)
##
## Whenever the inverse of the cached matrix is needed do the following.  E.g.
##      
##      cacheSolve(myCachedMatrix)


## Takes in a matrix and creates a cached matrix that also has an addtional
## settable/gettable property that is called the inverse matix (im)
## Please note:  No inversion function is performed on the matrix upon creation
## of this cached matrix.  This must be done in conjunction with cacheSolve()
makeCacheMatrix <- function(x = matrix()) {
        im <- NULL ## inverse matrix
        ## x is initially implicity set by the makeCacheMatrix function call
        ## However the below method can be used to "overwrite" the matrix "stored"
        ## in the "makeCacheMatrix" object
        ## Sets the cached matrix and uninitializes the inverse matrix (im)
        set <- function(y) {
                x <<- y
                im <<- NULL
        }
        ## Returns the cached matrix
        get <- function() x
        ## Sets the inverse matix (im) to whatever is passed from outside
        ## Assuming for the purposes of testing the assignment this will
        ## be the result of calling solve(x)
                ## highly coupled
        setinverse <- function(inverseMatrix) im <<- inverseMatrix
        ## Gets th inverse matrix (im)
        getinverse<- function() im
        ## returns the list of setter and getter functions for the cached matrix
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Requires an input in the form of a "makeCacheMatrix" object
##
## Determines if the inverse of its matrix has already been calculated and if so
## returns the previoulsy calculated inverse matrix; along with a warning stating
## it is returning the cached inverse
##
## Otherwise it calculates the inverse matrix and assigns it to the "cached"
## im variable via the "setinverse" function
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## If the inverse has already been assigned/calculated, return the cached
        ## matrix
        csm <- x$getinverse()
        if(!is.null(csm)) {
                message("getting cached data")
                return(csm)
        }
        ## If it hasn't run the solve() function on the cached matrix
        data <- x$get()
        csm <- solve(data, ...)
        ## And assign the resulting inverse matrix to the cached matrix
        x$setinverse(csm)
        ## And return it
        csm
}


