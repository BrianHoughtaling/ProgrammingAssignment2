## R Programming Assignment 2: Lexical Scoping
##
## The second programming assignment requires the creation of an R function that
## is able to cache potentially time-consuming computations.
## For example, taking the mean of a numeric vector is typically a fast
## operation. However, for a very long vector, it may take too long to compute
## the mean, especially if it has to be computed repeatedly (e.g. in a loop).
## If the contents of a vector are not changing, it may make sense to cache the
## value of the mean so that when we need it again, it can be looked up in the
## cache rather than recomputed. In this Programming Assignment we take
## advantage of the scoping rules of the R language and how they can be
## manipulated to preserve state inside of an R object.
##

##
## makeCacheMatrix: This function creates a special "matrix" object that can
## cache its inverse.
##

makeCacheMatrix <- function(x = matrix()) {
        
        m <- NULL
        
        set <- function(y){
                x <<-y
                m <<-NULL
        }
        get <- function() x
        
        setmatrix <- function(solve) m <<- solve
        
        getinverse <-function() m
        
        list(set=set, get=get,
             setmatrix=setmatrix,
             getinverse=getinverse)

}

##
## cacheSolve: This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix (see above).
## If the inverse has already been calculated (and the matrix has not changed),
## then the cacheSolve function retrieves the inverse from the cache.
##

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        
        if(!is.null(m)) {
                ## Output message to confirm that cached data is being returned
                message("getting cached data")
                return(m)
        }
        
        matrix <- x$get()
        m <- solve(matrix, ...)
        x$setmatrix(m)
        m
}

##
## Unit Test:
## The following can be used as a unit test for Programming Assignment 2.
## Copy and paste the commands in the console and review expected results as
## shown.
#
#       >    source("cachematrix.R")
#
#
#       >    test = makeCacheMatrix(matrix(c(10,20,30,40), nrow=2, ncol=2))
#
#       >    test$get()         # Returns original matrix
#             [,1]  [,2]
#       [1,]    10    30
#       [2,]    20    40
#
#       >   cacheSolve(test)   # Compute, cache, & return the matrix inverse
#              [,1] [,2]
#       [1,]   -0.2  0.15
#       [2,]    0.1 -0.05
#
#       >  test$getinverse()  # Return matrix inverse
#              [,1] [,2]
#       [1,]   -0.2  1.5
#       [2,]    0.1 -0.05
#
#       >  cacheSolve(test)   # Returns cached matrix inverse
#       getting cached data
#              [,1]  [,2]
#       [1,]   -0.2  0.15
#       [2,]    0.1 -0.05
#
#       >    test$set(matrix(c(0,5,99,66), nrow=2, ncol=2)) # Modify the matrix
#
#       >    cacheSolve(test)   # Compute, cache, and return new matrix inverse
#                   [,1] [,2]
#       [1,] -0.13333333  0.2
#       [2,]  0.01010101  0.0
#
#       >    test$get()         # Return matrix
#               [,1] [,2]
#       [1,]    0   99
#       [2,]    5   66
#
#       >    test$getinverse()  # Return matrix inverse
#                   [,1] [,2]
#       [1,] -0.13333333  0.2
#       [2,]  0.01010101  0.0
