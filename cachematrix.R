## R Programming Assignment 2: Lexical Scoping
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

## makeCacheMatrix: This function creates a special "matrix" object that can
## cache its inverse.
## It is a list containing a function to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the mean
## 4. get the value of the mean

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

## cacheSolve: This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix (see above).
## If the inverse has already been calculated (and the matrix has not changed),
## then the cacheSolve function retrieves the inverse from the cache.
## Otherwise, it calculates the mean of the data and sets the
## value of the mean in the cache via the setmean function.

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
