## Example of the mean cached function as a starting point for the inverse-matrix function (see below)
## NOT IMPORTANT FOR PEER REVIEW
## ----------------------------------------------------

# makeVector <- function(x = numeric()) {
#         m <- NULL
#         set <- function(y) {
#                 x <<- y
#                 m <<- NULL
#         }
#         get <- function() x
#         setmean <- function(mean) m <<- mean
#         getmean <- function() m
#         list(set = set, get = get,
#              setmean = setmean,
#              getmean = getmean)
# }
# 
# 
# cachemean <- function(x, ...) {
#         m <- x$getmean()
#         if(!is.null(m)) {
#                 message("getting cached data")
#                 return(m)
#         }
#         data <- x$get()
#         m <- mean(data, ...)
#         x$setmean(m)
#         m
# }

## Put comments here that give an overall description of what your
## functions do

## ---------------------------------------------------------------------------
## --- START OF THE IMPLEMENTATION Assignment: Caching the Inverse of a Matrix ---
## ---------------------------------------------------------------------------

## Instanciates the initial matrix 
## Sets and gets the values of the inversed matrix

## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get, setinverse=setinverse, getinverse=getinverse)
        
}


## Returns a matrix that is the inverse of 'x'. 
## If the inverse matrix has already been calculated the function will return the cached value, if not then it will calculate the inverse value and return it.

cacheInverse <- function(x, ...) {
        
        
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
        
}