## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix takes input matrix, and creates 
## inverse of it, caches it. 
makeCacheMatrix <- function(x = matrix()) {

        ## matrix inverse is set to null
        xinverse <- NULL
        
        ## set function to set the matrix and inverse to NULL
        set <- function(y){
                x <<- y
                xinverse <<- NULL
        }
        
        ## get function returns the matrix
        get <- function() x
        
        ## getinverse returns the inverse from cache
        getInverse <- function() xinverse
        
        ## setinverse returns the inverse from cache
        setInverse <- function(xinv) xinverse <<- xinv
        
        list(set = set, get = get, 
             getInverse = getInverse, 
             setInverse = setInverse)
        
}


## Write a short comment describing this function
## cacheSolve returns  cached inverse matrix,
## if not cached, computes, caches and returns
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        xinv <- x$getInverse()
        
        if (!is.null(xinv)){
                message("getting cached data")
        }
        else{
                message("computing inverse and caching")
                data <- x$get()
                xinv <- solve(data)
                x$setInverse(xinv) 
        }

        return(xinv)
}
