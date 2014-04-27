## makeCacheMatrix & cacheSolve computes an inverse of a given matrix and cache the results for future use within the same session

## makeCacheMatrix handle's the caching of matrix and its inverse, i.e given a matrix and it's inverse, it'll set the datastructure for storeing and retriving the values. 
## @param x a matrix
## @example: 
##  		 d<- makeCacheMatrix(matrix(c(1,2,3,0,1,4,5,6,0), 3))
##		 d$setinverse(solve(d$get()))
##		 d$getinverse()

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y) {
                x <<- y
                i <<- NULL
        }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## cachSolve computes an inverse of a matrix and cache the result by using makeCacheMatrix
## @param x a makeCacheMatrix list
## @param ... parameter options for the solve function
## @example: 
##			 d<- makeCacheMatrix(matrix(c(1,2,3,0,1,4,5,6,0), 3))
##			 c<- cacheSolve(d)
##			 c%*%d$get() #would result in identity matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
       if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
}
