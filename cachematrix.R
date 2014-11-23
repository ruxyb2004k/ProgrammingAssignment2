## Put comments here that give an overall description of what your
## functions do
## The two functions are cachin the inverse of a matrix

## Write a short comment describing this function
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
		inverse_matrix <- NULL
		setmatrix <- function(y){
				x <<- y
				inverse_matrix <<- NULL
		}
		getmatrix <- function() x
		setinverse <- function(inv) inverse_matrix <<- inv
		getinverse <- function() inverse_matrix
		list(set = setmatrix, get = getmatrix, 
			setinv = setinverse, getinv = getinverse )
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
            ## Return a matrix that is the inverse of 'x'
		inverse_matrix <- x$getinv()
		if(!is.null(inverse_matrix)){
				message("getting cached data")
				return(inverse_matrix)
		}
		data <- x$get() 
		inverse_matrix <- solve(data, ...)
		x$setinv(inverse_matrix)
		inverse_matrix	
}
