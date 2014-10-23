## These functions help cache the inverse of the matrix once computed
## so that it can be re-used in subsequent calls if the original matrix has not changed


## This is a builder function that creates and returns a
## list of functions set, get, setinverse, getinverse for the input matrix

makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL
	
	set <- function(y) {
		x <<- y			#assigning to the original variable in the parent frame
		inverse <<- NULL	#reset the inverse as original matrix has changed
	}
	
	get <- function() x
	
	# update the inverse symbol from the parent environment with the input
	setinverse <- function(inv) {
			inverse <<- inv	
	}
	
	getinverse <- function() inverse
	
	# return the list of functions created above back to the caller
	list(set = set, 
		 get = get, 
		 setinverse = setinverse, 
		 getinverse = getinverse)
}

## This function checks if the matrix already has the inverse in the cache
## and if it does, it is returned, else it is computed and returned

cacheSolve <- function(x, ...) {
	inverse <- x$getinverse() # try to get the inverse from the cache
	
	if(!is.null(inverse)){
		message("getting cache data")
		return(inverse)	# return inverse from cache and exit function
	}
	
	# This section of the code is reached only if inverse was not found in the cache
	data <- x$get()
	
	inverse <- solve(data,...) 	# compute the inverse	
	x$setinverse(inverse)		# add it to cache so that it doesn't have to be computed again
	inverse 			# return the inverse of the matrix
}
