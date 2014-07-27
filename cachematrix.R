# Programming Assignment 2

# The 'makeCacheMatrix' function creates an object to handle
# the matrix that can be inverted.
# And returns a list of available methods.

makeCacheMatrix <- function(x = matrix()) {
	# Clears the myInverse variable
	myInverse <- NULL
	
	# Method to set a new matrix to be inverted
	set <- function(y) {					
		x <<- y								# update x
		myInverse <- NULL				# clear the inversion result, since x is updated
	}
	
	# Method to obtain the original matrix
	get <- function() x						# return x
	
	# Method to set the inverse of x
	setInverse <- function(inverse) {
		myInverse <<- inverse			# Update the inverse
	}
	
	# Method to obtain the cached inverse of x
	getInverse <- function() myInverse	# return the cached inverse
	
	list(set = set,
		get = get,
		setInverse = setInverse,
		getInverse = getInverse)
}


## This function is responsible for obtaining the
## inverse of the given matrix. It first checks the cache,
## if inverse is already calculated, it obtains it from the cache 
## and returns, otherwise it calculated it and then returns
## the inverse of given matrix. 
## This method accepts 'makeCacheMatrix' as an argument.

cacheSolve <- function(matrix) {
	# Obtain the previously stored result
	mat_inverse <- matrix$getInverse()				
	
	# Check if variable 'mat_inverse' is null or not (i.e. inverse of the given matrix is already calculated or not)
	if(!is.null(mat_inverse)) {								
		message("Getting result from cache...")
		return(mat_inverse)									# Return the cached result of matrix inverse
	}
	
	# Calculate the inverse and cache it
	mat_inverse <- matrix$setInverse(solve(matrix$get()))	
	
	return(mat_inverse)
}
