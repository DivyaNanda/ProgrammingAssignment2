# R-programming
# Programming Assignment #2

# The 'makeCacheMatrix' function creates an object to handle
# the matrix which is to be inverted.
# And also returns a list of available methods.

makeCacheMatrix <- function(x = matrix()) {
	# Clears the cachedInverse variable
	cachedInverse <- NULL
	
	# Method to set a new matrix to be inverted
	set <- function(y) {					
		x <<- y						# update x
		cachedInverse <- NULL				# clear the inversion result, since x is updated
	}
	
	# Method to obtain the original matrix
	get <- function() x					# return x
	
	# Method to set the inverse of x
	setInverse <- function(inverse) {
		cachedInverse <<- inverse			# Update the inverse
	}
	
	# Method to obtain the cached inverse of x
	getInverse <- function() cachedInverse	                # return the cached inverse
	
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

cacheSolve <- function(cachedMatrix) {
	# Obtain the previously stored result
	inverse <- cachedMatrix$getInverse()				
	
	# Check if variable 'inverse' is null or not (i.e. inverse of the given matrix is already calculated or not)
	if(!is.null(inverse)) {								
		message("Obtaining inverse from cache..")
		return(inverse)					# Return the cached result of matrix inverse
	}
	
	# Calculate the inverse and cache it
	inverse <- cachedMatrix$setInverse(solve(cachedMatrix$get()))	
	
	return(inverse)
}
