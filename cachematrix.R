# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
		inv <- NULL
		set <- function(y) {
			x <<- y
			inv <<- NULL
		}
		get <- function() x
		setmatrix <- function(inverse) inv <<- inverse
		getmatrix <- function () inv
		list(set = set, get = get,
			setmatrix = setmatrix, getmatrix = getmatrix)
}
# The following function returns the inverse of the matrix. It first checks if
# the inverse has already been computed. If so, it gets the result and skips the
# computation. If not, it computes the inverse, sets the value in the cache via
# setinverse function.
#The det() function will calculate the determinant of the 
#matrix and if the determinant is zero then the matrix has  
#no inverse the function will return the input matrix
cacheSolve <- function(x,...){
		inv <- x$getmatrix()
		if (!is.null(inv)){
			message("getting cached data")
			return(inv)
		}
		data <- x$get()
		det <- det(data)
		if(det != 0){
			inv <- solve(data,...)
			x$setmatrix(inv)
		}
		else{
			message("Cannot calculate inverse for the matrix")
			inv <- data
			x$setmatrix(inv)
		}
		inv
}
