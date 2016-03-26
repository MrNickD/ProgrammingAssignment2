## These functions will cache the inverse of a matrix.
## This is to reduce the costly computation of matrix inversion.

## This first function creates a special "matrix" object that
## can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	invs <- NULL
	setmtrx <- function(y) {
		x <<- y
		invs <<- NULL
	}
	getmtrx <- function() x
	setinvs <- function(solve) invs <<- solve
	getinvs <- function() invs
	list(setmtrx = setmtrx, getmtrx = getmtrx,
		 setinvs = setinvs, getinvs = getinvs)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then the cachesolve should retrieve the
## inverse from the cache.

cacheSolve <- function(x, ...) {
	invs <- x$getinvs()
	if(!is.null(invs)) {
		message("getting cached matrix")
		return(invs)
	}
	storedmatrix <- x$getmtrx()
	invs <- solve(storedmatrix, ...)
	x$setinvs(invs)
	invs
}
