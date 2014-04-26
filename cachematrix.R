## A pair of functions that cache the inverse of a matrix to avoid costly computation.


## This function creates a special "matrix" object that can cache its inverse.
## "matrix" is a list of functions: get & set the matrix itself and get & set the inverse
makeCacheMatrix <- function(x = numeric()) {
	z <- NULL
	set <- function(y) {
		x <<- y				# set matrix
		z <<- NULL			# set inverse to null
	}
	get <- function() x			# get matrix
	setInverse <- function(w) z <<- w	# set inverse
	getInverse <- function() z		# return inverse
	list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
                               			# return the "matrix"
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
	z <- x$getInverse()             # query the x vector's cache
	if(!is.null(z)){                # if z is NOT null (there is a cache)
		message("getting cached data")
		return(z)		# just return the cache & exit
	}                               # else
	data <- x$get()                 # get the matrix
	z <- solve(data, ...)           # compute the inverse
	x$setInverse(z)                 # save the inverse
	z                               # return the inverse
}
