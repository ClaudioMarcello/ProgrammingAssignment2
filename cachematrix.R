## A pair of functions that cache the inverse of a matrix
## to avoid costly computation.


## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = numeric()) {
	z <- NULL
	set <- function(y) {
		x <<- y
		z <<- NULL
	}
	get <- function() x
	setInverse <- function(w) z <<- w
	getInverse <- function() z
	list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
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
